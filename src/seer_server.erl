-module(seer_server).

-include("seer.hrl").

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-record(state, {
    mode :: undefined | stdout | carbon | carbon_offline | carbon_reconnected,
    prefix :: undefined | binary(),
    host :: undefined | binary(),
    tcp_socket :: undefined | gen_tcp:socket(),
    poll_interval :: undefined | non_neg_integer(),
    carbon_buffer = [] :: [carbon_batch()],
    buffer_size = 0 :: non_neg_integer(),
    max_buffer_size :: non_neg_integer()
}).

-type state() :: #state{}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, Host} = inet:gethostname(),
    BinHost =
        case unicode:characters_to_binary(Host, latin1) of
            Name when is_binary(Name) ->
                Name
        end,
    Interval = ?ENV(?ENV_POLL_INTERVAL, ?DEFAULT_POLL_INTERVAL) * 1000,
    InitialState = #state{
        mode = ?ENV(?ENV_MODE, ?DEFAULT_MODE),
        prefix = ?ENV(?ENV_PREFIX, ?DEFAULT_PREFIX),
        host = BinHost,
        poll_interval = Interval,
        max_buffer_size = ?ENV(?ENV_MAX_BUFFER_SIZE, ?DEFAULT_MAX_BUFFER_SIZE)
    },
    self() ! setup,
    {ok, InitialState}.

handle_call(_, _From, State) ->
    {reply, {error, undefined_call}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(setup, #state{mode = Mode, poll_interval = Interval} = State) ->
    {Msg, SetupState} =
        case Mode of
            stdout ->
                {poll_stdout, State};
            carbon ->
                carbon_setup(State)
        end,
    timer:send_after(Interval, Msg),
    {noreply, SetupState};
handle_info(poll_stdout, #state{poll_interval = Interval} = State) ->
    Metrics = seer:read_all(),
    io:format("~w~n", [Metrics]),
    timer:send_after(Interval, poll_stdout),
    {noreply, State};
handle_info(
    poll_carbon,
    #state{prefix = Prefix, host = Host, tcp_socket = Socket, poll_interval = Interval} = State
) ->
    Metrics = seer:read_all(),
    CarbonStrings = seer_utils:carbon_format(Prefix, Host, Metrics),
    case carbon_send_batch(Socket, CarbonStrings) of
        ok ->
            timer:send_after(Interval, poll_carbon),
            {noreply, State};
        {error, UnsentStrings} ->
            timer:send_after(Interval, poll_carbon_offline),
            timer:send_after(?TCP_RECONNECTION_INTERVAL, carbon_reconnect),
            {noreply, State#state{mode = carbon_offline, carbon_buffer = [UnsentStrings]}}
    end;
handle_info(poll_carbon_offline, #state{mode = carbon_reconnected} = State) ->
    self() ! poll_carbon,
    {noreply, State#state{mode = carbon}};
handle_info(
    poll_carbon_offline,
    #state{
        prefix = Prefix,
        host = Host,
        poll_interval = Interval,
        carbon_buffer = Buffer,
        buffer_size = BufferSize,
        max_buffer_size = MaxBufferSize
    } =
        State
) ->
    Metrics = seer:read_all(),
    CarbonStrings = seer_utils:carbon_format(Prefix, Host, Metrics),
    {NewBuffer, NewBufferSize} =
        case BufferSize < MaxBufferSize of
            true ->
                {[CarbonStrings | Buffer], BufferSize + 1};
            false ->
                {[CarbonStrings | lists:droplast(Buffer)], BufferSize}
        end,
    timer:send_after(Interval, poll_carbon_offline),
    {noreply, State#state{carbon_buffer = NewBuffer, buffer_size = NewBufferSize}};
handle_info(carbon_reconnect, #state{carbon_buffer = Buffer} = State) ->
    case carbon_connect() of
        {ok, Socket} ->
            case carbon_send_buffer(Socket, Buffer) of
                ok ->
                    {noreply, State#state{
                        mode = carbon_reconnected,
                        tcp_socket = Socket,
                        carbon_buffer = [],
                        buffer_size = 0
                    }};
                {error, NewBuffer} ->
                    timer:send_after(?TCP_RECONNECTION_INTERVAL, carbon_reconnect),
                    {noreply, State#state{
                        carbon_buffer = NewBuffer,
                        buffer_size = length(NewBuffer)
                    }}
            end;
        {error, _} ->
            timer:send_after(?TCP_RECONNECTION_INTERVAL, carbon_reconnect),
            {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private
-spec carbon_setup(state()) -> {atom(), state()}.
carbon_setup(State) ->
    case carbon_connect() of
        {ok, Socket} ->
            {poll_carbon, State#state{tcp_socket = Socket}};
        {error, _} ->
            timer:send_after(?TCP_RECONNECTION_INTERVAL, carbon_reconnect),
            {poll_carbon_offline, State#state{mode = carbon_offline}}
    end.

-spec carbon_connect() -> {ok, gen_tcp:socket()} | {error, term()}.
carbon_connect() ->
    gen_tcp:connect(
        ?ENV(?ENV_CARBON_HOST, ?DEFAULT_CARBON_HOST),
        ?ENV(?ENV_CARBON_PORT, ?DEFAULT_CARBON_PORT),
        [binary]
    ).

-spec carbon_send_buffer(gen_tcp:socket(), [carbon_batch()]) -> ok | {error, [[binary()]]}.
carbon_send_buffer(_Socket, []) ->
    ok;
carbon_send_buffer(Socket, [Batch | Buffer]) ->
    case carbon_send_batch(Socket, Batch) of
        ok ->
            carbon_send_buffer(Socket, Buffer);
        {error, UnsentStrings} ->
            {error, [UnsentStrings | Buffer]}
    end.

-spec carbon_send_batch(gen_tcp:socket(), carbon_batch()) -> ok | {error, [binary()]}.
carbon_send_batch(Socket, MetricStrings) ->
    carbon_send_batch(Socket, MetricStrings, []).

-spec carbon_send_batch(gen_tcp:socket(), carbon_batch(), carbon_batch()) ->
    ok | {error, carbon_batch()}.
carbon_send_batch(_Socket, [], UnsentStrings) ->
    case UnsentStrings of
        [] ->
            ok;
        _ ->
            {error, UnsentStrings}
    end;
carbon_send_batch(Socket, [Metric | MetricStrings], UnsentStrings) ->
    case gen_tcp:send(Socket, Metric) of
        ok ->
            carbon_send_batch(Socket, MetricStrings, UnsentStrings);
        {error, _} ->
            carbon_send_batch(Socket, MetricStrings, [Metric | UnsentStrings])
    end.
