-module(seer_server).

-include("seer.hrl").

-behaviour(gen_server).

-export(
    [
        init/1,
        handle_cast/2,
        handle_call/3,
        handle_info/2,
        terminate/2,
        code_change/3
    ]
).
-export([start_link/0]).

-record(
    state,
    {
        mode :: undefined | stdout,
        prefix :: undefined | binary(),
        host :: undefined | binary(),
        tcp_socket :: undefined | gen_tcp:socket(),
        interval :: undefined | non_neg_integer()
    }
).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    InitialState =
        #state{
            mode = ?ENV(?ENV_MODE, ?DEFAULT_MODE),
            prefix = ?ENV(?ENV_PREFIX, ?DEFAULT_PREFIX),
            host = ?ENV(?ENV_HOST, ?DEFAULT_HOST),
            interval = ?ENV(?ENV_INTERVAL, ?DEFAULT_INTERVAL)
        },
    self() ! setup,
    {ok, InitialState, 0}.

handle_call(_, _From, State) -> {reply, {error, undefined_call}, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_, State) -> {noreply, State}.

handle_info(setup, #state{mode = Mode, interval = Interval} = State) ->
    {Msg, SetupState} =
        case Mode of
            stdout -> {poll_stdout, State};
            carbon ->
                {ok, Socket} = get_carbon_socket(),
                {poll_carbon, State#state{tcp_socket = Socket}}
        end,
    timer:send_after(Interval, Msg),
    {noreply, SetupState};
handle_info(poll_stdout, #state{interval = Interval} = State) ->
    Metrics = seer:read_all(),
    io:format("~w~n", [Metrics]),
    timer:send_after(Interval, poll_stdout),
    {noreply, State};
handle_info(
    poll_carbon,
    #state{
            prefix = Prefix,
            host = Host,
            tcp_socket = Socket,
            interval = Interval
        }
        =
        State
) ->
    Metrics = seer:read_all(),
    CarbonStrings = seer_utils:carbon_format(Prefix, Host, Metrics),
    [gen_tcp:send(Socket, Metric) || Metric <- CarbonStrings],
    timer:send_after(Interval, poll_carbon),
    % TODO: put metrics in buffer and loop for connection
    {noreply, State};
handle_info(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% private
get_carbon_socket() ->
    case
    gen_tcp:connect(
        ?ENV(?ENV_CARBON_HOST, ?DEFAULT_CARBON_HOST),
        ?ENV(?ENV_CARBON_PORT, ?DEFAULT_CARBON_PORT),
        [binary]
    ) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} ->
            io:format("~s~n", [Reason]),
            timer:sleep(500),
            get_carbon_socket()
    end.
