-module(seer_server).

-include("seer.hrl").

-define(SERVER, ?MODULE).

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
-export([start_link/0, stop/0]).

-record(state, {mode :: undefined | stdout}).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:cast(?SERVER, stop).

init(_) ->
    Mode = ?ENV(?ENV_MODE, ?DEFAULT_MODE),
    Interval = ?ENV(?ENV_INTERVAL, ?DEFAULT_INTERVAL),
    case Mode of
        stdout ->
            timer:send_interval(Interval, poll_stdout),
            {ok, #state{mode = Mode}, 0}
    end.

handle_call(_, _From, State) -> {reply, {error, undefined_call}, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_, State) -> {noreply, State}.

handle_info(poll_stdout, State) ->
    Metrics = seer:read_all(),
    io:format("~w~n", [Metrics]),
    {noreply, State};
handle_info(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
