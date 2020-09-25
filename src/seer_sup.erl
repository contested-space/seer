-module(seer_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
    Poller = #{
        id => seer_server,
        start => {seer_server, start_link, []},
        shutdown => 2000,
        restart => permanent,
        type => worker,
        modules => [seer_server]
    },
    ChildSpecs = [Poller],
    {ok, {SupFlags, ChildSpecs}}.
