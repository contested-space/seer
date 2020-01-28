-module(seer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    seer_sup:start_link(),
    Poller =
        #{
            id => seer_server,
            start => {seer_server, start_link, []},
            shutdown => 2000,
            restart => permanent,
            type => worker,
            modules => [seer_server]
        },
    supervisor:start_child(seer_sup, Poller).

stop(_State) ->
    ok.
