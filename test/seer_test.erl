-module(seer_test).

-include_lib("eunit/include/eunit.hrl").

counter_inc_test() ->
    ok = seer:counter_inc(<<"bob_the_counter">>),
    ok = seer:counter_inc(<<"bob_the_counter">>, 10),
    ?assertEqual({ok, 11}, seer:read(counter, <<"bob_the_counter">>)),
    ?assertEqual({ok, 0}, seer:read(counter, <<"bob_the_counter">>)),
    true = persistent_term:erase({seer, <<"bob_the_counter">>}).

dist_record_test() ->
    [seer:dist_record(<<"bob_the_dist">>, N) || N <- lists:seq(1, 10)],
    Expected1 =
        #{n_samples => 10, min => 1, max => 10, p50 => 5, p90 => 9, p99 => 9},
    ?assertEqual({ok, Expected1}, seer:read(dist, <<"bob_the_dist">>)),
    [seer:dist_record(<<"bob_the_dist">>, N) || N <- lists:seq(1, 200)],
    Expected2 =
        #{
            n_samples => 200,
            min => 1,
            max => 200,
            p50 => 100,
            p90 => 180,
            p99 => 198
        },
    ?assertEqual({ok, Expected2}, seer:read(dist, <<"bob_the_dist">>)),
    true = persistent_term:erase({seer, <<"bob_the_dist">>}).

gauge_set_test() ->
    ok = seer:gauge_set(<<"bob_the_gauge">>, 5),
    ?assertEqual({ok, 5}, seer:read(gauge, <<"bob_the_gauge">>)),
    ?assertEqual({ok, 5}, seer:read(gauge, <<"bob_the_gauge">>)),
    ok = seer:gauge_set(<<"bob_the_gauge">>, 6),
    ok = seer:gauge_set(<<"bob_the_gauge">>, 5),
    ?assertEqual({ok, 5}, seer:read(gauge, <<"bob_the_gauge">>)),
    true = persistent_term:erase({seer, <<"bob_the_gauge">>}).

read_all_test() ->
    ok = seer:counter_inc(<<"bob_the_counter">>, 10),
    ok = seer:gauge_set(<<"bob_the_gauge">>, 5),
    Map =
        lists:foldl(
            fun ({Type, Name, Val}, Acc) -> Acc#{{Type, Name} => Val} end,
            #{},
            seer:read_all()
        ),
    ?assertMatch(#{{counter, <<"bob_the_counter">>} := 10}, Map),
    ?assertMatch(#{{gauge, <<"bob_the_gauge">>} := 5}, Map),
    Map2 =
        lists:foldl(
            fun ({Type, Name, Val}, Acc) -> Acc#{{Type, Name} => Val} end,
            #{},
            seer:read_all()
        ),
    ?assertMatch(#{{counter, <<"bob_the_counter">>} := 0}, Map2),
    ?assertMatch(#{{gauge, <<"bob_the_gauge">>} := 5}, Map2).
