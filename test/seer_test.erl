-module(seer_test).

-include_lib("eunit/include/eunit.hrl").

counter_inc_test() ->
    ok = seer:counter_inc(<<"bob_the_counter">>),
    ok = seer:counter_inc(<<"bob_the_counter">>, 10),
    ?assertEqual({ok, 11}, seer:read(counter, <<"bob_the_counter">>)),
    ?assertEqual({ok, 0}, seer:read(counter, <<"bob_the_counter">>)),
    true = persistent_term:erase({seer, <<"bob_the_counter">>}).

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
