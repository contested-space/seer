-module(seer).

-define(DIST_RESERVOIR_SIZE, 200).

-export(
    [
        counter_inc/1,
        counter_inc/2,
        dist_record/2,
        gauge_set/2,
        read/2,
        read_all/0
    ]
).

-spec counter_inc(binary()) -> ok.
counter_inc(Counter) -> counter_inc(Counter, 1).

-spec counter_inc(binary(), integer()) -> ok.
counter_inc(Counter, Val) ->
    {ok, Ref} = get_or_new(counter, Counter),
    atomics:add(Ref, 1, Val).

-spec dist_record(binary(), integer()) -> ok.
dist_record(Dist, Val) ->
    {ok, Ref} = get_or_new(dist, Dist),
    N = atomics:add_get(Ref, ?DIST_RESERVOIR_SIZE + 1, 1),
    dist_record2(Ref, Val, N).

dist_record2(Ref, Val, N) when N =< ?DIST_RESERVOIR_SIZE ->
    atomics:put(Ref, N, Val);
dist_record2(Ref, Val, N) ->
    case granderl:uniform(N) of
        X when X < ?DIST_RESERVOIR_SIZE -> atomics:put(Ref, X, Val);
        _ -> ok
    end.

-spec gauge_set(binary(), integer()) -> ok.
gauge_set(Gauge, Val) ->
    {ok, Ref} = get_or_new(gauge, Gauge),
    atomics:put(Ref, 1, Val).

read(Type, Name) ->
    {ok, Ref} = get(Type, Name),
    {ok, read_metric(Type, Name, Ref)}.

read_metric(counter, _Name, Ref) -> atomics:exchange(Ref, 1, 0);
read_metric(gauge, _Name, Ref) -> atomics:get(Ref, 1);
read_metric(dist, Name, Ref) ->
    new(dist, Name),
    N = atomics:get(Ref, ?DIST_RESERVOIR_SIZE + 1),
    Samples =
        [
            atomics:get(Ref, Idx)
            || Idx <- lists:seq(1, min(N, ?DIST_RESERVOIR_SIZE))
        ],
    SortedSamples = lists:sort(Samples),
    [Max, P99, P90, P50, Min] =
        case SortedSamples of
            [] -> [0, 0, 0, 0, 0];
            _ ->
                nths(summary_stats_idxs(N, ?DIST_RESERVOIR_SIZE), SortedSamples)
        end,
    #{
        n_samples => N,
        min => Min,
        max => Max,
        p50 => P50,
        p90 => P90,
        p99 => P99
    }.

read_all() ->
    [
        {Type, Name, read_metric(Type, Name, Ref)}
        || {{?MODULE, Name}, {Type, Ref}} <- persistent_term:get()
    ].

% private
get_or_new(Type, Name) ->
    case get(Type, Name) of
        {ok, Ref} -> {ok, Ref};
        {error, undefined} -> {ok, new(Type, Name)};
        {error, _} = E -> E
    end.

get(Type, Name) when is_binary(Name) ->
    case persistent_term:get({?MODULE, Name}, {error, undefined}) of
        {Type, Ref} -> {ok, Ref};
        {error, undefined} = E -> E;
        {WrongType, _Ref} -> {error, {exists_as, WrongType}}
    end;
get(_Type, _Name) -> {error, metric_name_not_a_binary}.

new(counter, Name) ->
    Ref = atomics:new(1, [{signed, false}]),
    persistent_term:put({?MODULE, Name}, {counter, Ref}),
    Ref;
new(gauge, Name) ->
    Ref = atomics:new(1, [{signed, true}]),
    persistent_term:put({?MODULE, Name}, {gauge, Ref}),
    Ref;
% We add one to the reservoir size to have a counter for the number of samples
new(dist, Name) ->
    Ref = atomics:new(?DIST_RESERVOIR_SIZE + 1, [{signed, true}]),
    persistent_term:put({?MODULE, Name}, {dist, Ref}),
    Ref.

%% nths([], _N, _Lst, Res) -> Res;
%% nths([Idx | IdxRest], Idx, [Elt | LstRest], Res) ->
%%     nths(IdxRest, Idx + 1, LstRest, [Elt | Res]);
%% nths(Idxs, Idx, Lst, Res) -> nths(Idxs, Idx + 1, tl(Lst), Res).

nths(Idxs, Lst) -> nths(Idxs, 1, Lst, []).

nths([], _N, _Lst, Res) -> Res;
nths([Idx | IdxRest], Idx, Lst, Res) ->
    nths(IdxRest, Idx, Lst, [hd(Lst) | Res]);
nths(Idxs = [Idx | _], Idx2, Lst, Res) when Idx > Idx2 ->
    nths(Idxs, Idx2 + 1, tl(Lst), Res).

summary_stats_idxs(NumSamples, Size) when NumSamples < Size ->
    summary_stats_idxs(NumSamples, NumSamples);
summary_stats_idxs(_Samples, Size) ->
    [
        1,
        max(1, floor(0.5 * Size)),
        max(1, floor(0.9 * Size)),
        max(1, floor(0.99 * Size)),
        Size
    ].
