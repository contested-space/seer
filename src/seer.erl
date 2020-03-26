-module(seer).

-define(DIST_RESERVOIR_SIZE, 200).
-define(HISTO_NUM_BUCKETS, 64).

-export([counter_inc/1,
         counter_inc/2,
         dist_record/2,
         dist_timing/2,
         gauge_set/2,
         histo_record/2,
         histo_timing/2,
         read/2,
         read_all/0]).

-spec counter_inc(binary()) -> ok.
counter_inc(Counter) ->
    counter_inc(Counter, 1).

-spec counter_inc(binary(), integer()) -> ok.
counter_inc(Counter, Val) ->
    {ok, Ref} = get_or_new(counter, Counter),
    atomics:add(Ref, 1, Val).

-spec dist_record(binary(), integer()) -> ok.
dist_record(Dist, Val) ->
    {ok, Ref} = get_or_new(dist, Dist),
    N = atomics:add_get(Ref, ?DIST_RESERVOIR_SIZE + 1, 1),
    dist_record2(Ref, Val, N).

-spec dist_timing(binary(), erlang:timestamp()) -> ok.
dist_timing(Timing, TimeStamp) ->
    Delta = timer:now_diff(os:timestamp(), TimeStamp),
    dist_record(Timing, Delta).

-spec gauge_set(binary(), integer()) -> ok.
gauge_set(Gauge, Val) ->
    {ok, Ref} = get_or_new(gauge, Gauge),
    atomics:put(Ref, 1, Val).

-spec histo_record(binary(), integer()) -> ok.
histo_record(Histo, Val) ->
    {ok, Ref} = get_or_new(histo, Histo),
    histo_record2(Ref, Val).

-spec histo_timing(binary(), erlang:timestamp()) -> ok.
histo_timing(Histo, TimeStamp) ->
    Delta = timer:now_diff(os:timestamp(), TimeStamp),
    histo_record(Histo, Delta).

read(Type, Name) ->
    {ok, Ref} = get(Type, Name),
    {ok, read_metric(Type, Name, Ref)}.

read_all() ->
    [{Type, Name, read_metric(Type, Name, Ref)} || {{?MODULE, Name}, {Type, Ref}} <- persistent_term:get()].

% private
dist_record2(Ref, Val, N) when N =< ?DIST_RESERVOIR_SIZE ->
    atomics:put(Ref, N, Val);
dist_record2(Ref, Val, N) ->
    case granderl:uniform(N) of
      X when X < ?DIST_RESERVOIR_SIZE ->
          atomics:put(Ref, X, Val);
      _ ->
          ok
    end.

get_or_new(Type, Name) ->
    case get(Type, Name) of
      {ok, Ref} ->
          {ok, Ref};
      {error, undefined} ->
          {ok, new(Type, Name)};
      {error, _} = E ->
          E
    end.

get(Type, Name) when is_binary(Name) ->
    case persistent_term:get({?MODULE, Name}, {error, undefined}) of
      {Type, Ref} ->
          {ok, Ref};
      {error, undefined} = E ->
          E;
      {WrongType, _Ref} ->
          {error, {exists_as, WrongType}}
    end;
get(_Type, _Name) ->
    {error, metric_name_not_a_binary}.

histo_percentiles(N, Buckets, PercentileIdxes) ->
    histo_percentiles(N, Buckets, PercentileIdxes, #{}).

histo_percentiles(_N, _Buckets, [], Res) ->
    Res;
histo_percentiles(N, Bs = [{BucketKey, BucketSamples} | _], Ps = [{PercentileKey, PercentileIdx} | _], Res) ->
    HiIdx = N,
    LoIdx = N - BucketSamples,
    case PercentileIdx >= LoIdx andalso PercentileIdx =< HiIdx of
      true ->
          Res2 = Res#{PercentileKey => BucketKey},
          histo_percentiles(N, Bs, tl(Ps), Res2);
      false ->
          histo_percentiles(LoIdx, tl(Bs), Ps, Res)
    end.

histo_record2(Ref, Val) ->
    Idx = ceil(math:log2(Val)) + 1,
    atomics:add(Ref, Idx, 1).

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
    Ref;
new(histo, Name) ->
    Ref = atomics:new(?HISTO_NUM_BUCKETS, [{signed, false}]),
    persistent_term:put({?MODULE, Name}, {histo, Ref}),
    Ref.

nths(Idxs, Lst) ->
    nths(Idxs, 1, Lst, []).

nths([], _N, _Lst, Res) ->
    Res;
nths([Idx | IdxRest], Idx, Lst, Res) ->
    nths(IdxRest, Idx, Lst, [hd(Lst) | Res]);
nths(Idxs = [Idx | _], Idx2, Lst, Res) when Idx > Idx2 ->
    nths(Idxs, Idx2 + 1, tl(Lst), Res).

read_metric(counter, _Name, Ref) ->
    atomics:exchange(Ref, 1, 0);
read_metric(gauge, _Name, Ref) ->
    atomics:get(Ref, 1);
read_metric(dist, Name, Ref) ->
    new(dist, Name),
    N = atomics:get(Ref, ?DIST_RESERVOIR_SIZE + 1),
    Samples = [atomics:get(Ref, Idx) || Idx <- lists:seq(1, min(N, ?DIST_RESERVOIR_SIZE))],
    SortedSamples = lists:sort(Samples),
    [Max, P99, P90, P50, Min] = case SortedSamples of
                                  [] ->
                                      [0, 0, 0, 0, 0];
                                  _ ->
                                      nths(summary_stats_idxs(N, ?DIST_RESERVOIR_SIZE), SortedSamples)
                                end,
    #{n_samples => N, min => Min, max => Max, p50 => P50, p90 => P90, p99 => P99};
read_metric(histo, Name, Ref) ->
    new(histo, Name),
    % Buckets must be in descending order
    {Buckets, N} = lists:foldl(fun (Idx, {Bs, Sum}) ->
                                       case atomics:get(Ref, Idx) of
                                         0 ->
                                             {Bs, Sum};
                                         Samples ->
                                             Key = {trunc(math:pow(2, Idx - 2)) + 1, trunc(math:pow(2, Idx - 1))},
                                             {[{Key, Samples} | Bs], Sum + Samples}
                                       end
                               end,
                               {[], 0},
                               lists:seq(1, ?HISTO_NUM_BUCKETS)),
    % PercentileIdxes must be in descending order
    PercentileIdxes = [{p99, max(1, floor(0.99 * N))},
                       {p90, max(1, floor(0.9 * N))},
                       {p50, max(1, floor(0.5 * N))}],
    Percentiles = histo_percentiles(N, Buckets, PercentileIdxes),
    Map = maps:from_list(Buckets),
    Map#{percentiles => Percentiles, n_samples => N}.

summary_stats_idxs(NumSamples, Size) when NumSamples < Size ->
    summary_stats_idxs(NumSamples, NumSamples);
summary_stats_idxs(_Samples, Size) ->
    [1, max(1, floor(0.5 * Size)), max(1, floor(0.9 * Size)), max(1, floor(0.99 * Size)), Size].

