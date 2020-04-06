-module(seer).

-include("seer.hrl").

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

-spec counter_inc(metric_name()) -> ok.
counter_inc(Counter) ->
    counter_inc(Counter, 1).

-spec counter_inc(metric_name(), integer()) -> ok.
counter_inc(Counter, Val) ->
    {ok, Ref} = get_or_new(counter, Counter),
    atomics:add(Ref, 1, Val).

-spec dist_record(metric_name(), integer()) -> ok.
dist_record(Dist, Val) ->
    {ok, Ref} = get_or_new(dist, Dist),
    N = atomics:add_get(Ref, ?DIST_RESERVOIR_SIZE + 1, 1),
    dist_record2(Ref, Val, N).

-spec dist_timing(metric_name(), erlang:timestamp()) -> ok.
dist_timing(Timing, TimeStamp) ->
    Delta = timer:now_diff(os:timestamp(), TimeStamp),
    dist_record(Timing, Delta).

-spec gauge_set(metric_name(), integer()) -> ok.
gauge_set(Gauge, Val) ->
    {ok, Ref} = get_or_new(gauge, Gauge),
    atomics:put(Ref, 1, Val).

-spec histo_record(metric_name(), integer()) -> ok.
histo_record(Histo, Val) ->
    {ok, Ref} = get_or_new(histo, Histo),
    histo_record2(Ref, Val).

-spec histo_timing(metric_name(), erlang:timestamp()) -> ok.
histo_timing(Histo, TimeStamp) ->
    Delta = timer:now_diff(os:timestamp(), TimeStamp),
    histo_record(Histo, Delta).

-spec read(metric_type(), metric_name()) -> {ok, read_value()}.
read(Type, Name) ->
    {ok, Ref} = get(Type, Name),
    {ok, read_metric(Type, Name, Ref)}.

-spec read_all() -> [{metric_type(), metric_name(), read_value()}].
read_all() ->
    [{Type, Name, read_metric(Type, Name, Ref)} || {{?MODULE, Name}, {Type, Ref}} <- persistent_term:get()].

% private
-spec dist_record2(atomics:atomics_ref(), integer(), integer()) -> ok.
dist_record2(Ref, Val, N) when N =< ?DIST_RESERVOIR_SIZE ->
    atomics:put(Ref, N, Val);
dist_record2(Ref, Val, N) when N >= 1 andalso N =< 4294967295 ->
    case granderl:uniform(N) of
      X when X < ?DIST_RESERVOIR_SIZE ->
          atomics:put(Ref, X, Val);
      _ ->
          ok
    end.

-spec get_or_new(metric_type(), metric_name()) -> {ok, atomics:atomics_ref()} | {error, term()}.
get_or_new(Type, Name) ->
    case get(Type, Name) of
      {ok, Ref} ->
          {ok, Ref};
      {error, undefined} ->
          {ok, new(Type, Name)};
      {error, _} = E ->
          E
    end.

-spec get(metric_type(), metric_name()) -> {ok, atomics:atomics_ref()} | {error, term()}.
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

-spec histo_percentiles(non_neg_integer(),
                        [histo_bucket()],
                        [histo_percentile_index()]) -> #{histo_percentile() => histo_bucket_key()} | #{}.
histo_percentiles(N, Buckets, PercentileIdxes) ->
    histo_percentiles(N, Buckets, PercentileIdxes, #{}).

% TODO: find out why using #{histo_percentile() => histo_bucket_key()} | #{}
%       instead of map() makes gradualizer unhappy
-spec histo_percentiles(integer(),
                        [histo_bucket()],
                        [histo_percentile_index()],
                        map()) -> #{histo_percentile() => histo_bucket_key()} | #{}.
histo_percentiles(_N, _Buckets, [], Res) ->
    Res;
histo_percentiles(N,
                  Bs = [{BucketKey, BucketSamples} | BsRest],
                  Ps = [{PercentileKey, PercentileIdx} | PsRest],
                  Res) ->
    HiIdx = N,
    LoIdx = N - BucketSamples,
    case PercentileIdx >= LoIdx andalso PercentileIdx =< HiIdx of
      true ->
          Res2 = Res#{PercentileKey => BucketKey},
          histo_percentiles(N, Bs, PsRest, Res2);
      false ->
          histo_percentiles(LoIdx, BsRest, Ps, Res)
    end.

-spec histo_record2(atomics:atomics_ref(), integer()) -> ok.
histo_record2(Ref, Val) ->
    Idx = ceil(math:log2(Val)) + 1,
    atomics:add(Ref, Idx, 1).

-spec new(metric_type(), metric_name()) -> atomics:atomics_ref().
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

-spec nths(dist_stats_indexes(), dist_samples()) -> dist_stats().
nths(Idxs, Lst) ->
    nths(Idxs, 1, Lst, []).

-spec nths(dist_stats_indexes(), integer(), [integer()], dist_stats()) -> dist_stats().
nths([], _N, _Lst, Res) ->
    Res;
nths([Idx | IdxRest], Idx, [H | _] = Lst, Res) ->
    nths(IdxRest, Idx, Lst, [H | Res]);
nths(Idxs = [Idx | _], Idx2, [_ | T] = _Lst, Res) when Idx > Idx2 ->
    nths(Idxs, Idx2 + 1, T, Res).

-spec read_metric(metric_type(), metric_name(), atomics:atomics_ref()) -> read_value().
read_metric(counter, _Name, Ref) ->
    atomics:exchange(Ref, 1, 0);
read_metric(gauge, _Name, Ref) ->
    atomics:get(Ref, 1);
read_metric(dist, Name, Ref) ->
    new(dist, Name),
    case atomics:get(Ref, ?DIST_RESERVOIR_SIZE + 1) of
      0 ->
          empty;
      N ->
          Samples = [atomics:get(Ref, Idx) || Idx <- lists:seq(1, min(N, ?DIST_RESERVOIR_SIZE))],
          SortedSamples = lists:sort(Samples),
          [Max, P99, P90, P50, Min] = case SortedSamples of
                                        [] ->
                                            [0, 0, 0, 0, 0];
                                        _ ->
                                            nths(summary_stats_idxs(N, ?DIST_RESERVOIR_SIZE), SortedSamples)
                                      end,
          #{n_samples => N, min => Min, max => Max, p50 => P50, p90 => P90, p99 => P99}
    end;
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
    case N of
      0 ->
          empty;
      _ ->
          % PercentileIdxes must be in descending order
          PercentileIdxes = [{p99, max(1, floor(0.99 * N))},
                             {p90, max(1, floor(0.9 * N))},
                             {p50, max(1, floor(0.5 * N))}],
          Percentiles = histo_percentiles(N, Buckets, PercentileIdxes),
          Map = maps:from_list(Buckets),
          Map#{percentiles => Percentiles, n_samples => N}
    end.

-spec summary_stats_idxs(integer(), integer()) -> dist_stats().
summary_stats_idxs(NumSamples, Size) when NumSamples < Size ->
    summary_stats_idxs(NumSamples, NumSamples);
summary_stats_idxs(_Samples, Size) ->
    [1, max(1, floor(0.5 * Size)), max(1, floor(0.9 * Size)), max(1, floor(0.99 * Size)), Size].

