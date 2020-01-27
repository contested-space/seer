-module(seer).

-export([counter_inc/1, counter_inc/2, gauge_set/2, read/2, read_all/0]).

-spec counter_inc(binary()) -> ok.
counter_inc(Counter) -> counter_inc(Counter, 1).

-spec counter_inc(binary(), integer()) -> ok.
counter_inc(Counter, Val) ->
    {ok, Ref} = get_or_new(counter, Counter),
    atomics:add(Ref, 1, Val).

-spec gauge_set(binary(), integer()) -> ok.
gauge_set(Gauge, Val) ->
    {ok, Ref} = get_or_new(gauge, Gauge),
    atomics:put(Ref, 1, Val).

read(Type, Name) ->
    {ok, Ref} = get(Type, Name),
    {ok, read_by_ref(Type, Ref)}.

read_by_ref(counter, Ref) -> atomics:exchange(Ref, 1, 0);
read_by_ref(gauge, Ref) -> atomics:get(Ref, 1).

read_all() ->
    [
        {Type, Name, read_by_ref(Type, Ref)}
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
    Ref.
