-module(seer_utils).

-include("seer.hrl").

-export([carbon_format/1]).

carbon_format(Metrics) ->
    Timestamp = erlang:timestamp(),
    lists:flatten([carbon_string(Metric, Timestamp) || Metric <- Metrics]).

% private
carbon_string({Type, Name, Value}, Timestamp) ->
    MetricKey = metric_key(Name),
    TimestampBin = unix_time_binary(Timestamp),
    case Type of
        counter ->
            carbon_string(MetricKey, integer_to_binary(Value), TimestampBin);
        gauge ->
            carbon_string(MetricKey, integer_to_binary(Value), TimestampBin);
        dist ->
            [
                carbon_string(
                    <<
                        MetricKey/binary,
                        ".",
                        (atom_to_binary(Key, latin1)) / binary
                    >>,
                    integer_to_binary(Val),
                    TimestampBin
                )
                || {Key, Val} <- maps:to_list(Value)
            ];
        histo ->
            #{n_samples := NSamples, percentiles := Percentiles} = Value,
            [
                carbon_string(
                    <<MetricKey/binary, ".n_samples">>,
                    integer_to_binary(NSamples),
                    TimestampBin
                )
                |
                [
                    carbon_string(
                        <<MetricKey/binary, ".", Key/binary>>,
                        Val,
                        TimestampBin
                    )
                    || {Key, Val} <- make_key_values(Percentiles)
                ]
            ]
    end.

make_key_values(Percentiles) ->
    lists:map(fun make_key_val/1, maps:to_list(Percentiles)).

make_key_val({Key, {_Min, Max}}) ->
    {atom_to_binary(Key, latin1), integer_to_binary(Max)}.

carbon_string(MetricKey, Value, Timestamp) ->
    <<MetricKey/binary, " ", Value/binary, " ", Timestamp/binary, "\n">>.

metric_key(Name) ->
    <<
        (?ENV(?ENV_PREFIX, ?DEFAULT_PREFIX)) / binary,
        ".",
        (?ENV(?ENV_HOST, ?DEFAULT_HOST)) / binary,
        ".",
        Name/binary
    >>.

unix_time_binary({MegaSecs, Secs, _MicroSecs}) ->
    integer_to_binary(1000000 * MegaSecs + Secs).
