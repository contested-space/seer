-module(seer_utils).

-include("seer.hrl").

-export([carbon_format/3]).

-spec carbon_format(binary(), binary(), [read_metric()]) -> carbon_batch().
carbon_format(Prefix, Host, Metrics) ->
    Timestamp = erlang:system_time(second),
    lists:flatten([format_carbon_line(Prefix, Host, Metric, Timestamp) || Metric <- Metrics]).

% private
-spec format_carbon_line(binary(), binary(), read_metric(), integer()) -> carbon_string() | carbon_batch().
format_carbon_line(Prefix, Host, {Type, Name, Value}, Timestamp) ->
    MetricKey = metric_key(Prefix, Host, Name),
    TimestampBin = integer_to_binary(Timestamp),
    case Value of
      empty ->
          [];
      _ ->
          case Type of
            counter ->
                counter_carbon_line(<<MetricKey/binary, ".counter">>, Value, TimestampBin);
            gauge ->
                format_carbon_line(<<MetricKey/binary, ".gauge">>, integer_to_binary(Value), TimestampBin);
            dist ->
                dist_carbon_line(<<MetricKey/binary, ".dist">>, Value, TimestampBin);
            dist_timing ->
                dist_carbon_line(<<MetricKey/binary, ".dist_timing">>, Value, TimestampBin);
            histo ->
                histo_carbon_line(<<MetricKey/binary, ".histo">>, Value, TimestampBin);
            histo_timing ->
                histo_carbon_line(<<MetricKey/binary, ".histo_timing">>, Value, TimestampBin)
          end
    end.

-spec counter_carbon_line(binary(), integer(), binary()) -> [carbon_string()].
counter_carbon_line(MetricKey, Value, TsBin) ->
    NbSec = ?ENV(?ENV_POLL_INTERVAL, ?DEFAULT_POLL_INTERVAL),
    Rate = Value div NbSec,
    [format_carbon_line(MetricKey, integer_to_binary(Value), TsBin),
     format_carbon_line(<<MetricKey/binary, ".rate">>, integer_to_binary(Rate), TsBin)].

-spec dist_carbon_line(binary(), map(), binary()) -> [carbon_string()].
dist_carbon_line(MetricKey, Value, TsBin) ->
    [format_carbon_line(<<MetricKey/binary, ".", (atom_to_binary(Key, latin1))/binary>>,
                        integer_to_binary(Val),
                        TsBin)
     || {Key, Val} <- maps:to_list(Value)].

-spec histo_carbon_line(binary(), map(), binary()) -> [carbon_string()].
histo_carbon_line(MetricKey, Value, TsBin) ->
    #{n_samples := NSamples, percentiles := Percentiles} = Value,
    [format_carbon_line(<<MetricKey/binary, ".n_samples">>, integer_to_binary(NSamples), TsBin)
     | [format_carbon_line(<<MetricKey/binary, ".", Key/binary>>, Val, TsBin)
        || {Key, Val} <- make_key_values(Percentiles)]].

-spec format_carbon_line(binary(), binary(), binary()) -> carbon_string().
format_carbon_line(MetricKey, Value, Timestamp) ->
    <<MetricKey/binary, " ", Value/binary, " ", Timestamp/binary, "\n">>.

-spec make_key_values(#{histo_percentile() => histo_bucket_key()} | #{}) -> [{binary(), binary()}].
make_key_values(Percentiles) ->
    lists:map(fun make_key_val/1, maps:to_list(Percentiles)).

-spec make_key_val({histo_percentile(), histo_bucket_key()}) -> {binary(), binary()}.
make_key_val({Key, {_Min, Max}}) ->
    {atom_to_binary(Key, latin1), integer_to_binary(Max)}.

-spec metric_key(binary(), binary(), metric_name()) -> binary().
metric_key(Prefix, Host, Name) ->
    <<Prefix/binary, ".", Host/binary, ".", Name/binary>>.

