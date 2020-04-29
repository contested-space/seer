-module(seer_utils).

-include("seer.hrl").

-export([format_carbon_line/4]).

-spec format_carbon_line(binary(), binary(), read_metric(), erlang:timestamp()) -> carbon_string() |
                                                                                   carbon_batch().
format_carbon_line(Prefix, Host, {Type, Name, Value}, {Mega, Sec, _}) ->
    TimeInSeconds = Mega * 1000000 + Sec,
    MetricKey = metric_key(Prefix, Host, Name),
    TimestampBin = integer_to_binary(TimeInSeconds),
    case Value of
      empty ->
          [];
      _ ->
          case Type of
            counter ->
                format_carbon_line(<<MetricKey/binary, ".counter">>, integer_to_binary(Value), TimestampBin);
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

% private
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

