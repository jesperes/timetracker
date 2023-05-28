-module(tt_utils).

-export([get_time_env/1, get_time_env/2, format_time/1, format_date/1]).

-type system_time_native() :: integer().

-export_type([system_time_native/0]).

-spec get_time_env(Key :: atom(),
                   {Default :: system_time_native(), Unit :: erlang:time_unit()}) ->
                    system_time_native().
get_time_env(Key, {Default, Unit}) ->
  case get_time_env(Key) of
    undefined ->
      erlang:convert_time_unit(Default, Unit, native);
    Time ->
      Time
  end.

-spec get_time_env(Key :: atom()) -> system_time_native() | undefined.
get_time_env(Key) ->
  case application:get_env(timetracker, Key) of
    undefined ->
      undefined;
    {ok, {Time, Unit}} ->
      erlang:convert_time_unit(Time, Unit, native)
  end.

-spec format_time(Duration :: system_time_native()) -> string().
format_time(Duration) ->
  {_, {H, M, S}} =
    calendar:seconds_to_daystime(
      erlang:convert_time_unit(Duration, native, second)),
  io_lib:format("~2..0w:~2..0w:~2..0w", [H, M, S]).

format_date({Y, M, D}) ->
  io_lib:format("~4..0w:~2..0w:~2..0w", [Y, M, D]).
