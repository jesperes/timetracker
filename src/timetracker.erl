-module(timetracker).

%% API exports
-export([main/1]).

-record(state,
        { start = inactive :: calendar:datetime() | inactive
        , idle = 300 :: integer() %% seconds
        , intervals = []
        , last_save = 0 :: integer()
        }).

%% Save state every 10s
-define(SAVE_INTERVAL, 10).
-define(SAVEFILE, "/tmp/timetracker.eterm").

%% escript Entry point
main(Args) ->
  try
    do_main(Args)
  catch _:T:St ->
      logger:error("Error: ~p", [{T, St}])
  after
    logger_std_h:filesync(default),
    erlang:halt(0)
  end.

do_main(_Args) ->
  ok = logger:update_primary_config(#{level => info}),
  ok = logger:update_formatter_config(
         default, #{single_line => true, legacy_header => false}),
  start_monitors().

load_state() ->
  case file:consult(?SAVEFILE) of
    {ok, [State]} when is_record(State, state) ->
      print_state(State),
      State;
    Error ->
      logger:error("No savefile ~p, starting from scratch: ~0p", [?SAVEFILE, Error]),
      #state{}
  end.

save_state(#state{last_save = LastSave} = State) ->
  Now = erlang:system_time(second),
  case Now - LastSave of
    Age when Age > ?SAVE_INTERVAL ->
      logger:debug("Saving state: ~0p", [State]),
      ok = file:write_file(?SAVEFILE, io_lib:format("~p.", [State])),
      State#state{last_save = Now};
    _ -> State
  end.

print_state(#state{intervals = Intervals} = _State) ->
  io:format("--------------------------------------------------~n", []),
  io:format("Current worked periods:~n", []),
  {Today, _} = erlang:localtime(),
  io:format("Today's date: ~w~n", [Today]),
  io:format("Intervals:~n", []),
  lists:foreach(
    fun({Start, {Date, _} = End}) when Date =:= Today ->
        {_, StartTime} = Start,
        {_, EndTime} = End,
        SecondsStart = calendar:datetime_to_gregorian_seconds(Start),
        SecondsEnd = calendar:datetime_to_gregorian_seconds(End),
        Duration = SecondsEnd - SecondsStart,
        Hours = Duration div 3600,
        Minutes = Duration div 60,
        Seconds = Duration rem 60,
        io:format("~w -> ~w (duration ~wh ~wm ~ws)~n",
                  [StartTime, EndTime, Hours, Minutes, Seconds]);
       (_) -> ok
    end, Intervals),
  TotalTimeToday =
    lists:foldl(fun({Start, {Date, _} = End}, Acc) when Date =:= Today ->
                    SecondsStart = calendar:datetime_to_gregorian_seconds(Start),
                    SecondsEnd = calendar:datetime_to_gregorian_seconds(End),
                    Acc + (SecondsEnd - SecondsStart);
                   (_, Acc) ->
                    Acc
                end, 0, Intervals),
  Hours = TotalTimeToday div 3600,
  Minutes = TotalTimeToday div 60,
  Seconds = TotalTimeToday rem 60,
  io:format("Total time worked today: ~wh ~wm ~ws~n", [Hours, Minutes, Seconds]),
  io:format("--------------------------------------------------~n", []).

start_monitors() ->
  monitor_mouse(),
  monitor_keyboard(),
  State = load_state(),
  loop(State).

monitor_mouse() ->
  Cmd = io_lib:format("xinput test 14", []),
  open_port({spawn, Cmd}, [exit_status, stderr_to_stdout, {line, 1024}]).

monitor_keyboard() ->
  Cmd = io_lib:format("xinput test 11", []),
  open_port({spawn, Cmd}, [exit_status, stderr_to_stdout, {line, 1024}]).

loop(#state{ idle = Idle
           , start = Start
           , intervals = Intervals
           } = State) ->
  State0 = save_state(State),
  After = erlang:convert_time_unit(Idle, second, millisecond),
  receive
    {_, {data, _}} when Start =:= inactive ->
      NewStart = erlang:localtime(),
      logger:info("Activity detected, starting new period"),
      loop(State0#state{start = NewStart});
    {_, {data, _}} ->
      %% Detected activity, keep waiting
      loop(State0)
  after
    After ->
      case Start of
        inactive ->
          loop(State0);
        _ ->
          %% Idle: close current period and start a new one
          End = erlang:localtime(),
          logger:info("Idle threshold reached, closing current period."),
          Diff = calendar:time_difference(Start, End),
          logger:info("Worked: ~p -> ~p (~p)", [Start, End, Diff]),
          %% Flush all pending message
          flush(),
          State1 = State0#state{ start = inactive
                               , intervals = [{Start, End}|Intervals]},
          print_state(State1),
          loop(State1)
      end
  end.

flush() ->
  receive
    _ -> flush()
  after 0 ->
      ok
  end.
