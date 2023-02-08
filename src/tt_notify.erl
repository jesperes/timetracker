-module(tt_notify).

-export([maybe_notify_daily_limit/1]).

-include_lib("kernel/include/logger.hrl").

maybe_notify_daily_limit(Seconds) ->
  {ok, DailyLimit} = application:get_env(timetracker, workday_length),
  maybe_notify_daily_limit(Seconds, DailyLimit).

maybe_notify_daily_limit(Seconds, DailyLimit) ->
  LastNotify = application:get_env(timetracker, last_notify, 0),
  Now = erlang:system_time(second),
  application:set_env(timetracker, last_notify, Now),

  if Now - LastNotify > 300 ->
       if Seconds > DailyLimit ->
            Msg =
              io_lib:format("You have worked ~s today.\n"
                            "This is ~s more than your limit, which is ~s.",
                            [fmt_seconds(Seconds),
                             fmt_seconds(Seconds - DailyLimit),
                             fmt_seconds(DailyLimit)]),
            ?LOG_WARNING(Msg),
            display_dialog(Msg);
          true ->
            ?LOG_INFO("You have worked ~s today. You have ~s left before you have reached your limit, which is ~s.",
                      [fmt_seconds(Seconds),
                       fmt_seconds(DailyLimit - Seconds),
                       fmt_seconds(DailyLimit)])
       end;
     true ->
       ok
  end.

fmt_seconds(Seconds) ->
  Hours = Seconds div 3600,
  S0 = Seconds - Hours * 3600,
  Minutes = S0 div 60,
  S1 = S0 - Minutes * 60,
  io_lib:format("~2..0w:~2..0w:~2..0w", [Hours, Minutes, S1]).

display_dialog(Msg) ->
  spawn(fun() ->
           Cmd =
             lists:flatten(
               io_lib:format("xmessage -center -timeout 30 '~s'", [Msg])),
           os:cmd(Cmd)
        end).
