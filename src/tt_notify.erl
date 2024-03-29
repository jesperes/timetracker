-module(tt_notify).

-export([maybe_notify_daily_limit/1, fmt/1]).

%% -include_lib("kernel/include/logger.hrl").

maybe_notify_daily_limit(_Duration) ->
  ok.

fmt(_) ->
  "".

%%   {ok, V} = application:get_env(timetracker, workday_length_secs),
%%   DailyLimit = erlang:convert_time_unit(V, second, timetracker_server:get_timeunit()),
%%   maybe_notify_daily_limit(Duration, DailyLimit).

%% secs(Seconds) ->
%%   erlang:convert_time_unit(Seconds, second, timetracker_server:get_timeunit()).

%% maybe_notify_daily_limit(Duration, DailyLimit) ->
%%   {ok, MinNotifySecs} = application:get_env(timetracker, min_notification_interval_secs),
%%   LastNotify = application:get_env(timetracker, last_notify, 0),

%%   Now = timetracker_server:timestamp_now(),
%%   ShouldNotify = Now - LastNotify > secs(MinNotifySecs),
%%   ShouldLog = Now - LastNotify > secs(60),
%%   HasExceededLimit = Duration > DailyLimit,

%%   if ShouldNotify andalso HasExceededLimit ->
%%        application:set_env(timetracker, last_notify, Now),
%%        Msg =
%%          io_lib:format("You have worked ~s today.~n"
%%                        "This is ~s more than your daily limit.",
%%                        [fmt(Duration), fmt(Duration - DailyLimit)]),
%%        ?LOG_WARNING(Msg),
%%        display_dialog(Msg);
%%      ShouldLog andalso not HasExceededLimit ->
%%        application:set_env(timetracker, last_notify, Now),
%%        ?LOG_DEBUG("You have worked ~s today. You have ~s left before "
%%                   "you have reached your limit, which is ~s.",
%%                   [fmt(Duration), fmt(DailyLimit - Duration), fmt(DailyLimit)]);
%%      true ->
%%        %% ?LOG_DEBUG("Time since last notification: ~p", [Now - LastNotify]),
%%        ok
%%   end.

%% fmt(Time) ->
%%   fmt(Time, timetracker_server:get_timeunit()).

%% fmt(Time, Unit) when Time >= 0 ->
%%   Seconds = erlang:convert_time_unit(Time, Unit, second),
%%   Hours = Seconds div 3600,
%%   S0 = Seconds - Hours * 3600,
%%   Minutes = S0 div 60,
%%   S1 = S0 - Minutes * 60,
%%   lists:flatten(
%%     io_lib:format("~2..0w:~2..0w:~2..0w", [Hours, Minutes, S1])).

%% display_dialog(Msg) ->
%%   spawn(fun() ->
%%            {ok, Fmt} = application:get_env(timetracker, display_dialog_fmt),
%%            Cmd = io_lib:format(Fmt, [Msg]),
%%            os:cmd(Cmd)
%%         end).
