-module(timetracker_server).

-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2, event/1,
         get_timeunit/0, timestamp_now/0]).

-type event() :: term().

%% Don't use exactly 5000 to avoid rounding issues when polling the
%% events.
-define(INTERVAL, 4711).
-define(CHECK_ACTIVITY, check_activity).
%% All time calculations is done using this time unit. The only
%% conversions to/from other units are when displaying formatted time
%% and when reading configuration values from app envs.
-define(TIME_UNIT, millisecond).

-record(state,
        {last_activity :: integer() | undefined,
         activity_period_start :: integer() | undefined,
         tref :: timer:tref(),
         is_working :: boolean(),
         inactivity_threshold :: integer()}).

%% API

-spec event(Event :: event()) -> ok.
event(Event) ->
  %% ?LOG_DEBUG("Event detected: ~p", [Event]),
  gen_server:cast(?MODULE, {event, Event}).

get_timeunit() ->
  ?TIME_UNIT.

%% gen_server callbacks

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  process_flag(trap_exit, true),
  {ok, TRef} = timer:apply_interval(?INTERVAL, gen_server, cast, [self(), ?CHECK_ACTIVITY]),
  {ok,
   #state{tref = TRef,
          activity_period_start = undefined,
          last_activity = undefined,
          is_working = false,
          inactivity_threshold =
            erlang:convert_time_unit(get_app_env(inactivity_threshold_secs), second, ?TIME_UNIT)}}.

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast({event, _Event}, #state{is_working = true} = State) ->
  %% We are currently working, just update the "last_activity" timestamp
  {noreply, State#state{last_activity = timestamp_now()}};
handle_cast({event, Event},
            #state{is_working = false, last_activity = LastActivity} = State) ->
  %% We were idle, but have started working again
  Now = timestamp_now(),
  if LastActivity =:= undefined ->
       ?LOG_INFO("Activity detected on ~p, starting work timer.", [Event]);
     true ->
       IdleTime = Now - LastActivity,
       ?LOG_INFO("Activity detected on ~p after being idle for ~p ~ss (~s), starting work timer.",
                 [Event, IdleTime, ?TIME_UNIT, tt_notify:fmt(IdleTime)])
  end,

  {noreply,
   State#state{is_working = true,
               activity_period_start = Now,
               last_activity = Now}};
handle_cast(?CHECK_ACTIVITY, #state{is_working = false} = State) ->
  %% Don't do anything until is_working is set to true again
  {noreply, State};
handle_cast(?CHECK_ACTIVITY,
            #state{last_activity = LastActivity,
                   activity_period_start = Start,
                   inactivity_threshold = Threshold} =
              State) ->
  Now = timestamp_now(),
  Age = Now - LastActivity,
  Duration = LastActivity - Start,

  {ok, V} = application:get_env(timetracker, workday_length_secs),
  DailyLimit = erlang:convert_time_unit(V, second, timetracker_server:get_timeunit()),
  TimeToDailyLimit = DailyLimit - timetracker_db:get_worked_time() - (Now - Start),

  if TimeToDailyLimit > 0 ->
       ?LOG_DEBUG("last_activity=~p current_period=~p time_until_inactive=~p time_to_daily_limit=~s unit=~p",
                  [Age, Duration, Threshold - Age, tt_notify:fmt(TimeToDailyLimit), ?TIME_UNIT]);
     true ->
       ?LOG_WARNING("[PAST DAILY LIMIT] last_activity=~p current_period=~p time_until_inactive=~p time_past_daily_limit=~s unit=~p",
                    [Age, Duration, Threshold - Age, tt_notify:fmt(-TimeToDailyLimit), ?TIME_UNIT])
  end,

  if Age > Threshold ->
       ?LOG_INFO("Inactivity threshold exceeded (last activity ~p ~ss ago); stopping work timer. Length of activity period ~p ~ss.",
                 [Age, ?TIME_UNIT, Duration, ?TIME_UNIT]),
       WorkedTime = timetracker_db:register_activity_period(Duration),
       tt_notify:maybe_notify_daily_limit(WorkedTime),
       NewState = State#state{activity_period_start = undefined, is_working = false},
       {noreply, NewState};
     true ->
       WorkedTime = timetracker_db:get_worked_time(),
       CurrentActivityPeriod = Duration,
       tt_notify:maybe_notify_daily_limit(WorkedTime + CurrentActivityPeriod),
       {noreply, State}
  end.

terminate(_How,
          #state{tref = TRef,
                 is_working = IsWorking,
                 last_activity = LastActivity,
                 activity_period_start = Start}) ->
  if IsWorking ->
       Duration = LastActivity - Start,
       timetracker_db:register_activity_period(Duration);
     true ->
       ok
  end,
  timer:cancel(TRef),
  ?LOG_NOTICE("Terminated timetracker server", []),
  ok.

timestamp_now() ->
  erlang:system_time(?TIME_UNIT).

get_app_env(Key) ->
  {ok, Value} = application:get_env(timetracker, Key),
  Value.
