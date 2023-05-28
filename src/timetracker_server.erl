-module(timetracker_server).

-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2, event/1]).

-type event() :: term().

-define(CHECK_ACTIVITY, check_activity).

-record(state,
        {last_activity :: tt_utils:system_time_native() | undefined,
         activity_period_start :: tt_utils:system_time_native() | undefined,
         tref :: timer:tref(),
         is_working :: boolean(),
         inactivity_threshold :: tt_utils:system_time_native()}).

%% API

-spec event(Event :: event()) -> ok.
event(Event) ->
  gen_server:cast(?MODULE, {event, Event}).

%% gen_server callbacks

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  process_flag(trap_exit, true),
  {ok, TRef} = timer:apply_interval(1000, gen_server, cast, [self(), ?CHECK_ACTIVITY]),
  {ok,
   #state{tref = TRef,
          activity_period_start = undefined,
          last_activity = undefined,
          is_working = false,
          inactivity_threshold = tt_utils:get_time_env(inactivity_threshold, {180, second})}}.

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast({event, _Event}, #state{is_working = true} = State) ->
  %% An activity event has been registered. We are currently working,
  %% so just update the "last_activity" timestamp
  {noreply, State#state{last_activity = erlang:system_time()}};
handle_cast({event, Event},
            #state{is_working = false, last_activity = LastActivity} = State) ->
  %% We were idle, but have started working again
  Now = erlang:system_time(),
  if LastActivity =:= undefined ->
       ?LOG_INFO(#{label => "Activity detected", event => Event});
     true ->
       IdleTime = Now - LastActivity,
       ?LOG_INFO(#{label => "Activity detected",
                   event => Event,
                   idle_time => tt_utils:format_time(IdleTime)})
  end,

  {noreply,
   State#state{is_working = true,
               activity_period_start = Now,
               last_activity = Now}};
handle_cast(?CHECK_ACTIVITY, #state{is_working = false} = State) ->
  %% We are currently not working. Don't do anything until is_working
  %% is set to true again
  {noreply, State};
handle_cast(?CHECK_ACTIVITY,
            #state{last_activity = LastActivity,
                   activity_period_start = Start,
                   inactivity_threshold = Threshold} =
              State) ->
  %% We are currently set to "working", but we might have been
  %% inactive for long enough to exceed the inactivity threshold.
  Now = erlang:system_time(),
  TimeSinceLastActivity = Now - LastActivity,
  Duration = LastActivity - Start,

  DailyLimit = tt_utils:get_time_env(workday_length, {8 * 60 * 60, second}),
  TimeToDailyLimit = DailyLimit - timetracker_db:get_worked_time() - (Now - Start),

  if TimeToDailyLimit > 0 ->
       ?LOG_DEBUG(#{last_activity => tt_utils:format_time(LastActivity),
                    duration_of_current_period => tt_utils:format_time(Duration),
                    time_to_daily_limit => tt_utils:format_time(TimeToDailyLimit)}),
       ok;
     true ->
       ?LOG_WARNING(#{label => "Past daily limit",
                      last_activity => tt_utils:format_time(LastActivity),
                      duration_of_current_period => tt_utils:format_time(Duration),
                      time_to_daily_limit => tt_utils:format_time(TimeToDailyLimit)})
  end,

  TimeToInactivity = Threshold - TimeSinceLastActivity,
  InactivityNoticeThreshold = erlang:convert_time_unit(5, second, native),
  if TimeSinceLastActivity > Threshold ->
       ?LOG_INFO(#{label => "Inactivity threshold exceeded",
                   time_since_last_activity => tt_utils:format_time(TimeSinceLastActivity)}),
       NewState = State#state{activity_period_start = undefined, is_working = false},
       ok = timetracker_db:register_workunit(Start, Duration),
       {noreply, NewState};
     TimeToInactivity < InactivityNoticeThreshold ->
       ?LOG_INFO(#{label => "Near inactivity threshold",
                   duration_of_current_period => tt_utils:format_time(Duration),
                   time_to_inactivity => tt_utils:format_time(TimeToInactivity)}),
       {noreply, State};
     true ->
       {noreply, State}
  end.

terminate(_How,
          #state{tref = TRef,
                 is_working = IsWorking,
                 last_activity = LastActivity,
                 activity_period_start = Start}) ->
  if IsWorking ->
       Duration = LastActivity - Start,
       timetracker_db:register_workunit(Start, Duration);
     true ->
       ok
  end,
  timer:cancel(TRef),
  ?LOG_NOTICE("Terminated timetracker server", []),
  ok.
