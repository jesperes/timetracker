-module(timetracker_server).

-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2, event/1]).

-type event() :: term().

-define(INTERVAL, 5000).
-define(CHECK_ACTIVITY, check_activity).
-define(TIME_UNIT, second).

-record(state,
        {last_activity :: integer(),
         activity_period_start :: integer() | undefined,
         tref :: timer:tref(),
         is_working :: boolean(),
         inactivity_threshold :: integer()}).

%% API

-spec event(Event :: event()) -> ok.
event(Event) ->
  ?LOG_DEBUG("Event detected: ~p", [Event]),
  gen_server:cast(?MODULE, {event, Event}).

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
          inactivity_threshold = get_app_env(inactivity_threshold)}}.

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast({event, _Event}, #state{is_working = true} = State) ->
  %% We are currently working, just update the "last_activity" timestamp
  {noreply, State#state{last_activity = timestamp_now()}};
handle_cast({event, Event}, #state{is_working = false} = State) ->
  %% We were idle, but have started working again
  ?LOG_INFO("Activity detected (~p) after being idle, starting work timer.", [Event]),
  Now = timestamp_now(),
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

  ?LOG_DEBUG("Checking activity. Last activity ~p ~ss ago.", [Age, ?TIME_UNIT]),
  if Age > Threshold ->
       ?LOG_INFO("Inactivity threshold exceeded; stopping work timer. Length of activity period ~p ~ss.",
                 [Duration, ?TIME_UNIT]),
       timetracker_db:register_activity_period(Duration),
       NewState = State#state{activity_period_start = undefined, is_working = false},
       maybe_notify_daily_limit(NewState),
       {noreply, NewState};
     Duration > 60 ->
       ?LOG_INFO("Activity period max length exceeded, starting new one", []),
       timetracker_db:register_activity_period(Duration),
       NewState =
         State#state{activity_period_start = Now,
                     last_activity = Now,
                     is_working = true},
       maybe_notify_daily_limit(NewState),
       {noreply, NewState};
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

maybe_notify_daily_limit(#state{activity_period_start = Start}) ->
  WorkedSecs = timetracker_db:get_worked_secs(),
  Duration = timestamp_now() - Start,
  tt_notify:maybe_notify_daily_limit(WorkedSecs + Duration).
