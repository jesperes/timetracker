%% All API use erlang:system_time() (unit = native) to measure time
%% start/duration. Conversions to/from DateTime etc are done only when
%% needed (i.e. when formatting dates/durations) or when storing in
%% the DETS table.

-module(timetracker_db).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, get_worked_time/1, get_worked_time/0, register_workunit/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, workunit).

-type system_time() :: integer().

%% Record stored in the DETS table
-record(workunit,
        {start :: calendar:datetime() | {calendar:date(), '_'},
         duration :: calendar:time() | '_'}).
-record(state, {dbref :: reference()}).

-spec register_workunit(Start :: system_time(), Duration :: system_time()) -> ok.
register_workunit(Start, Duration) ->
  gen_server:cast(?SERVER, {register_workunit, Start, Duration}).

-spec get_worked_time() -> tt_utils:system_time_native().
get_worked_time() ->
  {Date, _} =
    calendar:now_to_local_time(
      erlang:timestamp()),
  get_worked_time(Date).

%% Return amount of worked time for the current date.
-spec get_worked_time(Date :: calendare:date()) -> tt_utils:system_time_native().
get_worked_time(Date) ->
  gen_server:call(?SERVER, {get_worked_time, Date}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  Filename = application:get_env(timetracker, db_filename, "timetracker.dets"),
  {ok, Ref} = dets:open_file(?TABLE, [{file, Filename}, {keypos, 2}, {type, set}]),
  ?LOG_INFO(#{label => "Opened table", info => dets:info(?TABLE)}),

  {Date, _} =
    calendar:now_to_local_time(
      erlang:timestamp()),
  WorkedTimeToday = do_get_worked_time(Date),
  ?LOG_INFO(#{label => "Worked time today", time => tt_utils:format_time(WorkedTimeToday)}),
  {ok, #state{dbref = Ref}}.

handle_call({get_worked_time, Date}, _From, State) ->
  WorkedTime = do_get_worked_time(Date),
  {reply, WorkedTime, State}.

handle_cast({register_workunit, Start, Duration}, State) ->
  StartDateTime = calendar:system_time_to_local_time(Start, native),
  {_, DurationTime} =
    calendar:seconds_to_daystime(
      erlang:convert_time_unit(Duration, native, second)),
  ?LOG_INFO(#{label => "Inserting work unit into table",
              start => StartDateTime,
              duration => DurationTime}),

  ok = dets:insert(?TABLE, #workunit{start = StartDateTime, duration = DurationTime}),
  {noreply, State};
handle_cast(Msg, State) ->
  ?LOG_ERROR("Unknown cast: ~p", [Msg]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  dets:close(?TABLE),
  ok.

%% Internals

do_get_worked_time(Date) ->
  Workunits =
    dets:select(?TABLE, [{#workunit{start = {Date, '_'}, duration = '_'}, [], ['$_']}]),
  Secs =
    lists:foldl(fun(#workunit{duration = {H, M, S}}, Acc) -> H * 3600 + M * 60 + S + Acc end,
                0,
                Workunits),
  erlang:convert_time_unit(Secs, second, native).
