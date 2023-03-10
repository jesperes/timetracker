-module(timetracker_db).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, register_activity_period/1, get_worked_time/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(worked_time, worked_time).
-define(unit, unit).

%% Unit is given by timetracker_server:get_timeunit()
-spec register_activity_period(Length :: integer()) -> ok.
register_activity_period(Length) ->
  gen_server:call(?SERVER, {reg_period, Length}).

-spec get_worked_time() -> integer().
get_worked_time() ->
  gen_server:call(?SERVER, get_worked_time).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, read_state()}.

handle_call(get_worked_time, _From, State) ->
  {reply, maps:get(?worked_time, State, 0), State};
handle_call({reg_period, Length}, _From, State) ->
  ?LOG_INFO("Registering new activity period, ~p ~ss long",
            [Length, timetracker_server:get_timeunit()]),
  NewState = maps:update_with(?worked_time, fun(Old) -> Length + Old end, Length, State),
  NewWorkedTime = maps:get(?worked_time, State, 0),
  {reply, NewWorkedTime, sync_state(NewState)}.

handle_cast(Msg, State) ->
  ?LOG_ERROR("Unknown cast: ~p", [Msg]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%% Internals
default_state() ->
  #{?worked_time => 0, ?unit => timetracker_server:get_timeunit()}.

state_filename() ->
  {{Y, M, D}, _} = calendar:local_time(),
  Today =
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0w", [Y, M, D])),
  {ok, SaveDir} = application:get_env(timetracker, save_dir),
  ok = filelib:ensure_path(SaveDir),
  filename:join(SaveDir, Today ++ ".eterm").

sync_state(State) ->
  case filelib:is_file(state_filename()) of
    true ->
      %% The state file already exists, update it with new info
      write_state(State);
    false ->
      %% The state file does not exist (e.g. current date has changed,
      %% so we should write to new file). Start with empty state.
      write_state(default_state())
  end.

read_state() ->
  case file:consult(state_filename()) of
    {ok, [Map]} ->
      ?LOG_INFO("Time worked today: ~s",
                [tt_notify:fmt(
                   maps:get(?worked_time, Map, 0))]),
      Map;
    {error, enoent} ->
      ?LOG_INFO("No state file found for today, starting new one.", []),
      default_state();
    Error ->
      ?LOG_ERROR("Failed to read state, starting new one: ~p", [Error]),
      default_state()
  end.

write_state(Map) ->
  File = state_filename(),
  ok = file:write_file(File, io_lib:format("~p.", [Map])),
  Map.
