-module(timetracker_db).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, register_activity_period/1, get_worked_secs/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-spec register_activity_period(Length :: integer()) -> ok.
register_activity_period(Length) ->
  gen_server:cast(?SERVER, {reg_period, Length}),
  ok.

get_worked_secs() ->
  gen_server:call(?SERVER, get_worked_secs).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, read_state()}.

handle_call(get_worked_secs, _From, State) ->
  {reply, maps:get(worked_secs, State, 0), State}.

handle_cast({reg_period, Length}, State) ->
  ?LOG_INFO("Registering new activity period, ~p seconds long", [Length]),
  NewState = maps:update_with(worked_secs, fun(Old) -> Length + Old end, Length, State),
  {noreply, sync_state(NewState)};
handle_cast(Msg, State) ->
  ?LOG_ERROR("Unknown cast: ~p", [Msg]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%% Internals
default_state() ->
  #{worked_secs => 0}.

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
      ?LOG_INFO("Worked seconds today: ~p", [maps:get(worked_secs, Map, 0)]),
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
