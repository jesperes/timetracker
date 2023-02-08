-module(tt_video_source).

-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         check_video/0]).

-record(state, {tref :: timer:tref()}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init(_Args) ->
  process_flag(trap_exit, true),
  {ok, Interval} = application:get_env(timetracker, activity_check_interval),
  {ok, TRef} = timer:apply_interval(Interval, ?MODULE, check_video, []),
  {ok, #state{tref = TRef}}.

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_How, #state{tref = TRef}) ->
  timer:cancel(TRef).

%% Internals
check_video() ->
  Output = os:cmd("lsmod | grep ^uvcvideo"),
  [_, _, CountStr] = string:lexemes(Output, " \n"),
  Count = list_to_integer(CountStr),
  if Count > 0 ->
       timetracker_server:event({?MODULE, Count});
     true ->
       ok
  end.
