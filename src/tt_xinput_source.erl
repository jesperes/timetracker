-module(tt_xinput_source).

-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(REG_ACTIVITY, reg_activity).

-record(state,
        {active = false :: boolean(), tref :: timer:tref(), port :: port(), device :: integer()}).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Device) when is_integer(Device) ->
  process_flag(trap_exit, true),
  Port = open_xinput_monitoring_port(Device),

  %% Do not trigger events for every mouse/keyboard event registered
  %% by xinput, instead check at a given interval if there has been
  %% any activity.
  IntervalMs =
    erlang:convert_time_unit(
      tt_utils:get_time_env(activity_check_interval, {1, second}), native, millisecond),
  {ok, TRef} = timer:apply_interval(IntervalMs, gen_server, cast, [self(), ?REG_ACTIVITY]),
  {ok,
   #state{port = Port,
          tref = TRef,
          device = Device}}.

handle_call(Msg, _From, State) ->
  ?LOG_ERROR("Undefined message: ~p", [Msg]),
  {noreply, State}.

handle_cast(?REG_ACTIVITY, #state{active = true, device = Device} = State) ->
  timetracker_server:event({?MODULE, Device}),
  {noreply, State#state{active = false}};
handle_cast(?REG_ACTIVITY, #state{active = false} = State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  %% Some activity was detected
  {noreply, State#state{active = true}}.

terminate(_How, #state{tref = TRef, port = Port}) ->
  timer:cancel(TRef),
  port_close(Port).

%% Internals
open_xinput_monitoring_port(XInputDevice) ->
  Wrapper =
    filename:join(
      code:priv_dir(timetracker), "xinput.sh"),
  Cmd = io_lib:format("~s test ~w", [Wrapper, XInputDevice]),
  open_port({spawn, Cmd}, [exit_status, stderr_to_stdout, {line, 1024}]).
