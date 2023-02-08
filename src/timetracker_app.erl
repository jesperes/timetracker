-module(timetracker_app).

-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  timetracker_sup:start_link().

stop(_State) ->
  ok.
