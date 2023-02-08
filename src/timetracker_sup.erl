-module(timetracker_sup).

-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

worker(Module) ->
  #{id => Module,
    start => {Module, start_link, []},
    type => worker}.

init(_Args) ->
  {ok, {#{}, [worker(timetracker_db), worker(timetracker_server)] ++ activity_sources()}}.

activity_sources() ->
  lists:map(fun({Module, Type, InitArgs}) ->
               #{id => {Module, Type},
                 start => {Module, start_link, InitArgs},
                 type => worker}
            end,
            application:get_env(timetracker, sources, [])).
