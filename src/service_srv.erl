-module(service_srv).
-author('komm@siphost.su').

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_APP, nconfig).
-record(app_state,{
          name :: atom() | list(),
          state :: 'on' | 'off',
          mode = auto :: 'auto' | 'manual'
       }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, start_service/1, stop_service/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([behaviour_info/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link( HandleServices :: list()) -> {ok, Pid :: pid()}.
start_link(HandleServices) ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [HandleServices], [])
.

-spec behaviour_info(callbacks) -> list().
behaviour_info(callbacks) ->
  [{start,1},
   {stop,1},
   {restart,1}
  ].

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_service(ServiceName)->
  gen_server:call({global, ?MODULE}, {start_service, ServiceName})
.

stop_service(ServiceName)->
  gen_server:call({global, ?MODULE}, {stop_service, ServiceName})
.

init([HandleServices])->
  {ok, HandleServices}
.

handle_call(all, _From, HandleServices) ->
  {reply, HandleServices, HandleServices};
handle_call({start_service, ServiceName}, _From, HandleServices) ->
  case [ X || X <- HandleServices, is_record(X, app_state), X#app_state.name =:= ServiceName] of
    [] -> application:load(ServiceName),
          config_srv:apply(ServiceName),
	  application:start(ServiceName)
    ;
    [App | _] -> App:start(config_srv:get_config(App))
  end,
  {reply, ok, HandleServices}
;
handle_call({stop_service, ServiceName}, _From, HandleServices) ->
  [ X:stop([]) || X <- HandleServices, is_record(X, app_state), X#app_state.name =:= ServiceName],
  application:stop(ServiceName),
  application:unload(ServiceName),
  {reply, ok, HandleServices}
.
handle_cast(_Msg, HandleServices) ->
  {noreply, HandleServices}.

handle_info(_Info, HandleServices) ->
  {noreply, HandleServices}.

terminate(_Reason, _HandleServices) ->
  ok.

code_change(_OldVsn, HandleServices, _Extra) ->
  {ok, HandleServices}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

