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

-export([start_link/1, start_service/2, stop_service/1, restart_service/2]).

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
  [{start,   1},
   {stop,    1},
   {restart, 1},
   {status,  0}
  ].

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec start_service(ServiceName :: atom(), StartMode :: 'auto' | 'manual') -> ok | error.
start_service(?DEFAULT_APP, _)->
  application:start(?DEFAULT_APP)
;
start_service(ServiceName, StartMode)->
  gen_server:call({global, ?MODULE}, {start_service, ServiceName, StartMode})
.

-spec stop_service(ServiceName :: atom()) -> ok.
stop_service(ServiceName)->
  gen_server:call({global, ?MODULE}, {stop_service, ServiceName})
.

-spec restart_service(ServiceName :: atom(), StartMode :: 'auto' | 'manual') -> ok | error.
restart_service(ServiceName, RestartMode)->
  gen_server:call({global, ?MODULE}, {restart_service, ServiceName, RestartMode})
.

init([HandleServices])->
  %%TODO: autostart HandleServices when mode = 'auto'
  {ok, HandleServices}
.

handle_call(all, _From, HandleServices) ->
  {reply, HandleServices, HandleServices};
handle_call({start_service, ServiceName, StartMode}, _From, HandleServices) ->
  case [ X || X <- HandleServices, is_record(X, app_state), X#app_state.name =:= ServiceName] of
    [] -> application:load(ServiceName),
          config_srv:apply(ServiceName),
	  application:start(ServiceName)
    ;
    [App | _] -> 
          case App:start([]) of
            ok-> {reply, ok };
            _-> 
              NewHandleServices =
              lists:map(
                fun(X) when is_record(X, app_state), X#app_state.name == ServiceName ->
                     X#app_state{mode = StartMode, state = on};
                   (X) -> X
                end,
                HandleServices 
              ),
              {reply, ok, NewHandleServices}
          end
  end
  
;
handle_call({stop_service, ServiceName}, _From, HandleServices) ->
  application:stop(ServiceName),
  application:unload(ServiceName),
  NewHandleServices = 
  lists:map(
    fun(X) when X#app_state.name == ServiceName -> X#app_state{state = off};
       (X) -> X
    end,
    HandleServices
  ),
  {reply, ok, NewHandleServices}
;
handle_call({restart_service, ServiceName, _RestartMode}, _From, HandleServices) ->
  case [ X || X <- HandleServices, is_record(X, app_state), X#app_state.name =:= ServiceName] of
    [] -> application:stop(ServiceName),
          config_srv:apply(ServiceName),
	  application:start(ServiceName)
    ;
    [App | _] -> App:restart([])
  end,
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

