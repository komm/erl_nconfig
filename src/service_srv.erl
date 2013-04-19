-module(service_srv).
-author('komm@siphost.su').

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_APP, nconfig).
-record(app_state,{
          module :: atom(),
          name :: atom() | list(),
          state :: 'on' | 'off',
          mode = auto :: 'auto' | 'manual'
       }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_service/2, stop_service/2, restart_service/2, status_services/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([behaviour_info/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], [])
.

-spec behaviour_info(callbacks) -> list().
behaviour_info(callbacks) ->
  [{start,   1},
   {stop,    1},
   {restart, 1},
   {status,  0},
   {register,1}
  ].


-spec start_service(ServiceName :: atom(), StartMode :: 'auto' | 'manual') -> ok | error.
start_service(?DEFAULT_APP, _)->
  application:start(?DEFAULT_APP)
;
start_service(ServiceName, StartMode)->
  gen_server:call({global, ?MODULE}, {start_service, ServiceName, StartMode})
.

-spec stop_service(ServiceName :: atom(), StopMode :: 'auto' | 'manual') -> ok.
stop_service(ServiceName, StopMode)->
  gen_server:call({global, ?MODULE}, {stop_service, ServiceName, StopMode})
.

-spec restart_service(ServiceName :: atom(), StartMode :: 'auto' | 'manual') -> ok | error.
restart_service(ServiceName, RestartMode)->
  gen_server:call({global, ?MODULE}, {restart_service, ServiceName, RestartMode})
.

-spec status_services()->list().
status_services()->
  gen_server:call({global, ?MODULE}, status_all)
.
name(Handle, Params)->
	%%is application?
	case application:load(Handle) of
	ok -> 
		config_srv:apply(Handle), 
		#app_state{module = application, name = Handle, mode = auto, state = off };
	{error,{"no such file or directory",_}} ->
  	  ModuleName = list_to_atom("handle_" ++ atom_to_list(Handle)),
	  case code:which(ModuleName) of
	    non_existing -> 
               #app_state{module= ModuleName, name = '', state = error, mode = manual};
	    _ ->
	       case ModuleName:register(Params) of
                 error -> [];
                 {'EXIT', _}-> [];
                 {ok, Name} -> #app_state{ module = ModuleName, name = Name, state = off, mode = auto}
               end
	  end;
	{error,{already_loaded,_}}->
		#app_state{module = application, name = Handle, mode = duplicate, state = off };
	_->[]
	end
.

app_start(X, all, Mode) when (X#app_state.state == off) and (X#app_state.mode == Mode) ->
	     M = X#app_state.module,
	     case M:start(X#app_state.name) of
	       ok -> X#app_state{state = on};
	       {error,{already_started,_}} -> X#app_state{state = on};
	       _ ->  X#app_state{state = error}
	     end
;
app_start(X, Name, Mode) when X#app_state.state == off, X#app_state.name == Name ->
	     M = X#app_state.module,
	     case M:start(X#app_state.name) of
	       ok -> X#app_state{mode = Mode, state = on};
	       {error,{already_started,_}} -> X#app_state{state = on, mode = Mode};
	       _ ->  X#app_state{state = error}
	     end
;
app_start(X, _, _) -> X.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([])->
  HandleServices = [ name(Name, Config) || {Name, Config} <- config_srv:get_config()],
  {ok, HandleServices}
.

handle_call(all, _From, HandleServices) ->
  {reply, HandleServices, HandleServices};
handle_call(status_all, _From, HandleServices) ->
  {reply, HandleServices, HandleServices};
handle_call({start_service, ServiceName, StartMode}, _From, HandleServices) ->
  case [ X || X <- HandleServices, is_record(X, app_state), (X#app_state.name == ServiceName) or (ServiceName == all)] of
    [] -> application:load(ServiceName),
          config_srv:apply(ServiceName),
	  application:start(ServiceName),
          {reply, ok, HandleServices}
    ;
    _ -> 
          NewHandleServices =
          lists:map(
%%                fun(X) when is_record(X, app_state), X#app_state.state == off, (X#app_state.name == ServiceName) or (ServiceName == all) ->
%%                     M = X#app_state.module,
%%                     case M:start(X#app_state.name) of
%%                       ok -> X#app_state{mode = StartMode, state = on};
%%                       {error,{already_started,_}} -> X#app_state{state = on};
%%                       _ ->  X#app_state{state = error}
%%                     end;
%%                   (X) -> X
%%                end,
		fun(X)-> app_start(X, ServiceName, StartMode) end,
                HandleServices 
          ),
          {reply, ok, NewHandleServices}
  end
  
;
handle_call({stop_service, all, StopMode}, _From, HandleServices) ->
  NewHandleServices = 
  lists:map(
    fun(X) when X#app_state.state == on, X#app_state.mode == StopMode
	   -> (X#app_state.module):stop(X#app_state.name), 
              X#app_state{state = off};
       (X) -> X
    end,
    HandleServices
  ),
  {reply, ok, NewHandleServices}
;
handle_call({stop_service, ServiceName, StopMode}, _From, HandleServices) ->
  application:stop(ServiceName),
  application:unload(ServiceName),
  NewHandleServices = 
  lists:map(
    fun(X) when X#app_state.name == ServiceName ->
           (X#app_state.module):stop(X#app_state.name), 
           X#app_state{state = off, mode = StopMode};
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

