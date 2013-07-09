-module(service_srv).
-author('komm@siphost.su').

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_APP, nconfig).
-record(app_state,{
          module :: atom(),
          name :: atom() | list(),
          state :: 'on' | 'off',
          mode = auto :: 'auto' | 'manual',
	  node :: node(),
	  ctime :: term()
       }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_service/2, stop_service/2, restart_service/2, status_services/0, reload_services/0]).

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
  [{start,     1},
   {stop,      1},
   {restart,   1},
   {status,    0},
   {register,  1},
   {unregister,1}
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

-spec reload_services()->list().
reload_services()->
  gen_server:call({global, ?MODULE}, reload_services)

.

%%hack for node parameters
name(node, _)->[];
%%/hack
name(Handle, Params)->
	case application:load(Handle) of
	ok -> 
		config_srv:apply(Handle), 
		#app_state{module = application, name = Handle, mode = auto, state = off, node = node() };
	{error,{"no such file or directory",_}} ->
  	  ModuleName = list_to_atom("handle_" ++ atom_to_list(Handle)),
	  case code:which(ModuleName) of
	    non_existing -> 
               %%#app_state{module= ModuleName, name = '', state = error, mode = manual};
               error_logger:error_report([{?MODULE, name}, {error_handler, ModuleName}]),
               [];
	    _ ->
	       %%TODO: valide already started services
	       case catch ModuleName:register(Params) of
                 error -> [];
                 {'EXIT', _}-> [];
                 {ok, Name} -> #app_state{ module = ModuleName, name = Name, state = off, mode = auto};
                 {ok, Name, Node} -> #app_state{ module = ModuleName, name = Name, state = off, mode = auto, node = Node}
               end
	  end;
	{error,{already_loaded,_}}->
		#app_state{module = application, name = Handle, mode = auto, state = off };
	_->[]
	end
.

app_start(X, all, Mode) when (X#app_state.state == off) and (X#app_state.mode == Mode) ->
	     M = X#app_state.module,
	     case catch M:start(X#app_state.name) of
	       ok -> X#app_state{state = on, ctime = now()};
	       {error,{already_started,_}} -> X#app_state{state = on};
	       _ ->  X#app_state{state = error}
	     end
;
app_start(X, Name, Mode) when X#app_state.state == off, X#app_state.name == Name ->
	     M = X#app_state.module,
	     case catch M:start(X#app_state.name) of
	       ok -> X#app_state{mode = Mode, state = on, ctime = now()};
	       {error,{already_started,_}} -> X#app_state{state = on, mode = Mode};
	       _ ->  X#app_state{state = error}
	     end
;
app_start(X, _, _) -> X.

app_restart(X, Name, Mode) when X#app_state.name == Name, X#app_state.mode == Mode ->
             application:load(Name),
             case lists:keyfind(Name, 1, application:loaded_applications()) of
             {Name, _, _} -> 
                application:stop(Name),
                config_srv:apply(Name),
                case application:start(Name) of
		ok -> X#app_state{state=on, ctime = now()};
		_ -> X#app_state{state=error, ctime = undefined}
		end
             ;
             false -> 
	        M = X#app_state.module,
		case catch M:restart(X#app_state.name) of
		   ok -> X#app_state{mode = Mode, state = on, ctime = now()};
		   _ ->  X#app_state{state = error, ctime = undefined}
		end
             end
.

app_status(X) when X#app_state.state == on, X#app_state.module /= application ->
  X#app_state{state = (X#app_state.module):status(X#app_state.name)}
;
app_status(X) ->
  X
.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([])->
  HandleServices = [ name(Name, Config) || {Name, Config} <- config_srv:get_config()],
  {ok, lists:flatten(HandleServices)}
.

handle_call(all, _From, HandleServices) ->
  {reply, HandleServices, HandleServices};
handle_call(status_all, _From, HandleServices) ->
  Resp = lists:map(fun app_status/1, HandleServices),
  {reply, Resp, HandleServices};
handle_call({start_service, ServiceName, StartMode}, _From, HandleServices) ->
  case [ X || X <- HandleServices, is_record(X, app_state), (X#app_state.name == ServiceName) or (ServiceName == all)] of
    [] -> application:load(ServiceName),
          config_srv:apply(ServiceName),
          StateApp = 
	  case application:start(ServiceName) of
            ok -> on;
            {error, {already_started, _}} -> on;
            _ -> false
          end,
          {reply, ok, HandleServices ++ [#app_state{module = application, name = ServiceName, mode = StartMode, state = StateApp, ctime = now()}]}
    ;
    _ -> 
          NewHandleServices =
          lists:map(
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
              X#app_state{state = off, ctime = undefined};
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
           X#app_state{state = off, mode = StopMode, ctime = undefined};
       (X) -> X
    end,
    HandleServices
  ),
  {reply, ok, NewHandleServices}
;
handle_call({restart_service, all, RestartMode}, _From, HandleServices) ->
  NewHandleServices = 
  lists:map(
    fun(X) -> app_restart(X, X#app_state.name, RestartMode) end,
    HandleServices
  ),
  {reply, ok, NewHandleServices}
;
handle_call({restart_service, ServiceName, RestartMode}, _From, HandleServices) ->
  NewHandleServices = 
  lists:map(
    fun(X) when X#app_state.name == ServiceName -> app_restart(X, X#app_state.name, RestartMode);
       (X) -> X
    end,
    HandleServices
  ),
  {reply, ok, NewHandleServices}
;
handle_call(reload_services, _From, Services) ->
  %stop disable services and remove.
  ReRegister = [ name(Name, Config) || {Name, Config} <- config_srv:get_config()],
  RemovedRegister = [X#app_state{state=off, mode=auto} || X<-Services, (X#app_state.mode == auto) or (X#app_state.mode == error) ] -- ReRegister,
  [ catch (X#app_state.module):stop(X#app_state.name) || X<-RemovedRegister, X#app_state.state == on ],
  [ catch (X#app_state.module):unregister(X#app_state.name) || X<-RemovedRegister, X#app_state.module /= application],
  [ application:unload(X#app_state.name) || X<-RemovedRegister, X#app_state.module == application],

  %restart old services
  OldLivedServices = ([X#app_state{state=off, mode=auto} || X<-Services,is_record(X, app_state), X#app_state.mode /= manual] -- RemovedRegister),
  S1 = [ app_restart(X, X#app_state.name, auto) || X<-OldLivedServices,is_record(X, app_state)],

  %start new services
  NewServices = ReRegister -- OldLivedServices,

  error_logger:info_report([{?MODULE, handle_call},
				{previous_services, Services},
				{restart_services, OldLivedServices},
				{removed_services, RemovedRegister},
				{new_services, NewServices}
			   ]),
  S2 = [app_start(X, X#app_state.name, auto) || X <- NewServices, is_record(X, app_state)],
  {reply, ok, lists:flatten(S1 ++ S2)}
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

