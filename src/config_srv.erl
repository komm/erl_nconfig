-module(config_srv).
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

-export([start_link/1, get_config/0, get_config/1, read_config/1, update_config/1, save_config/1, start_services/0]).

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
  {ok, Pid} = gen_server:start_link({global, ?SERVER}, ?MODULE, [HandleServices], []),
  start_services(),
  {ok, Pid}.

-spec behaviour_info(callbacks) -> list().
behaviour_info(callbacks) ->
  [{start,1},
   {stop,1},
   {restart,1}
  ].

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec save_config( file )            -> ok;
		 ( Value :: term() ) -> error.
save_config(file)-> 
  Argx=init:get_arguments(),
  case lists:keyfind(conf,1,Argx) of
    false-> error;
    {conf,Path}->
      {ok, File} = file:open(Path, write),
      Data = 
      [io:format(File, '~p{\n~s}\n',[X, 
		[case C of 
                 argv -> io_lib:format('\t~s = "~s";\n',[C, V]);
                 _ -> io_lib:format('\t~s = ~s;\n',[C, V])
                 end
                || {command, C, V} <- Y] ]) 
      || {X,Y} <- get_config()],
      file:close(File),
      ok
  end
;
save_config(_)->
  error
.


stop_service(SeviceName)->
  error_logger:info_report([{?MODULE, stop_service}, {ServiceName, stop}]),
  ServiceName:stop(Config)
.
-spec stop_services() -> ok.
stop_services()->
  {_, HandleServices} = get_config(),
  [ stop_service(X) || X <- HandleServices],
  ok
.

start_service(SeviceName, false)->
  error_logger:info_report([{?MODULE, start_service}, {ServiceName, not_started}]),
  not_started
;
start_service(SeviceName, Config)->
  error_logger:info_report([{?MODULE, start_service}, {ServiceName, initialization}]),
  case ServiceName:start(Config) of
    ok->
  end
.
-spec start_services() -> ok.
start_services()->
  {Config, HandleServices} = get_config(),
  [ start_service(X, get_config(X)) || X <- HandleServices],
  ok
.

%TODO: already running?
restart_service(SeviceName, false)->
  error_logger:info_report([{?MODULE, start_service}, {ServiceName, not_started}]),
  not_started
;
restart_service(SeviceName, Config)->
  error_logger:info_report([{?MODULE, start_service}, {ServiceName, initialization}]),
  ServiceName:start(Config)
.

-spec restart_services() -> ok.
restart_services()->
  {Config, HandleServices} = get_config(),
  [ start_service(X, get_config(X)) || X <- HandleServices],
  ok
.

-spec update_config( Value :: term() ) -> ok. 
update_config(_Val)->
  gen_server:call({global, ?MODULE}, update_config),
  ok
.

init([HandleServices])->
  Config=read_config(file),
  {ok, {Config, HandleServices}}
.

handle_call(all, _From, Config) ->
  {reply, Config, Config};
handle_call({get,Val}, _From, Config) ->
  {reply, pp(Val,Config), Config}
;
handle_call(update_config,_From,_Config)->
   {reply,ok, read_config(file)}
.
handle_cast(_Msg, Config) ->
  {noreply, Config}.

handle_info(_Info, Config) ->
  {noreply, Config}.

terminate(_Reason, _Config) ->
  ok.

code_change(_OldVsn, Config, _Extra) ->
  {ok, Config}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec pp(Value :: term(), Config :: list()) -> false | term().
pp(Val,Config)->
   case lists:keyfind(Val,1,Config) of
        false-> false;
        {Val,R}-> R;
        Other-> Other
   end
.

-spec compare(Key :: list(), Template :: list()) -> Result :: list().
compare([],_)->[];
compare([HeadConfig|Tail], Template)->
	{BlockName, Value} = HeadConfig,
	RequredParameter = case pp(BlockName, Template) of
		error -> [];
		false -> [];
		Other -> Other
	end,
	Diff = [ X || {command, X, _} <- RequredParameter] -- [X || {command, X, _} <- Value],
	[{BlockName, Value ++ [lists:keyfind(X,2,RequredParameter) || X <- Diff ]}] ++ compare(Tail, Template)

.

-spec read_config( http ) -> none;
		 ( json ) -> none;
		 ( file ) -> Config :: list().
read_config(file)->
  Argx=init:get_arguments(),
  case lists:keyfind(conf,1,Argx) of
    false->default();
    {conf,Path}->
      case emd_config:file(Path) of
        {_,BadString,BadValue}->
          error_logger:error_report([{?MODULE, read_config},
                                     BadValue, io_lib:format('~s', [binary_to_list(BadString)])]),
	  []
        ;
	Config when is_list(Config)-> 
		compare(Config, default()),
      end
  end
;
read_config(http)->none;
read_config(json)->none.

get_config()->
   gen_server:call({global, ?MODULE}, all).
get_config(Val)->
   gen_server:call({global, ?MODULE}, {get, Val}).

-spec default() -> Config :: list().
default()->
	application:get_all_env(?DEFAULT_APP)
.
