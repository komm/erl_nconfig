-module(config_srv).
-author('komm@siphost.su').

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_APP, nconfig).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_config/0, get_config/1, read_config/1, update_config/1, save_config/1, apply/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_)->
  Config=read_config(file),
  {ok, Config}
.

handle_call(all, _From, Config) ->
  {reply, Config, Config};
handle_call({get,Val}, _From, Config) ->
  {reply, pp(Val,Config), Config}
;
handle_call({update_config, file} ,_From, _Config)->
   {reply,ok, read_config(file)}
;
handle_call({update_config, json, Json} ,_From, _Config)->
   NewConfig = mochijson2:decode(Json),
   Fun =
   fun(Fun, {struct, Array})-> [{Name, Fun(Fun, X)} || {Name, X} <- Array ];
      (Fun, Array) when is_list(Array)-> [Fun(Fun, Y) || Y<-Array]; 
      (_, Int) when is_integer(Int)-> Int; 
      (_, Bin) when is_binary(Bin)-> 
                 list_to_atom(binary_to_list(Bin)) 
   end,
   {reply,ok, Fun(Fun, NewConfig)}
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
	Diff = [ X || {X, _} <- RequredParameter] -- [X || {X, _} <- Value],
	[{BlockName, Value ++ [lists:keyfind(X,1,RequredParameter) || X <- Diff ]}] ++ compare(Tail, Template)

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
                                     {error, BadValue},
                                     {string, io_lib:format('~s', [binary_to_list(BadString)])},
                                     "Load default parameters"
                                    ]),
	  default()
        ;
	Config when is_list(Config)-> 
		compare(Config, default())
      end
  end
;
read_config(http)->none;
read_config(json)->none.

get_config()->
   gen_server:call({global, ?MODULE}, all).
get_config(Val)->
   case gen_server:call({global, ?MODULE}, {get, Val}) of
     false -> application:get_all_env(Val);
     List -> List ++ application:get_all_env(Val)
   end
.

-spec default() -> Config :: list().
default()->
	application:get_all_env(?DEFAULT_APP) -- [{included_applications,[]}]
.

-spec save_config( file )            -> ok;
		 ( raw )             -> Config :: list();
		 ( Value :: term() ) -> error.
save_config(raw)-> 
      lists:flatten(
      [io_lib:format('~p{\n~s}\n',[X, 
		[case C of 
                 argv -> io_lib:format('\t~s = "~s";\n',[C, V]);
                 _ -> io_lib:format('\t~s = ~s;\n',[C, V])
                 end
                || {C, V} <- Y] ]) 
      || {X,Y} <- get_config()])
;
save_config(file)-> 
  Argx=init:get_arguments(),
  case lists:keyfind(conf,1,Argx) of
    false-> error;
    {conf,Path}->
      {ok, File} = file:open(Path, write),
      [io:format(File, '~p{\n~s}\n',[X, 
		[case C of 
                 argv -> io_lib:format('\t~s = "~s";\n',[C, V]);
                 _ -> io_lib:format('\t~s = ~s;\n',[C, V])
                 end
                || {C, V} <- Y] ]) 
      || {X,Y} <- get_config()],
      file:close(File),
      ok
  end
;
save_config(_)->
  error
.

-spec update_config( Value :: term() ) -> ok. 
update_config(file)->
  gen_server:call({global, ?MODULE}, {update_config, file}),
  ok
;
update_config({json, Json})->
  gen_server:call({global, ?MODULE}, {update_config, json, Json}),
  ok
.

-spec apply(App :: list()) -> ok | error.
apply(App)->
  case get_config(App) of
   false -> error;
   List when is_list(List) ->
     [application:set_env(App, Value, Parameter) || {Value, Parameter} <- List],
     ok
  end
.

