-module(nconfig_app).
-author('komm@siphost.su').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    nconfig_sup:start_link().

stop(_State) ->
    ok.
