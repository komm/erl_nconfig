-module(config_srv_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    get_all,
    get_first
    % список тестов
  ].

init_per_suite(Config) ->
  % действия, выполняемые перед запуском набора тестов
  Config.

init_per_testcase(_, Config) ->
  % действия, выполняемые перед запуском теста
  {ok, _Pid} = config_srv:start_link(),
  Config.

end_per_testcase(_, Config) ->
  % действия, выполняемые после завершения теста
  Config.

end_per_suite(Config) ->
  % действия, выполняемые после завершения всего набора тестов
  Config.

% код тестов

% обратите внимание, что ни одно из объявлений
% init_*/end_* функций не является обязательным

get_all(Config)->
  config_srv:get_config(),
  Config.

get_first(Config) ->
  Element = config_srv:get_config("/main"),
  Element = config_srv:get_config("main"),
  Element = config_srv:get_config("/main/"),
  Element = config_srv:get_config("main/"),
  ?assertEqual([{<<"name">>,<<"MAIN">>}], Element),
  Config.

