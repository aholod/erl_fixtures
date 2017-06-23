-module(dtl_tests).

-include_lib("eunit/include/eunit.hrl").

dtl_test_() ->
  {foreach,
   fun start/0,
   fun stop/1,
   [
    fun check_run/1
   ]
  }.

start() ->
  application:ensure_all_started(yamerl),
  application:set_env(erl_fixtures, fixtures_path, "test/fixtures"),
  ok.
stop(_) ->
  ok.


check_run(_) ->
  Result = erl_fixtures:get_obj("users", dimon),
  [?_assertEqual(4, Result)].
