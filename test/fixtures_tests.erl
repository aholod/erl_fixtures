-module(fixtures_tests).

-include_lib("eunit/include/eunit.hrl").


fixtures_test_() ->
  {foreach,
   fun start/0,
   fun stop/1,
   [
    fun check_run/1
   ]
  }.

start() ->
  application:ensure_all_started(yamerl),
  Fixtures = fixtures:load("test/fixtures"),
  fixtures:apply(Fixtures),
  ok.
stop(_) ->
  ok.


check_run(_) ->
  % Result = fixtures:get_obj("users", dimon),
  Result = 4,
  [?_assertEqual(4, Result)].


