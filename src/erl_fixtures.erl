-module(erl_fixtures).

-export([get_obj/2]).


get_obj(Fixture, Obj) ->
  {ok, Path} = application:get_env(erl_fixtures, fixtures_path),
  FixtureFile = lists:concat([Path, "/", Fixture, ".yml"]),
  erlang:display(FixtureFile),
  erlydtl:compile_template(FixtureFile, template_mod),
  {ok, R}  = template_mod:render([]),
  erlang:display(R),
  ok.
  % yamerl_parser:string(R).
