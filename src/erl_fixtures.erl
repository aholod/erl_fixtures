-module(erl_fixtures).

% -export([get_obj/2, name/0]).
-compile(export_all).

get_obj(Fixture, Obj) ->
  {ok, Path} = application:get_env(erl_fixtures, fixtures_path),
  FixtureFilePath = lists:concat([Path, "/", Fixture, ".yml"]),
  erlang:display(FixtureFilePath),
  % {ok, RawData} = file:read_file(FixtureFilePath),
  % Ctx = dict:from_list([]),
  % TFun = mustache:compile(binary_to_list(RawData)),
  Result = mustache:render(erl_fixtures, FixtureFilePath),
  % Yaml = binary_to_list(Result),
  erlang:display(Result),
  Data = yamerl_constr:string(Result),
  erlang:display(Data),
  ok.

name() -> "lala".
