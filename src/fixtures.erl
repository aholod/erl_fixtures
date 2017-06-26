-module(fixtures).

%% API exports
-export([load/1, get_obj/2, name/0]).

%%====================================================================
%% API functions
%%====================================================================

load(FixturesPath) ->
  {ok, Filenames} = file:list_dir(FixturesPath),
  FixturesFiles = [ filename:join(FixturesPath, File) || File <- Filenames, filename:extension(File) =:= ".yml"],
  erlang:display(FixturesFiles),
  % Renders = [ mustache:render(fixtures, File) || File <- FixturesFiles],
  R2 = mustache:render("test/fixtures/hosts.yml"),
  erlang:display(R2),
  R1 = mustache:render(fixtures, "test/fixtures/users.yml"),
  erlang:display(R1),
  ok.

get_obj(Fixture, Obj) ->
  {ok, Path} = application:get_env(fixtures, fixtures_path),
  FixtureFilePath = lists:concat([Path, "/", Fixture, ".yml"]),
  erlang:display(FixtureFilePath),
  % {ok, RawData} = file:read_file(FixtureFilePath),
  % Ctx = dict:from_list([]),
  % TFun = mustache:compile(binary_to_list(RawData)),
  Result = mustache:render(fixtures, FixtureFilePath),
  % Yaml = binary_to_list(Result),
  erlang:display(Result),
  Data = yamerl_constr:string(Result),
  erlang:display(Data),
  ok.

name() -> "lala".

%%====================================================================
%% Internal functions
%%====================================================================
