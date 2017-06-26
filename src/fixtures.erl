-module(fixtures).

%% API exports
-export([load/1, apply/1, name/0]).

%%====================================================================
%% API functions
%%====================================================================

load(FixturesPath) ->
  {ok, Filenames} = file:list_dir(FixturesPath),
  FixturesFiles = [{Filename, filename:join(FixturesPath, Filename)} || Filename <- Filenames, filename:extension(Filename) =:= ".yml"],
  ReadedFiles = [{filename:basename(Filename, ".yml"), file:read_file(File)} || {Filename, File} <- FixturesFiles],
  Compiles = [{Filename, mustache:compile(binary_to_list(Data))} || {Filename, {ok, Data}} <- ReadedFiles],
  Renders = [{Filename, mustache:render(undefined, CompiledTemplate)} || {Filename, CompiledTemplate} <- Compiles],
  Result = [{Filename, yamerl_constr:string(RenderedTemplate)} || {Filename, RenderedTemplate} <- Renders],
  erlang:display(Result),
  Result.

apply(_Fixtures) ->
  CS = "DSN=oraxe;UID=SSO;PWD=sso",
  Options = [{scrollable_cursors, off}],
  DisableConstraint =
    "select 'alter table '||table_name||' disable constraint '||constraint_name||';' from user_constraints where constraint_type = 'R'",
  EnableConstraints =
    "select 'alter table '||table_name||' enable constraint '||constraint_name||';' from user_constraints where constraint_type = 'R'",
  DelStmt = "delete from AS_USERS",

  ok = odbc:start(),
  {ok, Ref} = odbc:connect(CS, Options),
  {_, _, ResultConstraints} = odbc:sql_query(Ref, DisableConstraint),
  [odbc:sql_query(Ref, Query) || {Query} <- ResultConstraints],
  Result = odbc:sql_query(Ref, DelStmt),
  erlang:display(Result).

name() -> "lala".

%%====================================================================
%% Internal functions
%%====================================================================
