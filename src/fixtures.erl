-module(fixtures).

%% API exports
-export([load/1, apply/1]).

%%====================================================================
%% API functions
%%====================================================================

load(FixturesPath) ->
  yamerl_app:set_param(node_mods, [yamerl_node_erlang_atom]),
  {ok, Filenames} = file:list_dir(FixturesPath),
  FixturesFiles = [{Filename, filename:join(FixturesPath, Filename)} || Filename <- Filenames, filename:extension(Filename) =:= ".yml"],
  ReadedFiles = [{filename:basename(Filename, ".yml"), file:read_file(File)} || {Filename, File} <- FixturesFiles],
  Compiles = [{Filename, mustache:compile(binary_to_list(Data))} || {Filename, {ok, Data}} <- ReadedFiles],
  Renders = [{Filename, mustache:render(undefined, CompiledTemplate)} || {Filename, CompiledTemplate} <- Compiles],
  Result = [{Filename, yamerl_constr:string(RenderedTemplate, [{erlang_atom_autodetection, true}])} || {Filename, RenderedTemplate} <- Renders],
  erlang:display(Result),
  Result.

apply(Fixtures) ->
  CS = "DSN=oraxe;UID=SSO;PWD=sso",
  Options = [{scrollable_cursors, off}, {auto_commit, on}],
  DisableConstraint =
    "select 'alter table '||table_name||' disable constraint '||constraint_name||';' from user_constraints where constraint_type = 'R'",
  EnableConstraints =
    "select 'alter table '||table_name||' enable constraint '||constraint_name||';' from user_constraints where constraint_type = 'R'",
  DelStmt = "delete from AS_USERS",

  ok = odbc:start(),
  {ok, Ref} = odbc:connect(CS, Options),
  {_, _, ResultConstraints} = odbc:sql_query(Ref, DisableConstraint),
  [odbc:sql_query(Ref, Query) || {Query} <- ResultConstraints],
  odbc:sql_query(Ref, DelStmt),

  FixturesTransformed = lists:foldl(
    fun(Elem, Acc)->
        {Table, Params} = Elem,
        NamedParams = lists:map(fun({_, Param}) -> {Table, Param} end, lists:flatten(Params)),
        lists:append(Acc, NamedParams)
    end,
    [], Fixtures),

  Queries = [sqerl:sql({insert, list_to_atom(Table), Values}, true) || {Table, Values} <- FixturesTransformed],
  InsertResult = [odbc:sql_query(Ref, binary_to_list(Query)) || Query <- Queries],
  erlang:display(InsertResult).

%%====================================================================
%% Internal functions
%%====================================================================
