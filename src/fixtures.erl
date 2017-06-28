-module(fixtures).

%% API exports
-export([load/1, apply/1]).

%%====================================================================
%% API functions
%%====================================================================

load(FixturesPath) ->
  code:add_patha(FixturesPath),
  yamerl_app:set_param(node_mods, [yamerl_node_erlang_atom]),
  {ok, Filenames} = file:list_dir(FixturesPath),
  [read_fixture(FixturesPath, Filename) || Filename <- Filenames, filename:extension(Filename) =:= ".yml"].

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

  Queries = [sqerl:sql({insert, Table, Values}, true) || {Table, Values} <- FixturesTransformed],
  % [erlang:display(binary_to_list(Q)) || Q <- Queries],
  [odbc:sql_query(Ref, binary_to_list(Query)) || Query <- Queries].

%%====================================================================
%% Internal functions
%%====================================================================

read_fixture(Filepath, Filename) ->
  Fullname = filename:join(Filepath, Filename),
  Module = list_to_atom(filename:basename(Filename, ".yml")),
  Render = case code:load_file(Module) of
    {module, Module} ->
      mustache:render(Module, Fullname, []);
    _ ->
      {_, Content} = file:read_file(Fullname),
      Compiled = mustache:compile(binary_to_list(Content)),
      mustache:render(undefined, Compiled)
  end,
  {Module, yamerl_constr:string(Render, [{erlang_atom_autodetection, true}])}.


