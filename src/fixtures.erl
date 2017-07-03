-module(fixtures).

%% API exports
-export([load/1, apply/1]).

%%====================================================================
%% API functions
%%====================================================================

load(FixturesPath) ->
  code:add_patha(FixturesPath),
  {ok, Filenames} = file:list_dir(FixturesPath),
  Fixtures = [read_fixture(FixturesPath, Filename) || Filename <- Filenames, filename:extension(Filename) =:= ".yml"],
  discover_refs(Fixtures).

apply(Fixtures) ->
  CS = "DSN=oraxe;UID=SSO;PWD=sso",
  Options = [{scrollable_cursors, off}, {auto_commit, on}],
  DisableConstraintsGen =
    "select 'alter table '||table_name||' disable constraint '||constraint_name||';' from user_constraints where constraint_type = 'R'",
  EnableConstraintsGen =
    "select 'alter table '||table_name||' enable constraint '||constraint_name||';' from user_constraints where constraint_type = 'R'",

  ok = odbc:start(),
  {ok, Ref} = odbc:connect(CS, Options),
  {_, _, DisableConstraints} = odbc:sql_query(Ref, DisableConstraintsGen),
  {_, _, EnableConstraints} = odbc:sql_query(Ref, EnableConstraintsGen),
  [odbc:sql_query(Ref, Query) || {Query} <- DisableConstraints],
  % lager:debug("Fixtures before transform: ~p", [Fixtures]),
  FixturesTransformed = lists:foldl(
    fun(Elem, Acc)->
        {Table, Params} = Elem,
        NamedParams = lists:map(fun({_, Param}) -> {Table, Param} end, Params),
        lists:append(Acc, NamedParams)
    end,
    [], Fixtures),

  [odbc:sql_query(Ref, lists:append("truncate table ", atom_to_list(Table))) || {Table, _} <- FixturesTransformed],
  Queries = [sqerl:sql({insert, Table, Values}, true) || {Table, Values} <- FixturesTransformed],
  % [lager:debug("Ready query: ~p", [binary_to_list(Q)]) || Q <- Queries],
  [odbc:sql_query(Ref, binary_to_list(Query)) || Query <- Queries],
  [odbc:sql_query(Ref, Query) || {Query} <- EnableConstraints].

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
  {ok, [Y]} = yaml:load(list_to_binary(Render), [implicit_atoms]),
  {Module, Y}.

discover_refs(Fixtures) ->
  [{Table, scan_entities(Entities, Fixtures)} || {Table, Entities} <- Fixtures].

scan_entities(Entities, Fixtures) ->
  [{Name, scan_attrs(Attrs, Fixtures)} || {Name, Attrs} <- Entities, is_atom(Name)].

scan_attrs(Attrs, Fixtures) ->
  [{Name, proc_attr(Value, Fixtures)} || {Name, Value} <- Attrs].

proc_attr(Value = [{_, _},{_, _},{_, _}], Fixtures) ->
  F = proplists:get_value(fixture, Value),
  E = proplists:get_value(entity, Value),
  R = proplists:get_value(ref, Value),
  Entities = proplists:get_value(F, Fixtures),
  Attrs = proplists:get_value(E, Entities),
  proplists:get_value(R, Attrs);

proc_attr(Value, _) ->
  Value.

