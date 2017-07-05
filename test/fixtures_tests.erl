-module(fixtures_tests).

-include_lib("eunit/include/eunit.hrl").


fixtures_test_() ->
  {setup,
   fun setup/0,
   fun cleanup/1,
   {with, [
          fun check_load/1,
          fun check_links/1
         ]
   }
  }.

setup() ->
  lager:start(),
  lager:set_loglevel(lager_console_backend, debug),
  Fixtures = fixtures:load("test/fixtures"),
  % fixtures:apply(Fixtures),
  Fixtures.
  % ok.

cleanup(_) ->
  ok.


check_load(Fixtures) ->
  ?debugVal(Fixtures),
  Users = fixtures:get_table(as_users, Fixtures),
  Admin = fixtures:get_entity(admin, Users),
  Admin2 = fixtures:get_entity(as_users, admin, Fixtures),
  Dimon = fixtures:get_entity(dimon, Users),
  Attrs = fixtures:get_table(as_user_attributes, Fixtures),
  MsisdnDimon = fixtures:get_entity(msisdn_dimon, Attrs),
  MsisdnAdmin = fixtures:get_entity(msisdn_admin, Attrs),
  DispNameDimon = fixtures:get_entity(disp_name_dimon, Attrs),

  [?assertEqual(2, length(Users)),
   ?assertEqual(10, proplists:get_value(asur_id, Admin)),
   ?assertEqual(erlpass, proplists:get_value(password, Admin)),
   ?assertEqual('13.06.2017 07:40:00', proplists:get_value(navi_date, Admin)),
   ?assertEqual(null, proplists:get_value(null_attr, Admin)),
   ?assertEqual(80, proplists:get_value(asur_id, Dimon)),
   ?assertEqual(ymlpass, proplists:get_value(password, Dimon)),
   ?assertEqual(Admin, Admin2),
   ?assertEqual(3, length(Attrs)),
   ?assertEqual(22, proplists:get_value(asua_id, MsisdnDimon)),
   ?assertEqual(22, proplists:get_value(asua_id, MsisdnAdmin)),
   ?assertEqual(11, proplists:get_value(asua_id, DispNameDimon))
  ].

check_links(Fixtures) ->
  Users = fixtures:get_table(as_users, Fixtures),
  Admin = fixtures:get_entity(admin, Users),
  Dimon = fixtures:get_entity(dimon, Users),
  Attrs = fixtures:get_table(as_user_attributes, Fixtures),
  MsisdnDimon = fixtures:get_entity(msisdn_dimon, Attrs),
  MsisdnAdmin = fixtures:get_entity(msisdn_admin, Attrs),
  DispNameDimon = fixtures:get_entity(disp_name_dimon, Attrs),

  [?assertEqual(proplists:get_value(asur_id, Dimon), proplists:get_value(asur_asur_id, MsisdnDimon)),
   ?assertEqual(proplists:get_value(asur_id, Admin), proplists:get_value(asur_asur_id, MsisdnAdmin)),
   ?assertEqual(proplists:get_value(asur_id, Dimon), proplists:get_value(asur_asur_id, DispNameDimon))
  ].
