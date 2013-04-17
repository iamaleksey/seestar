-module(seestar_session_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("seestar/include/constants.hrl").

schema_queries_test() ->
    {ok, Pid} = seestar_session:start_link("localhost", 9042),

    Qry0 = "CREATE KEYSPACE seestar_test_keyspace "
           "WITH REPLICATION = {'class': 'SimpleStrategy', 'replication_factor': 1}",
    {ok, Res0} = seestar_session:perform(Pid, Qry0),
    ?assertEqual(schema_change, seestar_result:type(Res0)),
    ?assertEqual(<<"seestar_test_keyspace">>, seestar_result:keyspace(Res0)),
    ?assertEqual(undefined, seestar_result:table(Res0)),

    {error, Err0} = seestar_session:perform(Pid, Qry0),
    ?assertEqual(?ALREADY_EXISTS, seestar_error:code(Err0)),
    ?assertEqual(<<"seestar_test_keyspace">>, seestar_error:keyspace(Err0)),
    ?assertEqual(undefined, seestar_error:table(Err0)),

    Qry1 = "USE seestar_test_keyspace",
    {ok, Res1} = seestar_session:perform(Pid, Qry1),
    ?assertEqual(set_keyspace, seestar_result:type(Res1)),
    ?assertEqual(<<"seestar_test_keyspace">>, seestar_result:keyspace(Res1)),

    Qry2 = "CREATE TABLE seestar_test_table (id int primary key, value text)",
    {ok, Res2} = seestar_session:perform(Pid, Qry2),
    ?assertEqual(schema_change, seestar_result:type(Res2)),
    ?assertEqual(<<"seestar_test_keyspace">>, seestar_result:keyspace(Res2)),
    ?assertEqual(<<"seestar_test_table">>, seestar_result:table(Res2)),

    {error, Err1} = seestar_session:perform(Pid, Qry2),
    ?assertEqual(?ALREADY_EXISTS, seestar_error:code(Err1)),
    ?assertEqual(<<"seestar_test_keyspace">>, seestar_error:keyspace(Err1)),
    ?assertEqual(<<"seestar_test_table">>, seestar_error:table(Err1)),

    Qry3 = "DROP TABLE seestar_test_table",
    {ok, _Res3} = seestar_session:perform(Pid, Qry3),

    Qry4 = "DROP KEYSPACE seestar_test_keyspace",
    {ok, _Res4} = seestar_session:perform(Pid, Qry4).
