-module(seestar_ssl_tests).
-author("odobroiu").

-include_lib("eunit/include/eunit.hrl").
%% API
-export([]).

ssl_test_() ->
    {setup,
        fun() ->
            seestar_ccm:create(),
            seestar_ccm:configure_ssl(),
            seestar_ccm:start(),
            timer:sleep(500),
            %% Eunit does not start the app, so ssl does not get started
            {ok, _Apps} = application:ensure_all_started(ssl)
        end,
        fun(_) ->
            seestar_ccm:remove()
        end,
        [
            fun simple/0
        ]}.

simple() ->
    Qry0 = "CREATE KEYSPACE seestar "
    "WITH REPLICATION = {'class': 'SimpleStrategy', 'replication_factor': 1}",
    {ok, Pid} = seestar_session:start_link("localhost", 9042, [], [{ssl, []}]),
    unlink(Pid),
    {ok, Res0} = seestar_session:perform(Pid, Qry0, one),
    ?assertEqual(schema_change, seestar_result:type(Res0)),
    ?assertEqual(<<"seestar">>, seestar_result:keyspace(Res0)),
    ?assertEqual(undefined, seestar_result:table(Res0)),
    ok = seestar_session:stop(Pid).