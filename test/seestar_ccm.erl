-module(seestar_ccm).

-export([create/0, create/1, remove/0, start/0, stop/0, update_config/1, configure_ssl/0]).

create() ->
    create(1).

create(Nodes) ->
    cmd("create seestar_unit_test_cluster -n ~w -b --install-dir ~s",
        [Nodes, getenv("CASSANDRA_DIR", "./")]).

remove() ->
    stop(),
    cmd("remove").

start() ->
    cmd("start").

stop() ->
    cmd("stop").

cmd(Cmd) ->
    os:cmd("ccm " ++ Cmd).

cmd(Pattern, Variables) ->
    cmd(lists:flatten(io_lib:format(Pattern, Variables))).

update_config(Configs) ->
    cmd("updateconf ~s" , [string:join(Configs, " ")]).

configure_ssl() ->
    CassandraKeyStore = getenv("CASSANDRA_KEYSTORE", ".keystore"),
    CassandraKeyStorePass = getenv("CASSANDRA_KEYSTOREPASS", "cassandra"),
    seestar_ccm:update_config([
        "'client_encryption_options.enabled: true'",
        io_lib:format("'client_encryption_options.keystore: ~s'", [CassandraKeyStore]),
        io_lib:format("'client_encryption_options.keystore_password: ~s'", [CassandraKeyStorePass])
    ]).

getenv(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        Value -> Value
    end.
