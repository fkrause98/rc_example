-module(key_value_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
  [ping_test,
   key_value_test].

init_per_suite(Config) ->
    Host = "127.0.0.1",
    Node1 = start_node('node1', Host, 8198, 8199),
    Node2 = start_node('node2', Host, 8298, 8299),
    Node3 = start_node('node3', Host, 8398, 8399),

    build_cluster(Node1, Node2, Node3),

    [{node1, Node1},
     {node2, Node2},
     {node3, Node3} | Config].

end_per_suite(_) -> ok.

ping_test(Config) ->
  Node1 = ?config(node1, Config),
  Node2 = ?config(node2, Config),
  Node3 = ?config(node3, Config),

 {pong, _Partition1} = rc_command(Node1, ping),
 {pong, _Partition2} = rc_command(Node2, ping),
 {pong, _Partition3} = rc_command(Node3, ping),

 ok.

key_value_test(Config) ->
 Node1 = ?config(node1, Config),
 Node2 = ?config(node2, Config),
 Node3 = ?config(node3, Config),

 ok = rc_command(Node1, put, [k1, v1]),
 ok = rc_command(Node1, put, [k2, v2]),
 ok = rc_command(Node1, put, [k3, v3]),

 %% get from any of the nodes
 v1 = rc_command(Node1, get, [k1]),
 v2 = rc_command(Node1, get, [k2]),
 v3 = rc_command(Node1, get, [k3]),
 not_found = rc_command(Node1, get, [k10]),

 v1 = rc_command(Node2, get, [k1]),
 v2 = rc_command(Node2, get, [k2]),
 v3 = rc_command(Node2, get, [k3]),
 not_found = rc_command(Node2, get, [k10]),

 v1 = rc_command(Node3, get, [k1]),
 v2 = rc_command(Node3, get, [k2]),
 v3 = rc_command(Node3, get, [k3]),
 not_found = rc_command(Node3, get, [k10]),

 %% test reset and delete
 ok = rc_command(Node1, put, [k1, v_new]),
 v_new = rc_command(Node1, get, [k1]),

 v_new = rc_command(Node1, delete, [k1]),
 not_found = rc_command(Node1, get, [k1]),

 ok = rc_command(Node1, put, [k1, v_new]),
 v_new = rc_command(Node1, get, [k1]),

 ok.

start_node(Name, Host, WebPort, HandoffPort) ->
    %% Need to set the code path so the same modules are available in the slave
    CodePath = code:get_path(),
    %% Arguments to set up the node
    NodeArgs = #{name => Name, host => Host, args => ["-pa" | CodePath]},
    %% Since OTP 25, ct_slaves nodes are deprecated
    %% (and to be removed in OTP 27), so we're
    %% using peer nodes instead, with the CT_PEER macro.
    {ok, Peer, Node} = ?CT_PEER(NodeArgs),
    unlink(Peer),
    DataDir = "./data/" ++ atom_to_list(Name),

    %% set the required environment for riak core
    ok = rpc:call(Node, application, load, [riak_core]),
    ok = rpc:call(Node, application, set_env, [riak_core, ring_state_dir, DataDir]),
    ok = rpc:call(Node, application, set_env, [riak_core, platform_data_dir, DataDir]),
    ok = rpc:call(Node, application, set_env, [riak_core, web_port, WebPort]),
    ok = rpc:call(Node, application, set_env, [riak_core, handoff_port, HandoffPort]),
    ok = rpc:call(Node, application, set_env, [riak_core, schema_dirs, ["../../lib/rc_example/priv"]]),

    %% start the rc_example app
    {ok, _} = rpc:call(Node, application, ensure_all_started, [rc_example]),

    Node.

build_cluster(Node1, Node2, Node3) ->
  rpc:call(Node2, riak_core, join, [Node1]),
  rpc:call(Node3, riak_core, join, [Node1]),
  ok.

rc_command(Node, Command) ->
  rc_command(Node, Command, []).
rc_command(Node, Command, Arguments) ->
  rpc:call(Node, rc_example, Command, Arguments).
