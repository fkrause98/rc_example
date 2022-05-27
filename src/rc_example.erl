-module(rc_example).

-export([ping/0,
         ring_status/0,
         put/2,
         get/1,
         delete/1]).

ping() ->
  sync_command(os:timestamp(), ping).

ring_status() ->
  {ok, Ring} = riak_core_ring_manager:get_my_ring(),
  riak_core_ring:pretty_print(Ring, [legend]).

put(Key, Value) ->
  sync_command(Key, {put, Key, Value}).

get(Key) ->
  sync_command(Key, {get, Key}).

delete(Key) ->
  sync_command(Key, {delete, Key}).

%% internal
hash_key(Key) ->
  riak_core_util:chash_key({<<"rc_example">>, term_to_binary(Key)}).

sync_command(Key, Command) ->
  DocIdx = hash_key(Key),
  PrefList = riak_core_apl:get_apl(DocIdx, 1, rc_example),
  [IndexNode] = PrefList,
  io:format("Index Node: ~p", [IndexNode]),
  riak_core_vnode_master:sync_spawn_command(IndexNode, Command, rc_example_vnode_master).
