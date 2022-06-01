-module(rc_example_vnode).
-behaviour(riak_core_vnode).

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #{partition => Partition, data => #{}}}.

handle_command({put, Key, Value}, _Sender, State = #{data := Data}) ->
  log("PUT ~p:~p", [Key, Value], State),
  NewData = Data#{Key => Value},
  {reply, ok, State#{data => NewData}};

handle_command({get, Key}, _Sender, State = #{data := Data}) ->
  log("GET ~p", [Key], State),
  {reply, maps:get(Key, Data, not_found), State};

handle_command({delete, Key}, _Sender, State = #{data := Data}) ->
  log("DELETE ~p", [Key], State),
  NewData = maps:remove(Key, Data),
  {reply, maps:get(Key, Data, not_found), State#{data => NewData}};

handle_command(ping, _Sender, State = #{partition := Partition}) ->
  log("Received ping command ~p", [Partition], State),
  {reply, {pong, Partition}, State};

handle_command(Message, _Sender, State) ->
    log("unhandled_command ~p", [Message], State),
    {noreply, State}.
handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(keys, _KeySpaces, {_, ReqId, _}, State = #{data := Data}) ->
  log("Received keys coverage", State),
  Keys = maps:keys(Data),
  {reply, {ReqId, Keys}, State};

handle_coverage(values, _KeySpaces, {_, ReqId, _}, State = #{data := Data}) ->
  log("Received values coverage", State),
  Values = maps:values(Data),
  {reply, {ReqId, Values}, State};

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% internal

%% same as logger:info but prepends the partition
log(String, State) ->
  log(String, [], State).

log(String, Args, #{partition := Partition}) ->
  String2 = "[~.36B] " ++ String,
  Args2 = [Partition | Args],
  logger:info(String2, Args2),
  ok.
