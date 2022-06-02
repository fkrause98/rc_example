-module(rc_example_vnode).
-include_lib("riak_core/include/riak_core_vnode.hrl").
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

handle_handoff_command(?FOLD_REQ{foldfun=FoldFun, acc0=Acc0}, _Sender,
                       State = #{data := Data}) ->
    log("Received fold request for handoff", State),
    Result = maps:fold(FoldFun, Acc0, Data),
    {reply, Result, State};
handle_handoff_command({get, Key}, Sender, State) ->
    log("GET during handoff, handling locally ~p", [Key], State),
    handle_command({get, Key}, Sender, State);

handle_handoff_command(Message, Sender, State) ->
    {reply, _Result, NewState} = handle_command(Message, Sender, State),
    {forward, NewState}.
handoff_starting(_TargetNode, State) ->
  log("starting handoff", State),
  {true, State}.

handoff_cancelled(State) ->
    log("handoff cancelled", State),
    {ok, State}.

handoff_finished(_TargetNode, State) ->
  log("finished handoff", State),
  {ok, State}.

delete(State) ->
  log("deleting the vnode", State),
  {ok, State#{data => #{}}}.

encode_handoff_item(Key, Value) ->
  erlang:term_to_binary({Key, Value}).

handle_handoff_data(BinData, State = #{data := Data}) ->
  {Key, Value} = erlang:binary_to_term(BinData),
  log("received handoff data ~p", [{Key, Value}], State),
  NewData = Data#{Key => Value},
  {reply, ok, State#{data => NewData}}.

is_empty(State = #{data := Data}) ->
  IsEmpty = maps:size(Data) == 0,
  {IsEmpty, State}.

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
