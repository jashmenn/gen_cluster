-module(eunit_example_cluster_srv).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, Node1Pid} = example_cluster_srv:start_named(node1),
    application:set_env(gen_cluster, servers, Node1Pid),
    {ok, Node2Pid} = example_cluster_srv:start_named(node2),
    {ok, Node3Pid} = example_cluster_srv:start_named(node3),
    [Node1Pid, Node2Pid, Node3Pid].

teardown(Servers) ->
    io:format(user, "teardown: once~n", []),
    [gen_cluster:cast(Pid, stop)  || Pid <- Servers],
    ok.

node_state_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         ?assert(true =:= true),
         {ok, State1} = gen_server:call(node1, {state}),
         % ?assert(is_record(State1, state) =:= true),
         % ?assertEqual(testnode1, gen_server:call(testnode1, {registered_name})),
         {ok}
      end
  }.

node_join_test_() ->
  {
      setup, fun setup/0,
      fun () ->
         ?assert(true =:= true),
         {ok}
      end
  }.

% node_leave_test_() ->
%   {
%       setup, fun setup/0,
%       fun () ->
%          ?assert(true =:= true),
%          {ok}
%       end
%   }.

