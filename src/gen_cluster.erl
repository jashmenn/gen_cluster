%%%-------------------------------------------------------------------
%%% File    : stoplight_srv.erl
%%% Author  : nmurray@attinteractive.com
%%% Description : Cascading gen_server behavior that implements process clustering.  
%%% See: http://wiki.trapexit.org/index.php/Cascading_Behaviours
%%% Created     : 2009-08-03
%%%-------------------------------------------------------------------
-module(gen_cluster).
-include_lib("../include/gen_cluster.hrl").

%% Define this module as a gen_server callback module.
-behaviour(gen_server).

%% Export the same API as gen_server.
-export([start/3, start/4,
    start_link/3, start_link/4,
    call/2, call/3,
    cast/2, reply/2,
    abcast/2, abcast/3,
    multi_call/2, multi_call/3, multi_call/4,
    enter_loop/3, enter_loop/4, enter_loop/5, wake_hib/5]).

%% Export the callbacks that gen_server expects
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Define the behaviour's required callbacks.
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
    % gen_cluster
      {handle_join, 0}, {handle_leave, 0},
    % gen_server      
      {init,1}, {handle_call,3},{handle_cast,2},{handle_info,2}, {terminate,2},{code_change,3}
   ];

behaviour_info(_) ->
    undefined.

%% State data record.
-record(state, {module, state, data, plist}).

%% Users will use these start functions instead of gen_server's.
%% We add the user's module name to the arguments and call
%% gen_fsm's start function with our module name instead.
start(Mod, Args, Options) ->
    gen_server:start(?MODULE, nolink, Mod, Args, Options).
start(Name, Mod, Args, Options) ->
    gen_server:start(?MODULE, nolink, Name, Mod, Args, Options).
start_link(Mod, Args, Options) ->
    gen_server:start(?MODULE, link, Mod, Args, Options).
start_link(Name, Mod, Args, Options) ->
    gen_server:start(?MODULE, link, Name, Mod, Args, Options).

%% Delegate the rest of the reqests to gen_server
call(Name, Request) ->
    gen_server:call(Name, Request).
call(Name, Request, Timeout) ->
   gen_server:call(Name, Request, Timeout).
cast(Name, Request) ->
    gen_server:cast(Name, Request).
reply(To, Reply) ->
    gen_server:reply(To, Reply).
abcast(Name, Request) ->
    gen_server:abcast(Name, Request).
abcast(Nodes, Name, Request) ->
    gen_server:abcast(Nodes, Name, Request).
multi_call(Name, Req) ->
    gen_server:multi_call(Name, Req).
multi_call(Nodes, Name, Req)  ->
    gen_server:multi_call(Nodes, Name, Req).
multi_call(Nodes, Name, Req, Timeout)  ->
    gen_server:multi_call(Nodes, Name, Req, Timeout).
enter_loop(Mod, Options, State) ->
    gen_server:enter_loop(Mod, Options, State).
enter_loop(Mod, Options, State, Timeout) ->
    gen_server:enter_loop(Mod, Options, State, Timeout).
enter_loop(Mod, Options, State, ServerName, Timeout) ->
    gen_server:enter_loop(Mod, Options, State, ServerName, Timeout).
wake_hib(Parent, Name, State, Mod, Debug) ->
    gen_server:wake_hib(Parent, Name, State, Mod, Debug).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%
%% run the user's init/1 and store the user's state data in our internal
%% state data record for later reference.
%%--------------------------------------------------------------------

init([Mod, Args]) ->
    ?TRACE("init being called", foo),
    InitialState = #state{plist=[self()]},
    {ok, State1} = join_existing_cluster(InitialState),
    {_Resp, State2} = start_cluster_if_needed(State1),
 
    case Mod:init(Args) of
        {ok, ExtStateName, ExtStateData} -> 
            StateData = State2#state{module = Mod, state = ExtStateName, data = ExtStateData},
            {ok, state, StateData};
        {ok, ExtStateName, ExtStateData, Timeout} ->
            StateData = State2#state{module = Mod, state = ExtStateName, data = ExtStateData},
            {ok, state, StateData, Timeout};
        {stop, Reason} ->
            {stop, Reason};
        Other ->
            Other % double check this one
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) -> 
    {reply, todo_reply, State}.

% e.g.
% handle_call({create_ring}, _From, State) ->
%     {Reply, NewState} = handle_create_ring(State),
%     {reply, Reply, NewState};
%
% handle_call({join, OtherNode}, _From, State) ->
%     {Reply, NewState} = handle_join(OtherNode, State),
%     {reply, Reply, NewState};
% ...
% etc.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> 
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

foo() ->
    todo.

%% other methods: get_cluster

%%--------------------------------------------------------------------
%% Func: join_existing_cluster(State) -> {ok, NewState} | false
%% Description: Look for any existing servers in the cluster, try to join them
%%--------------------------------------------------------------------
join_existing_cluster(State) ->
    Servers = ?MODULE:known_nodes(State#state.state),
    connect_to_servers(Servers),
    global:sync(), % otherwise we may not see the pid yet
    NewState = State,
    % NewState = case global:whereis_name(?SERVER_GLOBAL) of % join unless we are the main server 
    %     undefined ->
    %         ?TRACE("existing cluster undefined", undefined),
    %         State;
    %     X when X =:= self() ->
    %         ?TRACE("we are the cluster, skipping", X),
    %         State;
    %     _ ->
    %         ?TRACE("joining server...", global:whereis_name(?SERVER_GLOBAL)),
    %         % {ok, KnownRing} = gen_server:call({global, ?SERVER_GLOBAL}, {join, State}),
    %         % {ok, NewInformedState} = add_pids_to_ring(KnownRing, State),
    %         % broadcast_join_announcement(NewInformedState)
    % end,
    {ok, NewState}.

get_cluster() ->
    todo.

connect_to_servers(ServerNames) ->
   ServerRefs = lists:map(fun(Server) ->
      case Server of
      undefined -> 
          ?TRACE("warning, skipping server", Server),
          skip; % do nothing
      _ -> 
         ?TRACE("connecting to server: ", Server),
         {Node, _Pid} = Server,
         pong = net_adm:ping(Node)
      end
    end,
    ServerNames),
   {ok, ServerRefs}.

%%--------------------------------------------------------------------
%% Func: start_cluster_if_needed(State) -> {{ok, yes}, NewState} |
%%                                         {{ok, no}, NewState}
%% Description: Start cluster if we need to
%%--------------------------------------------------------------------
start_cluster_if_needed(State) ->
    global:sync(), % otherwise we may not see the pid yet
    % {Resp, NewState} = case global:whereis_name(?SERVER_GLOBAL) of
    %   undefined ->
    %       start_cluster(State);
    %   _ ->
    %       {no, State}
    % end,
    % {{ok, Resp}, NewState}.
    {ok,ok, State}.

