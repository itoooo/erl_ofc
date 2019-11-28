%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erl_ofc_base_ofc).

-behaviour(gen_server).

%% API
-export([start_link/1,
  socket_opened/1,
  handle_msg/2,
  send_msg/2,
  send_msg_sync/2,
  send_msg_surely/2,
  barrier/1,
  barrier_sync/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3,
  format_status/2
]).


-include("erl_ofc.hrl").

-define(SERVER, ?MODULE).

-record(state, {
  driver,
  main_controller,
  barrier_waiter = #{},
  xid_waiter = #{},
  error_waiter = #{}
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(DriverPid) ->
  gen_server:start_link(?MODULE, [DriverPid], []).

socket_opened(Pid) ->
  gen_server:cast(Pid, send_hello).

handle_msg(Pid, Msg) ->
  gen_server:cast(Pid, {ofp_message, Msg}).

send_msg(Pid, Msg) ->
  gen_server:call(Pid, {send_msg, Msg}).

send_msg_sync(Pid, Msg) ->
  gen_server:call(Pid, {send_msg_sync, Msg}).

send_msg_surely(Pid, Msg) ->
  gen_server:call(Pid, {send_msg_surely, Msg}).

barrier(Pid) ->
  gen_server:cast(Pid, {barrier_request}).

barrier_sync(Pid) ->
  gen_server:call(Pid, {barrier_request}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([DriverPid]) ->
  {ok, #state{driver = DriverPid}}.

handle_call({barrier_request}, From, State) ->
  {ok, Xid} = send_barrier_request(State#state.driver),
  BarrierWaiter = (State#state.barrier_waiter)#{Xid => From},
  {noreply, State#state{barrier_waiter = BarrierWaiter}};

handle_call({send_msg, Msg}, _From, State) ->
  XidMsg = check_xid(Msg),
  {ok, Xid} = erl_ofc_sock_acceptor:send_msg(State#state.driver, XidMsg),
  {reply, {ok, Xid}, State};

handle_call({send_msg_sync, Msg}, From, State) ->
  XidMsg = check_xid(Msg),
  {ok, Xid} = erl_ofc_sock_acceptor:send_msg(State#state.driver, XidMsg),
  XidWaiter = State#state.xid_waiter,
  {noreply, State#state{xid_waiter = XidWaiter#{Xid => From}}};

handle_call({send_msg_surely, Msg}, From, State) ->
  XidMsg = check_xid(Msg),
  {ok, MsgXid} = erl_ofc_sock_acceptor:send_msg(State#state.driver, XidMsg),
  {ok, BarrierXid} = send_barrier_request(State#state.driver),
  BarrierWaiter = (State#state.barrier_waiter)#{BarrierXid => {From, MsgXid}},
  ErrorWaiter = (State#state.error_waiter)#{MsgXid => {From, BarrierXid}},
  {noreply, State#state{barrier_waiter = BarrierWaiter,
                        error_waiter = ErrorWaiter}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(send_hello, State) ->
  Msg = #ofp_message {
    type = ?OFPT_HELLO,
    body = #ofp_hello{}
  },
  erl_ofc_sock_acceptor:send_msg(State#state.driver, check_xid(Msg)),
  {noreply, State};

handle_cast({barrier_request}, State) ->
  send_barrier_request(State#state.driver),
  {noreply, State};

handle_cast({ofp_message, Msg}, State) ->
  XidWaiter = State#state.xid_waiter,
  Xid = Msg#ofp_message.xid,
  case XidWaiter of
    #{Xid := From} -> % handle synchronize request
      gen_server:reply(From, {ok, Msg}),
      NewXidWaiter = maps:remove(Msg#ofp_message.xid, XidWaiter),
      {noreply, State#state{xid_waiter = NewXidWaiter}};
    _ ->
      case Msg#ofp_message.type of
        ?OFPT_HELLO ->
          erl_ofc_sock_acceptor:send_msg(State#state.driver,
            check_xid(#ofp_message{type=?OFPT_FEATURES_REQUEST})),
          {noreply, State};
        ?OFPT_ERROR ->
          case State#state.error_waiter of
            #{Xid := {From, BarrierXid}} ->
              % exists a send_msg_surely called process
              gen_server:reply(From, {error, Msg}),
              NewErrorWaiter = maps:remove(Xid, State#state.error_waiter),
              NewBarrierWaiter = maps:remove(BarrierXid,
                                             State#state.barrier_waiter),
              {noreply, State#state{error_waiter = NewErrorWaiter,
                                    barrier_waiter = NewBarrierWaiter}};
            _ ->
              erl_ofc_controller_server:cast(self(), Msg),
              {noreply, State}
          end;
        ?OFPT_ECHO_REQUEST ->
          EchoReply = #ofp_message {
            type = ?OFPT_ECHO_REPLY,
            xid = Msg#ofp_message.xid,
            body = Msg#ofp_message.body
          },
          erl_ofc_sock_acceptor:send_msg(State#state.driver, EchoReply),
          {noreply, State};
        ?OFPT_BARRIER_REPLY ->
          BarrierWaiter = State#state.barrier_waiter,
          case BarrierWaiter of
            #{Xid := {From, MsgXid}} ->
              gen_server:reply(From, ok),
              NewErrorWaiter = maps:remove(MsgXid, State#state.error_waiter),
              NewBarrierWaiter = maps:remove(Xid, BarrierWaiter),
              {noreply, State#state{barrier_waiter = NewBarrierWaiter,
                                    error_waiter = NewErrorWaiter}};
            #{Xid := From} ->
              gen_server:reply(From, ok),
              NewBarrierWaiter = maps:remove(Xid, BarrierWaiter),
              {noreply, State#state{barrier_waiter = NewBarrierWaiter}};
            _ ->
              erl_ofc_controller_server:cast(self(), Msg),
              {noreply, State}
          end;
        _ ->
          erl_ofc_controller_server:cast(self(), Msg),
          {noreply, State}
      end
  end;

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_status(_Opt, _StatusData) ->
  erlang:error(not_implemented).

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_barrier_request(Datapath) ->
  Msg = #ofp_message{
    type = ?OFPT_BARRIER_REQUEST
  },
  erl_ofc_sock_acceptor:send_msg(Datapath, check_xid(Msg)).

check_xid(Msg) ->
  case Msg#ofp_message.xid of
    Num when is_number(Num) ->
      Msg;
    _ ->
      Msg#ofp_message{xid = erl_ofc_of13_encoder:generate_xid()}
  end.
