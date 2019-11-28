%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erl_ofc_sock_acceptor).

-behaviour(gen_server).

%% API
-export([start_link/2, send_msg/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("erl_ofc.hrl").

-define(SERVER, ?MODULE).

-record(state, {listen_socket,
  accept_socket,
  ofc,
  incomplete_msg = <<>>, sup_pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Socket, SupPid) ->
  gen_server:start_link(?MODULE, [Socket, SupPid], []).

%%--------------------------------------------------------------------
%% @doc
%% Send OpenFlow Message
%%
%% Msg - ofp_massage record
%%
%% @end
%%--------------------------------------------------------------------
send_msg(Pid, Msg) ->
  gen_server:call(Pid, {send_msg, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Socket, SupPid]) ->
  gen_server:cast(self(), accept),
  {ok, #state{listen_socket=Socket, sup_pid=SupPid}}.

handle_call({send_msg, Msg}, _From, State) ->
  try
    MsgBin = erl_ofc_of13_encoder:encode(Msg),
    Socket = State#state.accept_socket,
    gen_tcp:send(Socket, MsgBin),
    {reply, {ok, Msg#ofp_message.xid}, State}
  catch
    E ->
      error_logger:error_info(E),
      {reply, error, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(accept, State = #state{listen_socket=ListenSocket}) ->
  error_logger:info_msg("socket accept start"),
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  error_logger:info_msg("socket accepted: ~p", [AcceptSocket]),
  {ok, OfcPid} = erl_ofc_sock_acceptor_sup:start_base_controller(State#state.sup_pid,
                                                      self()),
  error_logger:info_msg("ofc started: ~p", [OfcPid]),
  erl_ofc_of_channel_sup:start_socket(), % create new empty listener
  erl_ofc_base_ofc:socket_opened(OfcPid),
  error_logger:info_msg("notified socket opend"),
  {noreply, State#state{accept_socket=AcceptSocket, ofc=OfcPid}};

handle_cast({received, Msg}, State) ->
  case erl_ofc_of13_parser:parse_ofp_packet(Msg) of
    {ok, OFPMessage, <<>>} ->
      erl_ofc_base_ofc:handle_msg(State#state.ofc, OFPMessage),
      {noreply, State#state{incomplete_msg = <<>>}};
    {ok, OFPMessage, RestMsg} ->
      erl_ofc_base_ofc:handle_msg(State#state.ofc, OFPMessage),
      gen_server:cast(self(), {received, RestMsg}),
      {noreply, State#state{incomplete_msg = <<>>}};
    {error, Msg} ->
      {noreply, State#state{incomplete_msg = Msg}}
  end;

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({tcp, _Socket, Msg}, State) ->
  gen_server:cast(self(), {received, <<(State#state.incomplete_msg)/binary, Msg/binary>>}),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
