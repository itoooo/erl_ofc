-module(erl_ofc_of_channel).

-behaviour(gen_server).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3,
  start_link/2,
  send_ofp_message/2]).

-record(ofp_hello_elem, {type, length, value}).
-record(state, {socket,
  listen_socket,
  accept_socket,
  event_manager,
  datapath,
  incomplete_msg = <<>>
}).

-include("erl_ofc.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket, EventManagerPid) ->
  gen_server:start_link(?MODULE, [Socket, EventManagerPid], []).

%% Send OpenFlow Message to a Datapath.
%%   Pid - OFP Channel Pid
%%   Msg - Message Binary Data
send_ofp_message(Pid, Msg) ->
  error_logger:info_msg("[erl_ofc_of_channel] send_ofp_message Pid:~p, Msg:~w", [Pid, Msg]),
  gen_server:cast(Pid, {send, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Socket, EventManagerPid]) ->
  process_flag(trap_exit, true),
  gen_server:cast(self(), accept),
  {ok, #state{listen_socket=Socket, event_manager=EventManagerPid}}.

handle_call(_Request, _From, _State) ->
  erlang:error(not_implemented).

%%%===================================================================
%%% handle_cast
%%%===================================================================

handle_cast(accept, State = #state{listen_socket=ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  erl_ofc_of_channel_sup:start_socket(),
  send_ofp_hello(AcceptSocket),
  {noreply, State#state{accept_socket=AcceptSocket}};

handle_cast({send, Msg}, State) ->
  Socket = State#state.accept_socket,
  gen_tcp:send(Socket, Msg),
  {noreply, State};

handle_cast({received, Msg}, State) ->
  case erl_ofc_of13_parser:parse_ofp_packet(Msg) of
    {ok, OFPPacket, <<>>} ->
      gen_server:cast(self(), {OFPPacket#ofp_message.type, OFPPacket}),
      {noreply, State#state{incomplete_msg = <<>>}};
    {ok, OFPPacket, RestMsg} ->
      gen_server:cast(self(), {OFPPacket#ofp_message.type, OFPPacket}),
      gen_server:cast(self(), {received, RestMsg}),
      {noreply, State#state{incomplete_msg = <<>>}};
    {error, Msg} ->
      {noreply, State#state{incomplete_msg = Msg}}
  end;

handle_cast(close, State) ->
  error_logger:info_msg("closing socket, ~w\n", [State]),
  gen_tcp:close(State#state.socket),
  {noreply, State#state{accept_socket=undefined}};

handle_cast({'OFPT_HELLO', OFPPacket}, State) ->
  error_logger:info_msg("received OFPT_HELLO"),
  % parse ofp_hello_elem_header
  Headers = parse_ofp_hello_elem_header(OFPPacket#ofp_message.body, []),
  error_logger:info_report(Headers),
  send_ofp_feature_request(State#state.accept_socket),
  {noreply, State};

handle_cast({'OFPT_ECHO_REQUEST', OFPPacket}, State) ->
  error_logger:info_msg("received OFPT_ECHO_REQUEST"),
  OFPEchoReply = erl_ofc_of13_encoder:ofp_echo_reply(OFPPacket#ofp_message.xid,
                                                     OFPPacket#ofp_message.body),
  send_ofp_message(self(), OFPEchoReply),
  {noreply, State};

handle_cast({'OFPT_FEATURES_REPLY', OFPPacket}, State) ->
  Features = erl_ofc_of13_parser:parse_ofp_switch_features_reply(
    OFPPacket#ofp_message.body),
  DatapathId = Features#ofp_switch_features.datapath_id,
  error_logger:info_msg("~p Datapath ~p connected.", [self(), DatapathId]),
  erl_ofc_event_manager:notify_ofp_message(
    State#state.event_manager,
    self(),
    DatapathId,
    'OFPT_FEATURES_REPLY',
    Features),
  {noreply, State#state{datapath=DatapathId}};

handle_cast({OFPPacketType, OFPPacket}, State) ->
  erl_ofc_event_manager:notify_ofp_message(
    State#state.event_manager,
    self(),
    State#state.datapath,
    OFPPacketType,
    OFPPacket#ofp_message.body),
  {noreply, State}.


handle_info({tcp, _Socket, Msg}, State) ->
  gen_server:cast(self(), {received, <<(State#state.incomplete_msg)/binary, Msg/binary>>}),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, _State, _Extra) ->
  erlang:error(not_implemented).

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_ofp_hello_elem_header(<<>>, Headers) ->
  lists:reverse(Headers);
parse_ofp_hello_elem_header(Bin, Headers) ->
  error_logger:info_msg("parse_ofp_hello_elem_header, ~w", [Bin]),
  <<Type:16, Length:16, Rest/binary>> = Bin,
  OFPHelloElemHeaderSize = 4,
  ValueBit = (Length-OFPHelloElemHeaderSize) * 8,
  <<Value:ValueBit, Rest2/binary>> = Rest,
  HelloElem = #ofp_hello_elem{type = Type, length = Length, value = Value},
  parse_ofp_hello_elem_header(Rest2, [HelloElem | Headers]).

send_ofp_hello(Socket) ->
  gen_tcp:send(Socket, erl_ofc_of13_encoder:ofp_hello()).

send_ofp_feature_request(Socket) ->
  spawn_link(fun() ->
    timer:sleep(3000),
    OFPSwitchFeaturesRequest = erl_ofc_of13_encoder:ofp_switch_features_request(),
    error_logger:info_msg("sent ofp_feature_request"),
    error_logger:info_report(OFPSwitchFeaturesRequest),
    gen_tcp:send(Socket, OFPSwitchFeaturesRequest)
  end).
