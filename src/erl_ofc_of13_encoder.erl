-module(erl_ofc_of13_encoder).

-include("erl_ofc.hrl").

-define(XID_MAX, (math:pow(2, 32)-1)).
-define(OFPHET_VERSIONBITMAP, 1).
-define(PAD, 0).
-define(INSTRUCTION_ACTIONS_HEADER_LEN, 8).
-define(INSTRUCTION_GOTO_TABLE_HEADER_LEN, 8).

%% API
-export([
  encode/1
]).

-export([
  generate_xid/0
]).

-define(VERSION, 4).

%%--------------------------------------------------------------------
%% @doc
%% Encode OpenFlow Message Record to Binary
%%
%% @end
%%--------------------------------------------------------------------
encode(#ofp_message{type = Type, xid = Xid, body = Body}) ->
  ofp_message(Type, Xid, encode_body(Type, Body)).

encode_body(?OFPT_HELLO, _Body) ->
  <<?OFPHET_VERSIONBITMAP:16, (4+4):16, 10#16:32>>;
encode_body(?OFPT_ERROR, Body) ->
  ofp_error_msg(Body);
encode_body(?OFPT_ECHO_REQUEST, Body) ->
  Body;
encode_body(?OFPT_ECHO_REPLY, Body) ->
  Body;
encode_body(?OFPT_FEATURES_REQUEST, _Body) ->
  <<>>;
encode_body(?OFPT_FEATURES_REPLY, Body) ->
  ofp_features_reply(Body);
encode_body(?OFPT_PACKET_IN, Body) ->
  ofp_packet_in(Body);
encode_body(?OFPT_FLOW_REMOVED, Body) ->
  ofp_flow_removed(Body);
encode_body(?OFPT_PACKET_OUT, Body) ->
  ofp_packet_out(Body);
encode_body(?OFPT_FLOW_MOD, Body) ->
  ofp_flow_mod(Body);
encode_body(?OFPT_GROUP_MOD, Body) ->
  ofp_group_mod(Body);
encode_body(?OFPT_MULTIPART_REQUEST, Body) ->
  ofp_multipart_request(Body);
encode_body(?OFPT_MULTIPART_REPLY, Body) ->
  ofp_multipart_reply(Body);
encode_body(?OFPT_BARRIER_REQUEST, _Body) ->
  <<>>.

ofp_message(Type, Xid, Data) ->
  Length = ?OFP_HEADER_SIZE + byte_size(Data),
  <<?VERSION:8, Type:8, Length:16, Xid:32, Data/binary>>.

ofp_error_msg(#ofp_error_msg{ type = Type,
                              code = Code,
                              data = Data }) ->
  <<Type:16, Code:16, Data/binary>>.

ofp_flow_removed(#ofp_flow_removed{ cookie = Cookie,
                                    priority = Priority,
                                    reason = Reason,
                                    table_id = TableId,
                                    duration_sec = DurationSec,
                                    duration_nsec = DurationNsec,
                                    idle_timeout = IdleTimeout,
                                    hard_timeout = HardTimeout,
                                    packet_count = PacketCount,
                                    byte_count = ByteCount,
                                    match = Match }) ->
  <<Cookie:64, Priority:16, Reason:8, TableId:8,
    DurationSec:32, DurationNsec:32,
    IdleTimeout:16, HardTimeout:16,
    PacketCount:64, ByteCount:64,
    (ofp_match(Match))/binary>>.

ofp_features_reply(#ofp_switch_features{
                      datapath_id = DatapathId,
                      n_buffers = NBuffers,
                      n_tables = NTables,
                      auxiliary_id = AuxiliaryId,
                      capabilities = Capabilities
                     }) ->
  <<DatapathId:64, NBuffers:32, NTables:8, AuxiliaryId:8, ?PAD:16,
    Capabilities:32, 0:32>>.

ofp_multipart_request(#ofp_multipart_request{type=Type,
                                             flags = Flags,
                                             body = Body}) ->
  ofp_multipart_request(Type, Flags, ofp_multipart_body(Body)).

ofp_multipart_request(MPType, Flags, Body) ->
  <<MPType:16, Flags:16, ?PAD:32, Body/binary>>.

ofp_multipart_body(undefined) ->
  <<>>;
ofp_multipart_body([]) ->
  <<>>;
ofp_multipart_body([H|T]) ->
  <<(ofp_multipart_body(H))/binary, (ofp_multipart_body(T))/binary>>;
ofp_multipart_body(Bin) when is_binary(Bin) ->
  Bin;
ofp_multipart_body(#ofp_flow_stats_request{
  table_id = TableId,
  out_port = OutPort,
  out_group = OutGroup,
  cookie = Cookie,
  cookie_mask = CookieMask,
  match = Match
}) ->
  <<TableId:8, ?PAD:24, OutPort:32, OutGroup:32, ?PAD:32,
    Cookie:64, CookieMask:64,
    (ofp_match(Match))/binary>>;
ofp_multipart_body(#ofp_group_stats_request{group_id = GroupId}) ->
  <<GroupId:32, ?PAD:32>>;
ofp_multipart_body(#ofp_port{ port_no = PortNo,
                              hw_addr = HwAddr,
                              name = Name,
                              config = Config,
                              state = State,
                              curr = Curr,
                              advertised = Advertised,
                              supported = Supported,
                              peer = Peer,
                              curr_speed = CurrSpeed,
                              max_speed = MaxSpeed }) ->
  <<PortNo:32, ?PAD:32, (mac_addr(HwAddr))/binary, ?PAD:16,
    (list_to_binary(Name))/binary, Config:32, State:32,
    Curr:32, Advertised:32, Supported:32, Peer:32,
    CurrSpeed:32, MaxSpeed:32>>;
ofp_multipart_body(#ofp_flow_stats{ table_id = TableId,
                                    duration_sec = DurationSec,
                                    duration_nsec = DurationNsec,
                                    priority = Priority,
                                    idle_timeout = IdleTimeout,
                                    hard_timeout = HardTimeout,
                                    flags = Flags,
                                    cookie = Cookie,
                                    packet_count = PacketCount,
                                    byte_count = ByteCount,
                                    match = Match,
                                    instructions = Instructions
                                  }) ->
  MatchBin = ofp_match(Match),
  InstBin = ofp_instructions(Instructions),
  Length = 48 + byte_size(MatchBin) + byte_size(InstBin),
  <<Length:16, TableId:8, ?PAD:8, DurationSec:32,
    DurationNsec:32, Priority:16, IdleTimeout:16,
    HardTimeout:16, Flags:16, ?PAD:32,
    Cookie:64,
    PacketCount:64,
    ByteCount:64,
    MatchBin/binary,
    InstBin/binary>>.

ofp_multipart_reply(#ofp_multipart_reply{ type = Type,
                                          flags = Flags,
                                          body = Body}) ->
  <<Type:16, Flags:16, ?PAD:32, (ofp_multipart_body(Body))/binary>>.


ofp_flow_mod(#ofp_flow_mod{ cookie = Cookie,
                            cookie_mask = CookieMask,
                            table_id = TableId,
                            command = Command,
                            idle_timeout = IdleTimeout,
                            hard_timeout = HardTimeout,
                            priority = Priority,
                            buffer_id = BufferId,
                            out_port = OutPort,
                            out_group = OutGroup,
                            flags = Flags,
                            match = Match,
                            instructions = Instructions
}) ->
  << Cookie:64,
     CookieMask:64,
     TableId:8,
     Command:8,
     IdleTimeout:16,
     HardTimeout:16,
     Priority:16,
     BufferId:32,
     OutPort:32,
     OutGroup:32,
     Flags:16,
     0:16, % Pad
     (ofp_match(Match))/binary,
     (ofp_instructions(Instructions))/binary
  >>.

ofp_match(#ofp_match { type = Type,
                       oxm_fields = OXMFields}) ->
  OXMFieldsBin = oxm_fields(OXMFields),
  OXMFieldsSize = byte_size(OXMFieldsBin),
  MatchStructSize = 4 + OXMFieldsSize,
  PadSize = erl_ofc_of13_util:pad_size(MatchStructSize),
  <<Type:16, MatchStructSize:16, OXMFieldsBin/binary, 0:(PadSize*8)>>.

oxm_fields(Fields) ->
  oxm_fields(Fields, <<>>).

oxm_fields([], Acc) ->
  Acc;
oxm_fields([H|T], Acc) ->
  oxm_fields(T, <<(oxm_field(H))/binary, Acc/binary>>).

oxm_field(#oxm_field{oxm_class = Class,
                     oxm_field = Field,
                     oxm_has_mask = HasMask,
                     oxm_value = Value}) ->
  Payload = oxm_value(Field, Value),
  <<Class:16,
    Field:7,
    HasMask:1,
    (byte_size(Payload)):8,
    Payload/binary>>.

oxm_value(?OFPXMT_OFB_IN_PORT, Value) -> <<Value:32>>;
oxm_value(?OFPXMT_OFB_IN_PHY_PORT, Value) -> <<Value:32>>;
oxm_value(?OFPXMT_OFB_METADATA, Value) -> <<Value:64>>;
oxm_value(?OFPXMT_OFB_ETH_DST, Value) -> mac_addr(Value);
oxm_value(?OFPXMT_OFB_ETH_SRC, Value) -> mac_addr(Value);
oxm_value(?OFPXMT_OFB_ETH_TYPE, Value) -> <<Value:16>>;
oxm_value(?OFPXMT_OFB_VLAN_VID, Value) -> <<0:3, Value:13>>;
oxm_value(?OFPXMT_OFB_VLAN_PCP, Value) -> <<Value:3>>;
oxm_value(?OFPXMT_OFB_IP_DSCP, Value) -> <<Value:6>>;
oxm_value(?OFPXMT_OFB_IP_ECN, Value) -> <<Value:2>>;
oxm_value(?OFPXMT_OFB_IP_PROTO, Value) -> <<Value:8>>;
oxm_value(?OFPXMT_OFB_IPV4_SRC, Value) -> ip_addr(Value);
oxm_value(?OFPXMT_OFB_IPV4_DST, Value) -> ip_addr(Value);
oxm_value(?OFPXMT_OFB_TCP_SRC, Value) -> <<Value:16>>;
oxm_value(?OFPXMT_OFB_TCP_DST, Value) -> <<Value:16>>;
oxm_value(?OFPXMT_OFB_UDP_SRC, Value) -> <<Value:16>>;
oxm_value(?OFPXMT_OFB_UDP_DST, Value) -> <<Value:16>>;
oxm_value(?OFPXMT_OFB_SCTP_SRC, Value) -> <<Value:16>>;
oxm_value(?OFPXMT_OFB_SCTP_DST, Value) -> <<Value:16>>;
oxm_value(?OFPXMT_OFB_ICMPV4_TYPE, Value) -> <<Value:8>>;
oxm_value(?OFPXMT_OFB_ICMPV4_CODE, Value) -> <<Value:8>>;
oxm_value(?OFPXMT_OFB_ARP_OP, Value) -> <<Value:16>>;
oxm_value(?OFPXMT_OFB_ARP_SPA, Value) -> ip_addr(Value);
oxm_value(?OFPXMT_OFB_ARP_TPA, Value) -> ip_addr(Value);
oxm_value(?OFPXMT_OFB_ARP_SHA, Value) -> mac_addr(Value);
oxm_value(?OFPXMT_OFB_ARP_THA, Value) -> mac_addr(Value);
oxm_value(?OFPXMT_OFB_IPV6_SRC, Value) -> ipv6_addr(Value);
oxm_value(?OFPXMT_OFB_IPV6_DST, Value) -> ipv6_addr(Value);
oxm_value(?OFPXMT_OFB_IPV6_FLABEL, Value) -> <<Value:20>>;
oxm_value(?OFPXMT_OFB_ICMPV6_TYPE, Value) -> <<Value:8>>;
oxm_value(?OFPXMT_OFB_ICMPV6_CODE, Value) -> <<Value:8>>;
oxm_value(?OFPXMT_OFB_IPV6_ND_TARGET, Value) -> ipv6_addr(Value);
oxm_value(?OFPXMT_OFB_IPV6_ND_SLL, Value) -> mac_addr(Value);
oxm_value(?OFPXMT_OFB_IPV6_ND_TLL, Value) -> mac_addr(Value);
oxm_value(?OFPXMT_OFB_MPLS_LABEL, Value) -> <<?PAD:12, Value:20>>;
oxm_value(?OFPXMT_OFB_MPLS_TC, Value) -> <<Value:3>>;
oxm_value(?OFPXMT_OFP_MPLS_BOS, Value) -> <<Value:1>>;
oxm_value(?OFPXMT_OFB_PBB_ISID, Value) -> <<Value:24>>;
oxm_value(?OFPXMT_OFB_IPV6_EXTHDR, Value) -> <<Value:9>>;
oxm_value(_, Value) -> Value. 

mac_addr({B1, B2, B3, B4, B5, B6}) ->
  <<B1, B2, B3, B4, B5, B6>>.

ip_addr({B1, B2, B3, B4}) ->
  <<B1, B2, B3, B4>>.

ipv6_addr({B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16}) ->
  <<B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16>>.

ofp_actions(Actions) ->
  ofp_actions(Actions, <<>>).
ofp_actions([], Acc) ->
  Acc;
ofp_actions([H|T], Acc) ->
  ofp_actions(T, <<Acc/binary, (ofp_action(H))/binary>>).

ofp_action(#ofp_action_output{type=Type,
                              len=Len,
                              port=Port,
                              max_len=MaxLen}) ->
  <<Type:16, Len:16, Port:32, MaxLen:16, ?PAD:48>>;
ofp_action(#ofp_action_group{type=Type,
                             len=Len,
                             group_id=GroupId}) ->
  <<Type:16, Len:16, GroupId:32>>;
ofp_action(#ofp_action_set_queue{type=Type,
                                 len=Len,
                                 queue_id=QueueId}) ->
  <<Type:16, Len:16, QueueId:32>>;
ofp_action(#ofp_action_mpls_ttl{type=Type,
                                len=Len,
                                mpls_ttl=MplsTtl}) ->
  <<Type:16, Len:16, MplsTtl:8, ?PAD:24>>;
ofp_action(#ofp_action_nw_ttl{type=Type,
                              len=Len,
                              nw_ttl=NwTtl}) ->
  <<Type:16, Len:16, NwTtl:8, ?PAD:24>>;
ofp_action(#ofp_action_push_vlan{type=Type,
                                 len=Len,
                                 ethertype=Ethertype}) ->
  <<Type:16, Len:16, Ethertype:16, ?PAD:16>>;
ofp_action(#ofp_action_push_mpls{type=Type,
                                 len=Len,
                                 ethertype=Ethertype}) ->
  <<Type:16, Len:16, Ethertype:16, ?PAD:16>>;
ofp_action(#ofp_action_push_pbb{type=Type,
                                len=Len,
                                ethertype=Ethertype}) ->
  <<Type:16, Len:16, Ethertype:16, ?PAD:16>>;
ofp_action(#ofp_action_pop_vlan{type=Type,
                                len=Len}) ->
  <<Type:16, Len:16, ?PAD:32>>;
ofp_action(#ofp_action_pop_mpls{type=Type,
                                len=Len,
                                ethertype=Ethertype}) ->
  <<Type:16, Len:16, Ethertype:16, ?PAD:16>>;
ofp_action(#ofp_action_set_field{type=Type,
                                 field=Field}) ->
  FieldBin = oxm_field(Field),
  FieldLen = byte_size(FieldBin),
  PadSize = erl_ofc_of13_util:pad_size(4 + FieldLen),
  <<Type:16, (4 + FieldLen + PadSize):16, FieldBin/binary, ?PAD:(PadSize*8)>>;
ofp_action(#ofp_action_encap{type=Type,
                             packet_type=PacketType,
                             props=Props}) ->
  Len = 8 + byte_size(Props),
  <<Type:16, Len:16, PacketType:32, Props/binary>>;
ofp_action(#ofp_action_decap{type=Type,
                             cur_pkt_type=CurPktType,
                             new_pkt_type=NewPktType,
                             props=Props}) ->
  Len = 16 + byte_size(Props),
  <<Type:16, Len:16, CurPktType:32, NewPktType:32, ?PAD:32, Props/binary>>;
ofp_action(#ofp_action_experimenter{type=Type,
                                    experimenter=ExpId,
                                    data=Data}) ->
  <<Type:16, (8+byte_size(Data)):16, ExpId:32, Data/binary>>;
ofp_action(#ofp_action{type=Type, body=Body}) ->
  <<Type:16, (4+byte_size(Body)):16, Body/binary>>.

ofp_instructions(Instructions) ->
  ofp_instructions(Instructions, <<>>).
ofp_instructions([], Acc) ->
  Acc;
ofp_instructions([H|T], Acc) ->
  ofp_instructions(T, <<(ofp_instruction(H))/binary, Acc/binary>>).

ofp_instruction(#ofp_instruction_goto_table{type = Type,
                                            table_id = TableId}) ->
  <<Type:16,
    ?INSTRUCTION_GOTO_TABLE_HEADER_LEN:16,
    TableId:8,
    ?PAD:24>>;

ofp_instruction(#ofp_instruction_write_actions{type = Type,
                                               actions = Actions}) ->
  ActionsBin = ofp_actions(Actions),
  <<Type:16,
    (byte_size(ActionsBin) + ?INSTRUCTION_ACTIONS_HEADER_LEN):16,
    ?PAD:32,
    ActionsBin/binary>>;

ofp_instruction(#ofp_instruction_apply_actions{type = Type,
                                               actions = Actions}) ->
  ActionsBin = ofp_actions(Actions),
  <<Type:16,
    (byte_size(ActionsBin) + ?INSTRUCTION_ACTIONS_HEADER_LEN):16,
    ?PAD:32,
    ActionsBin/binary>>.

ofp_group_mod(#ofp_group_mod{
  command = Command,
  type = Type,
  group_id = GroupId,
  buckets = Buckets
}) ->
  <<Command:16, Type:8, ?PAD:8, GroupId:32, (ofp_buckets(Buckets))/binary>>.

ofp_buckets([]) ->
  <<>>;
ofp_buckets(Buckets) ->
  << <<(ofp_bucket(X))/binary>> || X <- Buckets >>.

ofp_bucket(#ofp_bucket{
  weight = Weight,
  watch_port = WatchPort,
  watch_group = WatchGroup,
  actions = Actions
}) ->
  ActionsBin = ofp_actions(Actions),
  Len = byte_size(ActionsBin) + 16,
  <<Len:16, Weight:16, WatchPort:32, WatchGroup:32, ?PAD:32,
    ActionsBin/binary>>.

ofp_packet_in(#ofp_packet_in{ buffer_id = BufferId,
                              total_len = TotalLen,
                              reason = Reason,
                              table_id = TableId,
                              cookie = Cookie,
                              match = Match,
                              data = Data }) ->
  case byte_size(Data) of
    0 ->
      <<BufferId:32, TotalLen:16, Reason:8, TableId:8, Cookie:64,
        (ofp_match(Match))/binary>>;
    _ ->
      <<BufferId:32, TotalLen:16, Reason:8, TableId:8, Cookie:64,
        (ofp_match(Match))/binary, ?PAD:16, Data/binary>>
  end.

ofp_packet_out(#ofp_packet_out{
  buffer_id = BufferId,
  in_port = InPort,
  actions = Actions,
  data = Data
}) ->
  ActionsBin = ofp_actions(Actions),
  <<BufferId:32, InPort:32,
  (byte_size(ActionsBin)):16, ?PAD:48,
  ActionsBin/binary,
  Data/binary
  >>.

generate_xid() ->
  trunc(rand:uniform() * ?XID_MAX).
