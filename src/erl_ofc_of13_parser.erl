-module(erl_ofc_of13_parser).

%% API
-export([
  parse_ofp_packet/1,
  parse_ofp_switch_features_reply/1,
  parse_ofp_multipart_reply/1,
  parse_ofp_flow_stats/1,
  parse_ofp_packet_in/1,
  parse_ofp_error_msg/1]).

-include("erl_ofc.hrl").

parse_ofp_packet(Msg) ->
  MsgSize = byte_size(Msg),
  <<Version:8, Type:8, Length:16, Xid:32, Rest/binary>> = Msg,
  BodySize = Length - ?OFP_HEADER_SIZE,
  ParsedMsg = #ofp_message{
      version = Version,
      type = Type,
      length = Length,
      xid = Xid
  },
  if
    Length =:= MsgSize ->
      {ok,
       ParsedMsg#ofp_message{
         body = parse_body(Type, Rest)
        },
       <<>>};
    Length < MsgSize ->
      Body = binary:part(Rest, {0, BodySize}),
      RestMsg = binary:part(Rest, {BodySize, byte_size(Rest) - BodySize}),
      {ok,
       ParsedMsg#ofp_message{
         body = parse_body(Type, Body)
        },
       RestMsg};
    Length > MsgSize ->
      {
       error,
       Msg
      }
  end.

parse_body(?OFPT_HELLO, Bin) ->
  Bin;
parse_body(?OFPT_ERROR, Bin) ->
  parse_ofp_error(Bin);
parse_body(?OFPT_ECHO_REQUEST, Bin) ->
  Bin;
parse_body(?OFPT_ECHO_REPLY, Bin) ->
  Bin;
parse_body(?OFPT_FEATURES_REQUEST, Bin) ->
  Bin;
parse_body(?OFPT_FEATURES_REPLY, Bin) ->
  parse_ofp_switch_features_reply(Bin);
parse_body(?OFPT_PACKET_IN, Bin) ->
  parse_ofp_packet_in(Bin);
parse_body(?OFPT_FLOW_REMOVED, Bin) ->
  parse_ofp_flow_removed(Bin);
parse_body(?OFPT_PACKET_OUT, Bin) ->
  parse_ofp_packet_out(Bin);
parse_body(?OFPT_FLOW_MOD, Bin) ->
  parse_ofp_flow_mod(Bin);
parse_body(?OFPT_MULTIPART_REQUEST, Bin) ->
  parse_ofp_multipart_request(Bin);
parse_body(?OFPT_MULTIPART_REPLY, Bin) ->
  parse_ofp_multipart_reply(Bin);
parse_body(?OFPT_BARRIER_REPLY, Bin) ->
  Bin.

parse_ofp_error(<<Type:16, Code:16, Data/binary>>) ->
  #ofp_error_msg{
    type = Type,
    code = Code,
    data = Data
  }.

parse_ofp_switch_features_reply(Body) ->
  <<
    DatapathId:64,
    NBuffers:32,
    NTables:8,
    AuxiliaryId:8,
    _Pad:16,
    Capabilities:32,
    _Reserved:32, _Rest/binary % todo
  >> = Body,
  #ofp_switch_features {
    datapath_id = DatapathId,
    n_buffers = NBuffers,
    n_tables = NTables,
    auxiliary_id = AuxiliaryId,
    capabilities = Capabilities
  }.

parse_ofp_packet_in(Body) ->
  <<BufferId:32, TotalLen:16, Reason:8, TableId:8,
    Cookie:64,
    VariableData/binary
  >> = Body,
  <<_:16, Length:16, _/binary>> = VariableData, % obtain ofp_match size
  [OFPMatch, RestData] = erl_ofc_of13_util:binary_splitter(VariableData, Length),
  PadSize = erl_ofc_of13_util:pad_size(Length) + 2,
  [_Pad, Data] = erl_ofc_of13_util:binary_splitter(RestData, PadSize),
  #ofp_packet_in{
    buffer_id = BufferId,
    total_len = TotalLen,
    reason = Reason,
    table_id = TableId,
    cookie = Cookie,
    match = parse_ofp_match(OFPMatch),
    data = Data
  }.

parse_ofp_flow_removed(Body) ->
  <<Cookie:64, Priority:16, Reason:8, TableId: 8,
    DurationSec:32, DurationNsec:32,
    IdleTimeout:16, HardTimeout:16,
    PacketCount:64, ByteCount:64,
    VariableData/binary>> = Body,
  #ofp_flow_removed{
     cookie = Cookie,
     priority = Priority,
     reason = Reason,
     table_id = TableId,
     duration_sec = DurationSec,
     duration_nsec = DurationNsec,
     idle_timeout = IdleTimeout,
     hard_timeout = HardTimeout,
     packet_count = PacketCount,
     byte_count = ByteCount,
     match = parse_ofp_match(VariableData)
  }.

parse_ofp_packet_out(Body) ->
  <<BufferId:32, InPort:32, ActionsLen:16, _Pad:48,
    VariableData/binary>> = Body,
  [Actions, RestData] = erl_ofc_of13_util:binary_splitter(VariableData, ActionsLen),
  #ofp_packet_out{
    buffer_id = BufferId,
    in_port = InPort,
    actions = parse_ofp_actions(Actions),
    data = RestData
  }.

parse_ofp_flow_mod(Body) ->
  <<Cookie:64, CookieMask:64,
    TableId:8, Command:8, IdleTimeout:16, HardTimeout:16,
    Priority:16, BufferId:32,
    OutPort:32, OutGroup:32,
    Flags:16, _Pad:16,
    VariableData/binary>> = Body,
    {OFPMatchBin, OFPInstructionsBin} = split_ofp_match_and_instructions(VariableData),
    #ofp_flow_mod{
       cookie = Cookie,
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
       match = parse_ofp_match(OFPMatchBin),
       instructions = parse_ofp_instructions(OFPInstructionsBin, [])
    }.


parse_ofp_multipart_request(OFPBody) ->
  <<Type:16, Flags:16, _Pad:32, Body/binary>> = OFPBody,
  #ofp_multipart_request{
     type = Type,
     flags = Flags,
     body = parse_ofp_multipart_body(Type, Body)
  }.

parse_ofp_multipart_reply(OFPBody) ->
  <<
    Type:16,
    Flags:16,
    _Pad:32,
    Body/binary
  >> = OFPBody,
  #ofp_multipart_reply {
    type = Type,
    flags = Flags,
    body = parse_ofp_multipart_body(Type, Body)
  }
.

parse_ofp_multipart_body(?OFPMP_FLOW, Body) ->
  erl_ofc_of13_util:parse_array_struct(Body,
    fun first_16_bytes_splitter/1, fun parse_ofp_flow_stats/1, []);
parse_ofp_multipart_body(?OFPMP_GROUP, Body) ->
  erl_ofc_of13_util:parse_array_struct(Body,
    fun first_16_bytes_splitter/1, fun parse_ofp_group_stats/1, []);
parse_ofp_multipart_body(?OFPMP_GROUP_DESC, Body) ->
  erl_ofc_of13_util:parse_array_struct(Body,
    fun first_16_bytes_splitter/1, fun parse_ofp_group_desc/1, []);
parse_ofp_multipart_body(?OFPMP_PORT_DESC, Body) ->
  erl_ofc_of13_util:parse_array_struct(Body,
    n_bytes_splitter(64), fun parse_ofp_port/1, []);
parse_ofp_multipart_body(_, _Body) ->
  [].

first_16_bytes_splitter(Bin = <<Length:16, _/binary>>) ->
  erl_ofc_of13_util:binary_splitter(Bin, Length).

n_bytes_splitter(NByte) ->
  fun(Bin) -> erl_ofc_of13_util:binary_splitter(Bin, NByte) end.

parse_ofp_group_stats(<<_Length:16,
                        _Pad:16,
                        GroupId:32,
                        RefCount:32,
                        _Pad2:32,
                        PacketCount:64,
                        ByteCount:64,
                        DurationSec:32,
                        DurationNsec:32,
                        BucketStats/binary>>) ->
  #ofp_group_stats{
    group_id = GroupId,
    ref_count = RefCount,
    packet_count = PacketCount,
    byte_count = ByteCount,
    duration_sec = DurationSec,
    duration_nsec = DurationNsec,
    bucket_stats = parse_ofp_bucket_counter(BucketStats)
  }.

parse_ofp_bucket_counter(<<PacketCount:64, ByteCount:64>>) ->
  #ofp_bucket_counter{
    packet_count = PacketCount,
    byte_count = ByteCount
  };
parse_ofp_bucket_counter(Bin) ->
  parse_ofp_bucket_counter(Bin, []).
parse_ofp_bucket_counter(<<>>, Acc) ->
  lists:reverse(Acc);
parse_ofp_bucket_counter(Bin, Acc) ->
  <<First16Bytes:16/binary, Rest/binary>> = Bin,
  parse_ofp_bucket_counter(Rest,
                           [parse_ofp_bucket_counter(First16Bytes) | Acc]).


parse_ofp_group_desc(<<_Length:16, Type:8, _Pad:8, GroupId:32, Buckets/binary>>) ->
  #ofp_group_desc {
    type = Type,
    group_id = GroupId,
    buckets = parse_ofp_buckets(Buckets)
  }.

parse_ofp_buckets(Bin) ->
  erl_ofc_of13_util:parse_array_struct(Bin,
    fun first_16_bytes_splitter/1, fun parse_ofp_bucket/1, []).

parse_ofp_bucket(<<_Len:16, Weight:16, WatchPort:32, WatchGroup:32, _Pad:32, Actions/binary>>) ->
  #ofp_bucket {
    weight = Weight,
    watch_port = WatchPort,
    watch_group = WatchGroup,
    actions = parse_ofp_actions(Actions)
  }.

split_ofp_match_and_instructions(Bin) ->
  <<_Type:16, Length:16, _Rest/binary>> = Bin,
  PadSize = erl_ofc_of13_util:pad_size(Length),
  OFPMatchEntireSize = Length + PadSize,
  OFPMatch = binary:part(Bin, {0, OFPMatchEntireSize}),
  OFPInstructions = binary:part(Bin, {OFPMatchEntireSize,
    byte_size(Bin)-OFPMatchEntireSize}),
  {OFPMatch, OFPInstructions}.

parse_ofp_port(<<PortNo:32,
  _Pad:32,
  HwAddr:6/binary,
  _Pad2:16,
  Name:?OFP_MAX_PORT_NAME_LEN/binary,
  Config:32,
  State:32,
  Curr:32,
  Advertised:32,
  Supported:32,
  Peer:32,
  CurrSpeed:32,
  MaxSpeed:32>>) ->
  #ofp_port{
    port_no = PortNo,
    hw_addr = mac_addr(HwAddr),
    name = binary_to_list(Name),
    config = Config,
    state = State,
    curr = Curr,
    advertised = Advertised,
    supported = Supported,
    peer = Peer,
    curr_speed = CurrSpeed,
    max_speed = MaxSpeed
  }.

parse_ofp_flow_stats(Body) ->
  <<
    _Length:16,
    TableId:8,
    _Pad:8,
    DurationSec:32,
    DurationNsec:32,
    Priority:16,
    IdleTimeout:16,
    HardTimeout:16,
    Flags:16,
    _Pad2:32,
    Cookie:64,
    PacketCount:64,
    ByteCount:64,
    Match/binary
  >> = Body,
  {OFPMatchBin, OFPInstructionsBin} = split_ofp_match_and_instructions(Match),
  #ofp_flow_stats {
    table_id = TableId,
    duration_sec = DurationSec,
    duration_nsec = DurationNsec,
    priority = Priority,
    idle_timeout = IdleTimeout,
    hard_timeout = HardTimeout,
    flags = Flags,
    cookie = Cookie,
    packet_count = PacketCount,
    byte_count = ByteCount,
    match = parse_ofp_match(OFPMatchBin),
    instructions = parse_ofp_instructions(OFPInstructionsBin, [])
  }.

parse_ofp_match(<<Type:16, Length:16, _OXMFields/binary>>) when Length =:= 4 ->
  #ofp_match {
    type = Type,
    oxm_fields = []
  };
parse_ofp_match(<<Type:16, Length:16, OXMFields/binary>>) when Length =:= 4 + byte_size(OXMFields) ->
  #ofp_match {
    type = Type,
    oxm_fields = parse_oxm_fields(OXMFields)
  };
parse_ofp_match(<<Type:16, Length:16, OXMFieldsWithPad/binary>>) ->
  OXMFields = binary:part(OXMFieldsWithPad, {0, Length-4}),
  #ofp_match {
    type = Type,
    oxm_fields = parse_oxm_fields(OXMFields)
  }.

parse_oxm_fields(Bin) ->
  erl_ofc_of13_util:parse_array_struct(Bin,
    fun oxm_field_splitter/1, fun parse_oxm_field/1, []).

parse_oxm_field(<<OXMClass:16, OXMField:7, OXMHasMask:1, OXMLength:8, Payload/binary>>) ->
  [Payload2, _Pad] = erl_ofc_of13_util:binary_splitter(Payload, OXMLength),
  #oxm_field {
    oxm_class = OXMClass,
    oxm_field = OXMField,
    oxm_has_mask = OXMHasMask,
    oxm_value = parse_oxm_value(OXMField, Payload2)
  }.

oxm_field_splitter(Bin = <<_Class:16, _Field:7, _HasMask:1, OXMLength:8, _Payload/binary>>) ->
  erl_ofc_of13_util:binary_splitter(Bin, 4+OXMLength).

parse_ofp_instructions(<<>>, Acc) ->
  lists:reverse(Acc);
parse_ofp_instructions(<<Type:16, Len:16, Body/binary>>, Acc) ->
  BodySize = Len-4,
  BodyBin = binary:part(Body, {0, BodySize}),
  RestBin = binary:part(Body, {BodySize, byte_size(Body) - BodySize}),
  parse_ofp_instructions(RestBin, [ parse_ofp_instruction(Type, BodyBin) | Acc ]).

parse_ofp_instruction(Type, Bin) ->
  parse_ofp_instruction_body(Type, Bin).

parse_ofp_instruction_body(?OFPIT_GOTO_TABLE, Bin) ->
  <<TableId:8, _Pad:24>> = Bin,
  #ofp_instruction_goto_table {
    table_id = TableId
  };
parse_ofp_instruction_body(?OFPIT_WRITE_METADATA, Bin) ->
  <<_Pad:32, Metadata:64, MetadataMask:64>> = Bin,
  #ofp_instruction_write_metadata {
    metadata = Metadata,
    metadata_mask = MetadataMask
  };
parse_ofp_instruction_body(?OFPIT_WRITE_ACTIONS, Bin) ->
  <<_Pad:32, Actions/binary>> = Bin,
  #ofp_instruction_write_actions {
    actions = parse_ofp_actions(Actions)
  };
parse_ofp_instruction_body(?OFPIT_APPLY_ACTIONS, Bin) ->
  <<_Pad:32, Actions/binary>> = Bin,
  #ofp_instruction_apply_actions {
    actions = parse_ofp_actions(Actions)
  };
parse_ofp_instruction_body(?OFPIT_CLEAR_ACTIONS, Bin) ->
  <<_Pad:32, _Actions/binary>> = Bin,
  #ofp_instruction_clear_actions {
    actions = []
  };
parse_ofp_instruction_body(?OFPIT_METER, Bin) ->
  <<MeterId:32>> = Bin,
  #ofp_instruction_meter {
    meter_id = MeterId
  };
parse_ofp_instruction_body(?OFPIT_EXPERIMENTER, Bin) ->
  <<Experimenter:32, ExperimenterData/binary>> = Bin,
  #ofp_instruction_experimenter {
    experimenter = Experimenter,
    data = ExperimenterData
  };
parse_ofp_instruction_body(InstructionType, _Bin) ->
  error_logger:warning_msg("unknown instruction type ~p", [InstructionType]),
  undefined.

parse_ofp_actions(Bin) ->
  erl_ofc_of13_util:parse_array_struct(
    Bin, fun ofp_action_splitter/1, fun parse_ofp_action/1, []).

ofp_action_splitter(Bin = <<_Type:16, Len:16, _Body/binary>>) ->
  erl_ofc_of13_util:binary_splitter(Bin, Len).

parse_ofp_action(Bin) ->
  <<Type:16, _Len:16, Body/binary>> = Bin,
  parse_ofp_action_body(Type, Body).

parse_ofp_action_body(?OFPAT_OUTPUT, Bin) ->
  <<Port:32, MaxLen:16, _Pad:48>> = Bin,
  #ofp_action_output {
    port = Port,
    max_len = MaxLen
  };
parse_ofp_action_body(?OFPAT_GROUP, Bin) ->
  <<GroupId:32>> = Bin,
  #ofp_action_group {
    group_id = GroupId
  };
parse_ofp_action_body(?OFPAT_SET_QUEUE, Bin) ->
  <<QueueId:32>> = Bin,
  #ofp_action_set_queue {
    queue_id = QueueId
  };
parse_ofp_action_body(?OFPAT_SET_MPLS_TTL, Bin) ->
  <<MplsTtl:8, _Pad:24>> = Bin,
  #ofp_action_mpls_ttl {
    mpls_ttl = MplsTtl
  };
parse_ofp_action_body(?OFPAT_SET_NW_TTL, Bin) ->
  <<NwTtl:8, _Pad:24>> = Bin,
  #ofp_action_nw_ttl {
    nw_ttl = NwTtl
  };
parse_ofp_action_body(?OFPAT_PUSH_VLAN, Bin) ->
  <<Ethertype:16, _Pad:16>> = Bin,
  #ofp_action_push_vlan {
    ethertype = Ethertype
  };
parse_ofp_action_body(?OFPAT_POP_VLAN, _Bin) ->
  #ofp_action_pop_vlan {
  };
parse_ofp_action_body(?OFPAT_PUSH_MPLS, Bin) ->
  <<Ethertype:16, _Pad:16>> = Bin,
  #ofp_action_push_mpls {
    ethertype = Ethertype
  };
parse_ofp_action_body(?OFPAT_PUSH_PBB, Bin) ->
  <<Ethertype:16, _Pad:16>> = Bin,
  #ofp_action_push_pbb {
    ethertype = Ethertype
  };
parse_ofp_action_body(?OFPAT_POP_MPLS, Bin) ->
  <<Ethertype:16, _Pad:16>> = Bin,
  #ofp_action_pop_mpls {
    ethertype = Ethertype
  };
parse_ofp_action_body(?OFPAT_SET_FIELD, Bin) ->
  #ofp_action_set_field {
    field = parse_oxm_field(Bin)
  };
parse_ofp_action_body(?OFPAT_ENCAP, Bin) ->
  <<PacketType:32, Props/binary>> = Bin,
  #ofp_action_encap {
     packet_type = PacketType,
     props = Props
  };
parse_ofp_action_body(?OFPAT_DECAP, Bin) ->
  <<CurPktType:32, NewPktType:32, _Pad:32, Props/binary>> = Bin,
  #ofp_action_decap {
     cur_pkt_type = CurPktType,
     new_pkt_type = NewPktType,
     props = Props
  };
parse_ofp_action_body(?OFPAT_EXPERIMENTER, Bin) ->
  <<Experimenter:32, Data/binary>> = Bin,
  #ofp_action_experimenter {
    experimenter = Experimenter,
    data = Data
  };
parse_ofp_action_body(OFPAction, Bin) ->
  error_logger:warning_msg("unknown OFPAction ~w : ~w", [OFPAction, Bin]),
  #ofp_action{
     type = OFPAction,
     body = Bin
  }.

parse_ofp_error_msg(<<Type:16,
                      Code:16,
                      Data/binary>>) ->
  #ofp_error_msg {
    type = Type,
    code = Code,
    data = Data
  }.

parse_oxm_value(?OFPXMT_OFB_IN_PORT, ValueBin) ->
  int_32(ValueBin);
parse_oxm_value(?OFPXMT_OFB_IN_PHY_PORT, ValueBin) ->
  int_32(ValueBin);
parse_oxm_value(?OFPXMT_OFB_METADATA, ValueBin) ->
  <<Value:64>> = ValueBin,
  Value;
parse_oxm_value(?OFPXMT_OFB_ETH_DST, ValueBin) ->
  mac_addr(ValueBin);
parse_oxm_value(?OFPXMT_OFB_ETH_SRC, ValueBin) ->
  mac_addr(ValueBin);
parse_oxm_value(?OFPXMT_OFB_ETH_TYPE, ValueBin) ->
  int_16(ValueBin);
parse_oxm_value(?OFPXMT_OFB_VLAN_VID, ValueBin) ->
  <<_Pad:3, Value:13>> = ValueBin,
  Value;
parse_oxm_value(?OFPXMT_OFB_VLAN_PCP, ValueBin) ->
  <<Value:3>> = ValueBin,
  Value;
parse_oxm_value(?OFPXMT_OFB_IP_DSCP, ValueBin) ->
  <<Value:6>> = ValueBin,
  Value;
parse_oxm_value(?OFPXMT_OFB_IP_ECN, ValueBin) ->
  <<Value:2>> = ValueBin,
  Value;
parse_oxm_value(?OFPXMT_OFB_IP_PROTO, ValueBin) ->
  <<Value:8>> = ValueBin,
  Value;
parse_oxm_value(?OFPXMT_OFB_IPV4_SRC, ValueBin) ->
  ip_addr(ValueBin);
parse_oxm_value(?OFPXMT_OFB_IPV4_DST, ValueBin) ->
  ip_addr(ValueBin);
parse_oxm_value(?OFPXMT_OFB_TCP_SRC, ValueBin) ->
  int_16(ValueBin);
parse_oxm_value(?OFPXMT_OFB_TCP_DST, ValueBin) ->
  int_16(ValueBin);
parse_oxm_value(?OFPXMT_OFB_UDP_SRC, ValueBin) ->
  int_16(ValueBin);
parse_oxm_value(?OFPXMT_OFB_UDP_DST, ValueBin) ->
  int_16(ValueBin);
parse_oxm_value(?OFPXMT_OFB_SCTP_SRC, ValueBin) ->
  int_16(ValueBin);
parse_oxm_value(?OFPXMT_OFB_SCTP_DST, ValueBin) ->
  int_16(ValueBin);
parse_oxm_value(?OFPXMT_OFB_ICMPV4_TYPE, ValueBin) ->
  int_8(ValueBin);
parse_oxm_value(?OFPXMT_OFB_ICMPV4_CODE, ValueBin) ->
  int_8(ValueBin);
parse_oxm_value(?OFPXMT_OFB_ARP_OP, ValueBin) ->
  int_16(ValueBin);
parse_oxm_value(?OFPXMT_OFB_ARP_SPA, ValueBin) ->
  ip_addr(ValueBin);
parse_oxm_value(?OFPXMT_OFB_ARP_TPA, ValueBin) ->
  ip_addr(ValueBin);
parse_oxm_value(?OFPXMT_OFB_ARP_SHA, ValueBin) ->
  mac_addr(ValueBin);
parse_oxm_value(?OFPXMT_OFB_ARP_THA, ValueBin) ->
  mac_addr(ValueBin);
parse_oxm_value(?OFPXMT_OFB_MPLS_LABEL, ValueBin) ->
  <<0:12, Value:20>> = ValueBin,
  Value;
parse_oxm_value(?OFPXMT_OFB_MPLS_TC, ValueBin) ->
  <<Value:3>> = ValueBin,
  Value;
parse_oxm_value(?OFPXMT_OFP_MPLS_BOS, ValueBin) ->
  <<Value:1>> = ValueBin,
  Value;
parse_oxm_value(_, ValueBin) ->
  ValueBin.

mac_addr(Bin) ->
  <<B1:8, B2:8, B3:8, B4:8, B5:8, B6:8>> = Bin,
  {B1, B2, B3, B4, B5, B6}.
ip_addr(Bin) ->
  <<B1:8, B2:8, B3:8, B4:8>> = Bin,
  {B1, B2, B3, B4}.
int_8(Bin) ->
  <<Value:8>> = Bin,
  Value.
int_16(Bin) ->
  <<Value:16>> = Bin,
  Value.
int_32(Bin) ->
  <<Value:32>> = Bin,
  Value.
