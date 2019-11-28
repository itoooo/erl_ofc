-define(OFP_HEADER_SIZE, 8).
-define(OFP_NO_BUFFER, 16#ffffffff).
-define(OFP_MAX_PORT_NAME_LEN, 16).

-define(OFPT_HELLO, 0).
-define(OFPT_ERROR, 1).
-define(OFPT_ECHO_REQUEST, 2).
-define(OFPT_ECHO_REPLY, 3).
-define(OFPT_EXPERIMENTER, 4).
-define(OFPT_FEATURES_REQUEST, 5).
-define(OFPT_FEATURES_REPLY, 6).
-define(OFPT_GET_CONFIG_REQUEST, 7).
-define(OFPT_GET_CONFIG_REPLY, 8).
-define(OFPT_SET_CONFIG, 9).
-define(OFPT_PACKET_IN, 10).
-define(OFPT_FLOW_REMOVED, 11).
-define(OFPT_PORT_STATUS, 12).
-define(OFPT_PACKET_OUT, 13).
-define(OFPT_FLOW_MOD, 14).
-define(OFPT_GROUP_MOD, 15).
-define(OFPT_PORT_MOD, 16).
-define(OFPT_TABLE_MOD, 17).
-define(OFPT_MULTIPART_REQUEST, 18).
-define(OFPT_MULTIPART_REPLY, 19).
-define(OFPT_BARRIER_REQUEST, 20).
-define(OFPT_BARRIER_REPLY, 21).
-define(OFPT_QUEUE_GET_CONFIG_REQUEST, 22).
-define(OFPT_QUEUE_GET_CONFIG_REPLY, 23).
-define(OFPT_ROLE_REQUEST, 24).
-define(OFPT_ROLE_REPLY, 25).
-define(OFPT_GET_ASYNC_REQUEST, 26).
-define(OFPT_GET_ASYNC_REPLY, 27).
-define(OFPT_SET_ASYNC, 28).
-define(OFPT_METER_MOD, 29).

-define(OFPFC_ADD, 0).
-define(OFPFC_MODIFY, 1).
-define(OFPFC_MODIFY_STRICT, 2).
-define(OFPFC_DELETE, 3).
-define(OFPFC_DELETE_STRICT, 4).

-define(OFPMP_DESC, 0).
-define(OFPMP_FLOW, 1).
-define(OFPMP_AGGREGATE, 2).
-define(OFPMP_TABLE, 3).
-define(OFPMP_PORT_STATS, 4).
-define(OFPMP_QUEUE, 5).
-define(OFPMP_GROUP, 6).
-define(OFPMP_GROUP_DESC, 7).
-define(OFPMP_GROUP_FEATURES, 8).
-define(OFPMP_METER, 9).
-define(OFPMP_METER_CONFIG, 10).
-define(OFPMP_METER_FEATURES, 11).
-define(OFPMP_TABLE_FEATURES, 12).
-define(OFPMP_PORT_DESC, 13).
-define(OFPMP_EXPERIMENTER, 16#ffff).

-define(OFPTT_MAX, 16#fe).
-define(OFPTT_ALL, 16#ff).

-define(OFPP_MAX, 16#ffffff00).
-define(OFPP_IN_PORT, 16#fffffff8).
-define(OFPP_TABLE, 16#fffffff9).
-define(OFPP_NORMAL, 16#fffffffa).
-define(OFPP_FLOOD, 16#fffffffb).
-define(OFPP_ALL, 16#fffffffc).
-define(OFPP_CONTROLLER, 16#fffffffd).
-define(OFPP_LOCAL, 16#fffffffe).
-define(OFPP_ANY, 16#ffffffff).

-define(OFPG_MAX, 16#ffffff00).
-define(OFPG_ALL, 16#fffffffc).
-define(OFPG_ANY, 16#ffffffff).

-define(OFPGC_ADD, 0).
-define(OFPGC_MODIFY, 1).
-define(OFPGC_DELETE, 2).

-define(OFPGT_ALL, 0).
-define(OFPGT_SELECT, 1).
-define(OFPGT_INDIRECT, 2).
-define(OFPGT_FF, 3).

-define(OFPMT_STANDARD, 0).
-define(OFPMT_OXM, 1).

-define(OFPXMC_NXM_0, 16#0000).
-define(OFPXMC_NXM_1, 16#0001).
-define(OFPXMC_OPENFLOW_BASIC, 16#8000).
-define(OFPXMC_EXPERIMENTER, 16#ffff).

-define(OFPXMT_OFB_IN_PORT, 0).
-define(OFPXMT_OFB_IN_PHY_PORT, 1).
-define(OFPXMT_OFB_METADATA, 2).
-define(OFPXMT_OFB_ETH_DST, 3).
-define(OFPXMT_OFB_ETH_SRC, 4).
-define(OFPXMT_OFB_ETH_TYPE, 5).
-define(OFPXMT_OFB_VLAN_VID, 6).
-define(OFPXMT_OFB_VLAN_PCP, 7).
-define(OFPXMT_OFB_IP_DSCP, 8).
-define(OFPXMT_OFB_IP_ECN, 9).
-define(OFPXMT_OFB_IP_PROTO, 10).
-define(OFPXMT_OFB_IPV4_SRC, 11).
-define(OFPXMT_OFB_IPV4_DST, 12).
-define(OFPXMT_OFB_TCP_SRC, 13).
-define(OFPXMT_OFB_TCP_DST, 14).
-define(OFPXMT_OFB_UDP_SRC, 15).
-define(OFPXMT_OFB_UDP_DST, 16).
-define(OFPXMT_OFB_SCTP_SRC, 17).
-define(OFPXMT_OFB_SCTP_DST, 18).
-define(OFPXMT_OFB_ICMPV4_TYPE, 19).
-define(OFPXMT_OFB_ICMPV4_CODE, 20).
-define(OFPXMT_OFB_ARP_OP, 21).
-define(OFPXMT_OFB_ARP_SPA, 22).
-define(OFPXMT_OFB_ARP_TPA, 23).
-define(OFPXMT_OFB_ARP_SHA, 24).
-define(OFPXMT_OFB_ARP_THA, 25).
-define(OFPXMT_OFB_IPV6_SRC, 26).
-define(OFPXMT_OFB_IPV6_DST, 27).
-define(OFPXMT_OFB_IPV6_FLABEL, 28).
-define(OFPXMT_OFB_ICMPV6_TYPE, 29).
-define(OFPXMT_OFB_ICMPV6_CODE, 30).
-define(OFPXMT_OFB_IPV6_ND_TARGET, 31).
-define(OFPXMT_OFB_IPV6_ND_SLL, 32).
-define(OFPXMT_OFB_IPV6_ND_TLL, 33).
-define(OFPXMT_OFB_MPLS_LABEL, 34).
-define(OFPXMT_OFB_MPLS_TC, 35).
-define(OFPXMT_OFP_MPLS_BOS, 36).
-define(OFPXMT_OFB_PBB_ISID, 37).
-define(OFPXMT_OFB_TUNNEL_ID, 38).
-define(OFPXMT_OFB_IPV6_EXTHDR, 39).
-define(OFPXMT_OFB_VXLAN_VNI, 52).

-define(OFPIT_GOTO_TABLE, 1).
-define(OFPIT_WRITE_METADATA, 2).
-define(OFPIT_WRITE_ACTIONS, 3).
-define(OFPIT_APPLY_ACTIONS, 4).
-define(OFPIT_CLEAR_ACTIONS, 5).
-define(OFPIT_METER, 6).
-define(OFPIT_EXPERIMENTER, 16#ffff).

-define(OFPAT_OUTPUT, 0).
-define(OFPAT_COPY_TTL_OUT, 11).
-define(OFPAT_COPY_TTL_IN, 12).
-define(OFPAT_SET_MPLS_TTL, 15).
-define(OFPAT_DEC_MPLS_TTL, 16).
-define(OFPAT_PUSH_VLAN, 17).
-define(OFPAT_POP_VLAN, 18).
-define(OFPAT_PUSH_MPLS, 19).
-define(OFPAT_POP_MPLS, 20).
-define(OFPAT_SET_QUEUE, 21).
-define(OFPAT_GROUP, 22).
-define(OFPAT_SET_NW_TTL, 23).
-define(OFPAT_DEC_NW_TTL, 24).
-define(OFPAT_SET_FIELD, 25).
-define(OFPAT_PUSH_PBB, 26).
-define(OFPAT_POP_PBB, 27).
-define(OFPAT_ENCAP, 28).
-define(OFPAT_DECAP, 29).
-define(OFPAT_EXPERIMENTER, 16#ffff).

-define(OFPR_NO_MATCH, 0).
-define(OFPR_ACTION, 1).
-define(OFPR_INVALID_TTL, 2).

%%
%% ofp_error_type
%%

-define(OFPET_HELLO_FAILED, 0).
-define(OFPET_BAD_REQUEST, 1).
-define(OFPET_BAD_ACTION, 2).
-define(OFPET_BAD_INSTRUCTION, 3).
-define(OFPET_BAD_MATCH, 4).
-define(OFPET_FLOW_MOD_FAILED, 5).
-define(OFPET_GROUP_MOD_FAILED, 6).
-define(OFPET_PORT_MOD_FAILED, 7).
-define(OFPET_TABLE_MOD_FAILED, 8).
-define(OFPET_QUEUE_OP_FAILED, 9).
-define(OFPET_SWITCH_CONFIG_FAILED, 10).
-define(OFPET_ROLE_REQUEST_FAILED, 11).
-define(OFPET_METER_MOD_FAILED, 12).
-define(OFPET_TABLE_FEATURES_FAILED, 13).
-define(OFPET_EXPERIMENTER, 16#ffff).

%%
%% ofp_hello_failed_code
%%

-define(OFPHFC_INCOMPATIBLE, 0).
-define(OFPHFC_EPERM, 1).

%%
%% ofp_bad_request_code
%%

-define(OFPBRC_BAD_VERSION, 0).
-define(OFPBRC_BAD_TYPE, 1).
-define(OFPBRC_BAD_MULTIPART, 2).
-define(OFPBRC_BAD_EXPERIMENTER, 3).
-define(OFPBRC_BAD_EXP_TYPE, 4).
-define(OFPBRC_EPERM, 5).
-define(OFPBRC_BAD_LEN, 6).
-define(OFPBRC_BUFFER_EMPTY, 7).
-define(OFPBRC_BUFFER_UNKNOWN, 8).
-define(OFPBRC_BAD_TABLE_ID, 9).
-define(OFPBRC_IS_SLAVE, 10).
-define(OFPBRC_BAD_PORT, 11).
-define(OFPBRC_BAD_PACKET, 12).
-define(OFPBRC_MULTIPART_BUFFER_OVERFLOW, 13).

%%
%% ofp_bad_action_code
%%

-define(OFPBAC_BAD_TYPE, 0).
-define(OFPBAC_BAD_LEN, 1).
-define(OFPBAC_BAD_EXPERIMENTER, 2).
-define(OFPBAC_BAD_EXP_TYPE, 3).
-define(OFPBAC_BAD_OUT_PORT, 4).
-define(OFPBAC_BAD_ARGUMENT, 5).
-define(OFPBAC_EPERM, 6).
-define(OFPBAC_TOO_MANY, 7).
-define(OFPBAC_BAD_QUEUE, 8).
-define(OFPBAC_BAD_OUT_GROUP, 9).
-define(OFPBAC_MATCH_INCONSISTENT, 10).
-define(OFPBAC_UNSUPPORTED_ORDER, 11).
-define(OFPBAC_BAD_TAG, 12).
-define(OFPBAC_BAD_SET_TYPE, 13).
-define(OFPBAC_BAD_SET_LEN, 14).
-define(OFPBAC_BAD_SET_ARGUMENT, 15).

%%
%% ofp_bad_instruction_code
%%

-define(OFPBIC_UNKNOWN_INST, 0).
-define(OFPBIC_UNSUP_INST, 1).
-define(OFPBIC_BAD_TABLE_ID, 2).
-define(OFPBIC_UNSUP_METADATA, 3).
-define(OFPBIC_UNSUP_METADATA_MASK, 4).
-define(OFPBIC_BAD_EXPERIMETNER, 5).
-define(OFPBIC_BAD_EXP_TYPE, 6).
-define(OFPBIC_BAD_LEN, 7).
-define(OFPBIC_EPERM, 8).

%%
%% ofp_bad_match_code
%%

-define(OFPBMC_BAD_TYPE, 0).
-define(OFPBMC_BAD_LEN, 1).
-define(OFPBMC_BAD_TAG, 2).
-define(OFPBMC_BAD_DL_ADDR_MASK, 3).
-define(OFPBMC_BAD_NW_ADDR_MASK, 4).
-define(OFPBMC_BAD_WILDCARDS, 5).
-define(OFPBMC_BAD_FIELD, 6).
-define(OFPBMC_BAD_VALUE, 7).
-define(OFPBMC_BAD_MASK, 8).
-define(OFPBMC_BAD_PREREQ, 9).
-define(OFPBMC_DUP_FIELD, 10).
-define(OFPBMC_EPERM, 11).

%%
%% ofp_flow_mod_failed_code
%%

-define(OFPFMFC_UNKNOWN, 0).
-define(OFPFMFC_TABLE_FULL, 1).
-define(OFPFMFC_BAD_TABLE_ID, 2).
-define(OFPFMFC_OVERLAP, 3).
-define(OFPFMFC_EPERM, 4).
-define(OFPFMFC_BAD_TIMEOUT, 5).
-define(OFPFMFC_BAD_COMMAND, 6).
-define(OFPFMFC_BAD_FLAGS, 7).

%%
%% ofp_group_mod_failed_code
%%

-define(OFPGMFC_GROUP_EXISTS, 0).
-define(OFPGMFC_INVALID_GROUP, 1).
-define(OFPGMFC_WEIGHT_UNSUPPORTED, 2).
-define(OFPGMFC_OUT_OF_GROUPS, 3).
-define(OFPGMFC_OUT_OF_BUCKETS, 4).
-define(OFPGMFC_CHAINING_UNSUPPORTED, 5).
-define(OFPGMFC_WATCH_UNSUPPORTED, 6).
-define(OFPGMFC_LOOP, 7).
-define(OFPGMFC_UNKNOWN_GROUP, 8).
-define(OFPGMFC_CHAINED_GROUP, 9).
-define(OFPGMFC_BAD_TYPE, 10).
-define(OFPGMFC_BAD_COMMAND, 11).
-define(OFPGMFC_BAD_BUCKET, 12).
-define(OFPGMFC_BAD_WATCH, 13).
-define(OFPGMFC_EPERM, 14).

%%
%% ofp_port_mod_failed_code
%%

-define(OFPPMFC_BAD_PORT, 0).
-define(OFPPMFC_BAD_HW_ADDR, 1).
-define(OFPPMFC_BAD_CONFIG, 2).
-define(OFPPMFC_BAD_ADVERTISE, 3).
-define(OFPPMFC_EPERM, 4).

%%
%% ofp_table_mod_failed_code
%%

-define(OFPTMFC_BAD_TABLE, 0).
-define(OFPTMFC_BAD_CONFIG, 1).
-define(OFPTMFC_EPERM, 2).

%%
%% ofp_queue_op_failed_code
%%

-define(OFPQOFC_BAD_PORT, 0).
-define(OFPQOFC_BAD_QUEUE, 1).
-define(OFPQOFC_EPERM, 2).

%%
%% ofp_switch_config_failed_code
%%

-define(OFPSCFC_BAD_FLAGS, 0).
-define(OFPSCFC_BAD_LEN, 1).
-define(OFPSCFC_EPERM, 2).

%%
%% ofp_role_request_failed_code
%%

-define(OFPRRFC_STALE, 0).
-define(OFPRRFC_UNSUP, 1).
-define(OFPRRFC_BAD_ROLE, 2).

%%
%% ofp_meter_mod_failed_code
%%

-define(OFPMMFC_UNKNOWN, 0).
-define(OFPMMFC_METER_EXISTS, 1).
-define(OFPMMFC_INVALID_METER, 2).
-define(OFPMMFC_UNKNOWN_METER, 3).
-define(OFPMMFC_BAD_COMMAND, 4).
-define(OFPMMFC_BAD_FLAGS, 5).
-define(OFPMMFC_BAD_RATE, 6).
-define(OFPMMFC_BAD_BURST, 7).
-define(OFPMMFC_BAD_BAND, 8).
-define(OFPMMFC_BAD_BAND_VALUE, 9).
-define(OFPMMFC_OUT_OF_METERS, 10).
-define(OFPMMFC_OUT_OF_BANDS, 11).

%%
%% ofp_table_features_failed_code
%%

-define(OFPTFFC_BAD_TABLE, 0).
-define(OFPTFFC_BAD_METADATA, 1).
-define(OFPTFFC_BAD_TYPE, 2).
-define(OFPTFFC_BAD_LEN, 3).
-define(OFPTFFC_BAD_ARGUMENT, 4).
-define(OFPTFFC_EPERM, 5).

-record(ofp_message, {
  version :: integer(),
  type :: integer(),
  length :: integer(),
  xid :: integer(),
  body :: any() % #ofp_hello{} |
                % #ofp_switch_features{} |
                % #ofp_multipart_request{} |
                % #ofp_multipart_reply{} |
                % #ofp_flow_mod{} |
                % #ofp_group_mod{} |
                % #ofp_packet_in{} |
                % #ofp_flow_removed{} |
                % #ofp_packet_out{} |
                % #ofp_error_msg{}
}).

-record(ofp_hello, {}).

-record(ofp_switch_features, {
  datapath_id,
  n_buffers,
  n_tables,
  auxiliary_id,
  capabilities
}).

-record(ofp_port, {
  port_no,
  hw_addr,
  name,
  config,
  state,
  curr,
  advertised,
  supported,
  peer,
  curr_speed,
  max_speed
}).

-record(ofp_multipart_request, {
  type,
  flags = 0,
  body
}).

-record(ofp_multipart_reply, {
  type,
  flags,
  body
}).

-record(ofp_group_stats_request, {
  group_id = ?OFPG_ALL
}).

-record(ofp_group_stats, {
  group_id,
  ref_count,
  packet_count,
  byte_count,
  duration_sec,
  duration_nsec,
  bucket_stats
}).

-record(ofp_bucket_counter, {
  packet_count,
  byte_count
}).

-record(ofp_group_desc, {
  type,
  group_id,
  buckets
}).

-record(ofp_match, {
  type = ?OFPMT_OXM,
  oxm_fields = []
}).

-record(ofp_flow_mod, {
  cookie = 0 :: number,
  cookie_mask = 0:: number,
  table_id = 0:: number,
  command :: number,
  idle_timeout = 0 :: number,
  hard_timeout = 0 :: number,
  priority = 0 :: number,
  buffer_id = ?OFP_NO_BUFFER :: number,
  out_port = ?OFPP_ANY :: number,
  out_group = ?OFPG_ANY :: number,
  flags = 0 :: number,
  match = #ofp_match{},
  instructions = []
}).

-record(ofp_group_mod, {
  command,
  type = 0,
  group_id,
  buckets = []
}).

-record(ofp_bucket, {
  weight = 0,
  watch_port = 0,
  watch_group = 0,
  actions = []
}).

-record(ofp_flow_stats_request, {
  table_id = ?OFPTT_ALL,
  out_port = ?OFPP_ANY,
  out_group = ?OFPG_ANY,
  cookie = 0,
  cookie_mask = 0,
  match = #ofp_match{}
}).

-record(ofp_flow_stats, {
  table_id,
  duration_sec,
  duration_nsec,
  priority,
  idle_timeout,
  hard_timeout,
  flags,
  cookie,
  packet_count,
  byte_count,
  match,
  instructions
}).

-record(oxm_field, {
  oxm_class = ?OFPXMC_OPENFLOW_BASIC,
  oxm_field,
  oxm_has_mask = 0,
  oxm_value
}).

-record(ofp_action_output, {
  type = ?OFPAT_OUTPUT,
  len = 16,
  port,
  max_len = 65535
}).

-record(ofp_action_group, {
  type = ?OFPAT_GROUP,
  len = 8,
  group_id
}).

-record(ofp_action_set_queue, {
  type = ?OFPAT_SET_QUEUE,
  len = 8,
  queue_id
}).

-record(ofp_action_mpls_ttl, {
  type = ?OFPAT_SET_MPLS_TTL,
  len = 8,
  mpls_ttl
}).

-record(ofp_action_nw_ttl, {
  type = ?OFPAT_SET_NW_TTL,
  len = 8,
  nw_ttl
}).

-record(ofp_action_push_vlan, {
  type = ?OFPAT_PUSH_VLAN,
  len = 8,
  ethertype = 16#8100
}).

-record(ofp_action_push_mpls, {
  type = ?OFPAT_PUSH_MPLS,
  len = 8,
  ethertype = 16#8847
}).

-record(ofp_action_push_pbb, {
  type = ?OFPAT_PUSH_PBB,
  len = 8,
  ethertype = 16#88e7
}).

-record(ofp_action_pop_vlan, {
  type = ?OFPAT_POP_VLAN,
  len = 8
}).

-record(ofp_action_pop_mpls, {
  type = ?OFPAT_POP_MPLS,
  len = 8,
  ethertype
}).

-record(ofp_action_set_field, {
  type = ?OFPAT_SET_FIELD,
  field
}).

-record(ofp_action_encap, {
  type = ?OFPAT_ENCAP,
  packet_type,
  props
}).

-record(ofp_action_decap, {
  type = ?OFPAT_DECAP,
  cur_pkt_type,
  new_pkt_type,
  props
}).

-record(ofp_action_experimenter, {
  type = ?OFPAT_EXPERIMENTER,
  field,
  experimenter,
  data
}).

-record(ofp_instruction_goto_table, {
  type = ?OFPIT_GOTO_TABLE,
  table_id
}).

-record(ofp_instruction_write_actions, {
  type = ?OFPIT_WRITE_ACTIONS,
  actions
}).

-record(ofp_instruction_apply_actions, {
  type = ?OFPIT_APPLY_ACTIONS,
  actions
}).

-record(ofp_instruction_clear_actions, {
  type = ?OFPIT_CLEAR_ACTIONS,
  actions = []
}).

-record(ofp_packet_in, {
  buffer_id,
  total_len,
  reason,
  table_id,
  cookie,
  match,
  data
}).

-record(ofp_flow_removed, {
  cookie,
  priority,
  reason,
  table_id,
  duration_sec,
  duration_nsec,
  idle_timeout,
  hard_timeout,
  packet_count,
  byte_count,
  match
}).

-record(ofp_packet_out, {
  buffer_id = ?OFP_NO_BUFFER,
  in_port = ?OFPP_CONTROLLER,
  actions = [],
  data
}).

-record(ofp_action, {type, body}).
-record(ofp_action_push, {type, ethertype}).
-record(ofp_instruction, {type, body}).
-record(ofp_instruction_actions, {actions}).
-record(ofp_instruction_experimenter, {meter_id, experimenter, data}).
-record(ofp_instruction_meter, {meter_id}).
-record(ofp_instruction_write_metadata, {metadata, metadata_mask}).
-record(ofp_error_msg, {
          type = ?OFPET_BAD_REQUEST :: integer(),
          code = 0 :: integer(),
          data :: binary()
         }).
