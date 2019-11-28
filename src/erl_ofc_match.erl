%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erl_ofc_match).

-include("erl_ofc.hrl").

%% API
-export([match/1, to_map/1]).

%%-------------------------------------------------------------------
%% @doc
%% Generate ofp_match record from a oxm field map
%% @end
%%-------------------------------------------------------------------
match(Map) when is_map(Map)->
  Fields = [#oxm_field{
    oxm_field = type_code(Key),
    oxm_value = Value
  } || {Key, Value} <- maps:to_list(Map)],
  #ofp_match {
    oxm_fields = Fields
  }.

to_map(#ofp_match{oxm_fields = Fields}) ->
  MapList = [{type_key(Field#oxm_field.oxm_field), Field#oxm_field.oxm_value}
             ||
             Field <- Fields],
  maps:from_list(MapList).

type_code(in_port) -> ?OFPXMT_OFB_IN_PORT;
type_code(in_phy_port) -> ?OFPXMT_OFB_IN_PHY_PORT;
type_code(metadata) -> ?OFPXMT_OFB_METADATA;
type_code(eth_dst) -> ?OFPXMT_OFB_ETH_DST;
type_code(eth_src) -> ?OFPXMT_OFB_ETH_SRC;
type_code(eth_type) -> ?OFPXMT_OFB_ETH_TYPE;
type_code(vlan_vid) -> ?OFPXMT_OFB_VLAN_VID;
type_code(vlan_pcp) -> ?OFPXMT_OFB_VLAN_PCP;
type_code(ip_dscp) -> ?OFPXMT_OFB_IP_DSCP;
type_code(ip_ecn) -> ?OFPXMT_OFB_IP_ECN;
type_code(ip_proto) -> ?OFPXMT_OFB_IP_PROTO;
type_code(ipv4_src) -> ?OFPXMT_OFB_IPV4_SRC;
type_code(ipv4_dst) -> ?OFPXMT_OFB_IPV4_DST;
type_code(tcp_src) -> ?OFPXMT_OFB_TCP_SRC;
type_code(tcp_dst) -> ?OFPXMT_OFB_TCP_DST;
type_code(udp_src) -> ?OFPXMT_OFB_UDP_SRC;
type_code(udp_dst) -> ?OFPXMT_OFB_UDP_DST;
type_code(sctp_src) -> ?OFPXMT_OFB_SCTP_SRC;
type_code(sctp_dst) -> ?OFPXMT_OFB_SCTP_DST;
type_code(icmpv4_type) -> ?OFPXMT_OFB_ICMPV4_TYPE;
type_code(icmpv4_code) -> ?OFPXMT_OFB_ICMPV4_CODE;
type_code(arp_op) -> ?OFPXMT_OFB_ARP_OP;
type_code(arp_spa) -> ?OFPXMT_OFB_ARP_SPA;
type_code(arp_tpa) -> ?OFPXMT_OFB_ARP_TPA;
type_code(arp_sha) -> ?OFPXMT_OFB_ARP_SHA;
type_code(arp_tha) -> ?OFPXMT_OFB_ARP_THA;
type_code(ipv6_src) -> ?OFPXMT_OFB_IPV6_SRC;
type_code(ipv6_dst) -> ?OFPXMT_OFB_IPV6_DST;
type_code(ipv6_flabel) -> ?OFPXMT_OFB_IPV6_FLABEL;
type_code(icmpv6_type) -> ?OFPXMT_OFB_ICMPV6_TYPE;
type_code(ivmpv6_code) -> ?OFPXMT_OFB_ICMPV6_CODE;
type_code(ipv6_nd_target) -> ?OFPXMT_OFB_IPV6_ND_TARGET;
type_code(ipv6_nd_sll) -> ?OFPXMT_OFB_IPV6_ND_SLL;
type_code(ipv6_nd_tll) -> ?OFPXMT_OFB_IPV6_ND_TLL;
type_code(mpls_label) -> ?OFPXMT_OFB_MPLS_LABEL;
type_code(mpls_tc) -> ?OFPXMT_OFB_MPLS_TC;
type_code(mpls_bos) -> ?OFPXMT_OFP_MPLS_BOS;
type_code(pbb_isid) -> ?OFPXMT_OFB_PBB_ISID;
type_code(tunnel_id) -> ?OFPXMT_OFB_TUNNEL_ID;
type_code(ipv6_exthdr) -> ?OFPXMT_OFB_IPV6_EXTHDR.

type_key(?OFPXMT_OFB_IN_PORT) -> in_port;
type_key(?OFPXMT_OFB_IN_PHY_PORT) -> in_phy_port;
type_key(?OFPXMT_OFB_METADATA) -> metadata;
type_key(?OFPXMT_OFB_ETH_DST) -> eth_dst;
type_key(?OFPXMT_OFB_ETH_SRC) -> eth_src;
type_key(?OFPXMT_OFB_ETH_TYPE) -> eth_type;
type_key(?OFPXMT_OFB_VLAN_VID) -> vlan_vid;
type_key(?OFPXMT_OFB_VLAN_PCP) -> vlan_pcp;
type_key(?OFPXMT_OFB_IP_DSCP) -> ip_dscp;
type_key(?OFPXMT_OFB_IP_ECN) -> ip_ecn;
type_key(?OFPXMT_OFB_IP_PROTO) -> ip_proto;
type_key(?OFPXMT_OFB_IPV4_SRC) -> ipv4_src;
type_key(?OFPXMT_OFB_IPV4_DST) -> ipv4_dst;
type_key(?OFPXMT_OFB_TCP_SRC) -> tcp_src;
type_key(?OFPXMT_OFB_TCP_DST) -> tcp_dst;
type_key(?OFPXMT_OFB_UDP_SRC) -> udp_src;
type_key(?OFPXMT_OFB_UDP_DST) -> udp_dst;
type_key(?OFPXMT_OFB_SCTP_SRC) -> sctp_src;
type_key(?OFPXMT_OFB_SCTP_DST) -> sctp_dst;
type_key(?OFPXMT_OFB_ICMPV4_TYPE) -> icmpv4_type;
type_key(?OFPXMT_OFB_ICMPV4_CODE) -> icmpv4_code;
type_key(?OFPXMT_OFB_ARP_OP) -> arp_op;
type_key(?OFPXMT_OFB_ARP_SPA) -> arp_spa;
type_key(?OFPXMT_OFB_ARP_TPA) -> arp_tpa;
type_key(?OFPXMT_OFB_ARP_SHA) -> arp_sha;
type_key(?OFPXMT_OFB_ARP_THA) -> arp_tha;
type_key(?OFPXMT_OFB_IPV6_SRC) -> ipv6_src;
type_key(?OFPXMT_OFB_IPV6_DST) -> ipv6_dst;
type_key(?OFPXMT_OFB_IPV6_FLABEL) -> ipv6_flabel;
type_key(?OFPXMT_OFB_ICMPV6_TYPE) -> icmpv6_type;
type_key(?OFPXMT_OFB_ICMPV6_CODE) -> ivmpv6_code;
type_key(?OFPXMT_OFB_IPV6_ND_TARGET) -> ipv6_nd_target;
type_key(?OFPXMT_OFB_IPV6_ND_SLL) -> ipv6_nd_sll;
type_key(?OFPXMT_OFB_IPV6_ND_TLL) -> ipv6_nd_tll;
type_key(?OFPXMT_OFB_MPLS_LABEL) -> mpls_label;
type_key(?OFPXMT_OFB_MPLS_TC) -> mpls_tc;
type_key(?OFPXMT_OFP_MPLS_BOS) -> mpls_bos;
type_key(?OFPXMT_OFB_PBB_ISID) -> pbb_isid;
type_key(?OFPXMT_OFB_TUNNEL_ID) -> tunnel_id;
type_key(?OFPXMT_OFB_IPV6_EXTHDR) -> ipv6_exthdr.
