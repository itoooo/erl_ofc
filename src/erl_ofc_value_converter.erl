%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erl_ofc_value_converter).

%% API
-export([to_oxm_field_atom/1]).

-include("erl_ofc.hrl").

to_oxm_field_atom(?OFPXMT_OFB_IN_PORT) -> in_port;
to_oxm_field_atom(?OFPXMT_OFB_IN_PHY_PORT) -> in_phy_port;
to_oxm_field_atom(?OFPXMT_OFB_METADATA) -> metadata;
to_oxm_field_atom(?OFPXMT_OFB_ETH_DST) -> eth_dst;
to_oxm_field_atom(?OFPXMT_OFB_ETH_SRC) -> eth_src;
to_oxm_field_atom(?OFPXMT_OFB_ETH_TYPE) -> eth_type;
to_oxm_field_atom(?OFPXMT_OFB_VLAN_VID) -> vlan_vid;
to_oxm_field_atom(?OFPXMT_OFB_VLAN_PCP) -> vlan_pcp;
to_oxm_field_atom(?OFPXMT_OFB_IP_DSCP) -> dscp;
to_oxm_field_atom(?OFPXMT_OFB_IP_ECN) -> ip_ecn;
to_oxm_field_atom(?OFPXMT_OFB_IP_PROTO) -> ip_proto;
to_oxm_field_atom(?OFPXMT_OFB_IPV4_SRC) -> ipv4_src;
to_oxm_field_atom(?OFPXMT_OFB_IPV4_DST) -> ipv4_dst;
to_oxm_field_atom(?OFPXMT_OFB_TCP_SRC) -> tcp_src;
to_oxm_field_atom(?OFPXMT_OFB_TCP_DST) -> tcp_dst;
to_oxm_field_atom(?OFPXMT_OFB_UDP_SRC) -> udp_src;
to_oxm_field_atom(?OFPXMT_OFB_UDP_DST) -> udp_dst;
to_oxm_field_atom(?OFPXMT_OFB_SCTP_SRC) -> sctp_src;
to_oxm_field_atom(?OFPXMT_OFB_SCTP_DST) -> sctp_dst;
to_oxm_field_atom(?OFPXMT_OFB_ICMPV4_TYPE) -> icmpv4_type;
to_oxm_field_atom(?OFPXMT_OFB_ICMPV4_CODE) -> ipmpv4_code;
to_oxm_field_atom(?OFPXMT_OFB_ARP_OP) -> arp_op;
to_oxm_field_atom(?OFPXMT_OFB_ARP_SPA) -> arp_spa;
to_oxm_field_atom(?OFPXMT_OFB_ARP_TPA) -> arp_tpa;
to_oxm_field_atom(?OFPXMT_OFB_ARP_SHA) -> arp_sha;
to_oxm_field_atom(?OFPXMT_OFB_ARP_THA) -> arp_tha;
to_oxm_field_atom(?OFPXMT_OFB_IPV6_SRC) -> ipv6_src;
to_oxm_field_atom(?OFPXMT_OFB_IPV6_DST) -> ipv4_dst;
to_oxm_field_atom(?OFPXMT_OFB_IPV6_FLABEL) -> ipv6_flabel;
to_oxm_field_atom(?OFPXMT_OFB_ICMPV6_TYPE) -> icmpv6_type;
to_oxm_field_atom(?OFPXMT_OFB_ICMPV6_CODE) -> icmpv6_code;
to_oxm_field_atom(?OFPXMT_OFB_IPV6_ND_TARGET) -> ipv6_nd_target;
to_oxm_field_atom(?OFPXMT_OFB_IPV6_ND_SLL) -> ipv6_nd_sll;
to_oxm_field_atom(?OFPXMT_OFB_IPV6_ND_TLL) -> ipv6_nd_tll;
to_oxm_field_atom(?OFPXMT_OFB_MPLS_LABEL) -> mpls_label;
to_oxm_field_atom(?OFPXMT_OFB_MPLS_TC) -> mpls_tc;
to_oxm_field_atom(?OFPXMT_OFP_MPLS_BOS) -> mpls_bos;
to_oxm_field_atom(?OFPXMT_OFB_PBB_ISID) -> pbb_isid;
to_oxm_field_atom(?OFPXMT_OFB_TUNNEL_ID) -> tunnel_id;
to_oxm_field_atom(?OFPXMT_OFB_IPV6_EXTHDR) -> ipv6_exthdr;
to_oxm_field_atom(?OFPXMT_OFB_VXLAN_VNI) -> vxlan_vni.
