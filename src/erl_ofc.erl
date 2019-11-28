%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erl_ofc).

%% API
-export([start/1,
  send_msg/2,
  send_msg_sync/2,
  send_msg_surely/2,
  barrier_sync/1,
  barrier/1,
  find_oxm_field_type/2
]).

-include("erl_ofc.hrl").

%%-------------------------------------------------------------------
%% @doc
%% Start OpenFlow Channel
%%
%% Controller - a gen_server behaviour module name as atom
%% if OFP Message received, cast message was sent, example message:
%% {ofp_message, Datapath, Msg}
%% @end
%%-------------------------------------------------------------------
start(ControllerPid) ->
  error_logger:info_msg("starting erl_ofc.."),
  erl_ofc_controller_server_sup:start_link(ControllerPid).

%%-------------------------------------------------------------------
%% @doc
%% Send an OpenFlow Message to the Datapath.
%% DatapathPid - Pid of Datapath
%% Msg - Message to send
%% @end
%%-------------------------------------------------------------------
send_msg(DatapathPid, Msg) ->
  erl_ofc_base_ofc:send_msg(DatapathPid, Msg).

%%-------------------------------------------------------------------
%% @doc
%% Send an OpenFlow Message to the Datapath synchronously
%% returns {ok, Msg}
%% @end
%%-------------------------------------------------------------------
send_msg_sync(DatapathPid, Msg) ->
  erl_ofc_base_ofc:send_msg_sync(DatapathPid, Msg).

%%-------------------------------------------------------------------
%% @doc
%% You can use this method, send a message and handled by datapath
%% certainly without any errors.
%% returns {ok, Msg}
%% @end
%%-------------------------------------------------------------------
send_msg_surely(DatapathPid, Msg) ->
  erl_ofc_base_ofc:send_msg_surely(DatapathPid, Msg).

%%-------------------------------------------------------------------
%% @doc
%% Send barrier request to the Datapath.
%% barrier reply send back to cast message.
%% @end
%%-------------------------------------------------------------------
barrier(DatapathPid) ->
  erl_ofc_base_ofc:barrier(DatapathPid).

%%-------------------------------------------------------------------
%% @doc
%% This function called, caller process will block
%% until receive a barreir reply.
%% @end
%%-------------------------------------------------------------------
barrier_sync(DatapathPid) ->
  erl_ofc_base_ofc:barrier_sync(DatapathPid).

%%-------------------------------------------------------------------
%% @doc
%% Find a field value from a field type from a match record
%% @end
%%-------------------------------------------------------------------
find_oxm_field_type(FieldType, Match) ->
  OxmField = lists:keyfind(FieldType, 3, Match#ofp_match.oxm_fields),
  OxmField#oxm_field.oxm_value.
