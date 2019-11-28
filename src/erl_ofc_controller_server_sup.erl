-module(erl_ofc_controller_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ControllerPid) ->
    error_logger:info_msg("erl_ofc_controller_server_sup starting"),
    supervisor:start_link(?MODULE, [ControllerPid]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% Controller, module name of ofp message handler
init([ControllerPid]) ->
    error_logger:info_msg("erl_ofc_controller_server_sup initializing"),
    {ok, {{one_for_one, 5, 10}, [
                                 #{ id => erl_ofc_controller_server,
                                    start => {erl_ofc_controller_server,
                                              start_link,
                                              [ControllerPid]}
                                  }
                                ]}}.

