-module(erl_ofc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    error_logger:info_msg("erl_ofc_sup starting"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Child) ->
    supervisor:start_child(?MODULE, Child).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% Controller, module name of ofp message handler
init([]) ->
    error_logger:info_msg("erl_ofc_sup initializing"),
    {ok, {{one_for_one, 5, 10}, []}}.

