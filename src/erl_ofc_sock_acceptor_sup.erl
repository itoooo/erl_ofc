%%%-------------------------------------------------------------------
%%% @author ttsc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jul 2015 22:36
%%%-------------------------------------------------------------------
-module(erl_ofc_sock_acceptor_sup).
-author("ttsc").

-behaviour(supervisor).

%% API
-export([start_link/1, start_base_controller/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(ListenSocket) ->
  supervisor:start_link(?MODULE, [ListenSocket]).

start_base_controller(Pid, DriverPid) ->
  supervisor:start_child(Pid, {erl_ofc_base_ofc,
                               {erl_ofc_base_ofc, start_link, [DriverPid]},
                               temporary, 5000, worker, [erl_ofc_base_ofc]}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([ListenSocket]) ->
  RestartStrategy = one_for_all,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  {ok, {SupFlags, [{erl_ofc_sock_acceptor,
                    {erl_ofc_sock_acceptor, start_link, [ListenSocket, self()]},
                    temporary, 5000, worker, [erl_ofc_sock_acceptor]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
