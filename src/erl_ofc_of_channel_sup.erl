-module(erl_ofc_of_channel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  error_logger:info_msg("channel_sup starting"),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  Port = 6633,
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}]),
  error_logger:info_msg("[TCP] listening start on port: ~p", [Port]),
  spawn_link(fun empty_listeners/0),

  WorkerArg = [ListenSocket],
  {ok, {{simple_one_for_one, 60, 3600},
        [{erl_ofc_sock_acceptor_sup, {erl_ofc_sock_acceptor_sup, start_link, WorkerArg},
         temporary, 5000, supervisor, [erl_ofc_sock_acceptor_sup]}
        ]}}.

start_socket() ->
  supervisor:start_child(?SERVER, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

empty_listeners() ->
  [start_socket() || _ <- lists:seq(1, 20)],
  ok.
