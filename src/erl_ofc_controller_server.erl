%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(erl_ofc_controller_server).

-behaviour(gen_server).

%% API
-export([start_link/1,
         cast/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          controller,
          supervisor
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Controller) ->
  error_logger:info_msg("erl_ofc_controller_server starting"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Controller], []).

cast(Datapath, Msg) ->
  gen_server:cast(?SERVER, {ofp_message, Datapath, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Controller]) ->
  error_logger:info_msg("erl_ofc_controller_server initializing"),
  % start listen ofp channel
  erl_ofc_of_channel_sup:start_link(),
  error_logger:info_msg("erl_ofc_controller_server initialized"),

  {ok, #state{controller = Controller}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(Request, State) ->
  gen_server:cast(State#state.controller, Request),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
