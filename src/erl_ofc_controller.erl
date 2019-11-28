%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erl_ofc_controller).

%% In the future, want to use a custom behavior
-callback handle_msg(DatapathPid :: pid(), Msg :: term()) -> 'ok'.
