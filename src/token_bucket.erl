%%%-------------------------------------------------------------------
%%% @doc
%%% Token bucket
%%% @end
%%%-------------------------------------------------------------------
-module(token_bucket).

%% API
-export([is_limit_reached/1,
         is_limit_reached/2]).

-define(DEFAULT_RPS, 3).

-type user_id() :: pos_integer().
-type max_rps() :: pos_integer() | infinity.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Check default limit
%% @end
%%--------------------------------------------------------------------
-spec is_limit_reached(user_id()) -> boolean().
is_limit_reached(UserId) ->
  is_limit_reached(UserId, ?DEFAULT_RPS).

%%--------------------------------------------------------------------
%% @doc
%% Check specific limit
%% @end
%%--------------------------------------------------------------------
-spec is_limit_reached(user_id(), max_rps()) -> boolean().
is_limit_reached(UserId, MaxRps) ->
  token_bucket_storage:inc(UserId, MaxRps).

%%%===================================================================
%%% Internal functions
%%%===================================================================
