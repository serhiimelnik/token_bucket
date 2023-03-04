%%%-------------------------------------------------------------------
%%% @doc
%%% Storage for tokens
%%% @end
%%%-------------------------------------------------------------------
-module(token_bucket_storage).

%% API
-export([init/0,
         inc/2,
         clean_expired/0]).

-define(KEY(Id, Timestamp), {Id, Timestamp}).

-include_lib("stdlib/include/ms_transform.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Init storage
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
  ?MODULE = ets:new(?MODULE, [set, named_table, public]),

  ok.

%%--------------------------------------------------------------------
%% @doc
%% Increase counter
%% @end
%%--------------------------------------------------------------------
-spec inc(pos_integer(), pos_integer()) -> boolean().
inc(Id, Threshold) ->
  Key = ?KEY(Id, get_timestamp()),

  case ets:lookup(?MODULE, Key) of
    [{Key, Counter}] when Counter < Threshold ->
      ets:update_counter(?MODULE, Key, 1),

      false;
    [] ->
      ets:insert(?MODULE, {Key, 1}),

      false;
    _ ->
      true
  end.

%%--------------------------------------------------------------------
%% @doc
%% Remove expited data
%% @end
%%--------------------------------------------------------------------
-spec clean_expired() -> ok.
clean_expired() ->
  Now = get_timestamp(),
  true =
    ets:foldl(
      fun
        ({{_, Timestamp} = Key, _}, Acc) when Timestamp < Now ->
          ets:delete(?MODULE, Key),

          Acc;
        (_, Acc) ->
          Acc
      end,
      true,
      ?MODULE
   ),

  ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_timestamp() ->
  erlang:system_time(second).
