%%%-------------------------------------------------------------------
%% @doc rest public API
%% @end
%%%-------------------------------------------------------------------

-module(rest_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Port = application:get_env(rest_api, port, 8999),
  Dispatch = cowboy_router:compile([
    {'_', [{"/scores/", rest_handler, []}]}
  ]),
  {ok, _} = cowboy:start_http(
    rest_http_listener,
    100,
    [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  rest_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
