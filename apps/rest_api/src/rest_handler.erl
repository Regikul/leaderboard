%%%-------------------------------------------------------------------
%%% @author regikul
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2017 16:52
%%%-------------------------------------------------------------------
-module(rest_handler).
-author("regikul").

-record(state, {}).

-define(CONTENT_TYPE_TEXT, [{<<"content-type">>, <<"text/plain">>}]).
-define(CONTENT_TYPE_JSON, [{<<"content-type">>, <<"application/json">>}]).

-define(ACTION_TOKEN, <<"action">>).
-define(AMOUNT_TOKEN, <<"amount">>).
-define(NAME_TOKEN, <<"name">>).

%% API
-export([
  init/3,
  handle/2,
  terminate/3
]).

init({tcp, http}, Req, []) ->
  {ok, Req, #state{}}.

handle(Req0, State = #state{}) ->
  {ok, RetReq} = case cowboy_req:method(Req0) of
                   {<<"GET">>, GetReq0} ->
                     {Action, GetReq1} = cowboy_req:qs_val(<<"action">>, GetReq0, <<"best_players">>),
                     {Response, GetReq2} = get_response(Action, GetReq1),
                     cowboy_req:reply(200, ?CONTENT_TYPE_JSON, Response, GetReq2);
                   {_, ElseReq0} ->
                     cowboy_req:reply(404, ?CONTENT_TYPE_TEXT, <<"Not found. \n\nP.S. Nginx guilty">>, ElseReq0)
                 end,
  {ok, RetReq, State}.

terminate(_Reason, _Req, _State) ->
  ok.

get_response(<<"best_players">>, Req0) ->
  {Amount, Req1} =  cowboy_req:qs_val(?AMOUNT_TOKEN, Req0, 20),
  BestPlayers = leaderboard:best(Amount),
  Proplist = lists:map(fun ({Score, Name}) -> [{<<"score">>, Score}, {<<"name">>, Name}] end, BestPlayers),
  Result = [{<<"status">>, 200}, {<<"data">>, Proplist}],
  JSON = jsx:encode(Result),
  {JSON, Req1};
get_response(<<"put">>, Req0) ->
  {Amount, Req1} =  cowboy_req:qs_val(?AMOUNT_TOKEN, Req0, 20),
  {Name, Req2} =  cowboy_req:qs_val(?NAME_TOKEN, Req1),
  leaderboard:post(Amount, Name),
  JSON = jsx:encode([{<<"status">>, 200}]),
  {JSON, Req2}.
