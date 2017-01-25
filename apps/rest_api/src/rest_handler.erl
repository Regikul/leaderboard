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

-define(USERNAME, <<"admin">>).
-define(PASSWORD, <<"password">>).

-define(AUTH_HEADER, <<"authorization">>).

%% API
-export([
  init/3,
  handle/2,
  terminate/3
]).

init({tcp, http}, Req, []) ->
  {ok, Req, #state{}}.

handle(Req0, State = #state{}) ->
  lager:info("trying to handle"),
  {AuthHeader, Req1} = cowboy_req:header(?AUTH_HEADER, Req0),
  Creds = creds_from_header(AuthHeader),
  lager:info("got creds: ~p", [Creds]),
  RetReq = case Creds of
             {?USERNAME, ?PASSWORD} ->
               lager:info("running query"),
               run_query(Req1);
             _ ->
               lager:info("running auth"),
               run_auth(Req1)
           end,
  {ok, RetReq, State}.

run_auth(Req0) ->
  Req1 = cowboy_req:set_resp_header(<<"Www-Authenticate">>, <<"Basic realm=\"Secure Area\"">>, Req0),
  Req2 = cowboy_req:set_resp_body(<<"Ha-ha!">>, Req1),
  {ok, Req3} = cowboy_req:reply(401, Req2),
  Req3.

creds_from_header(undefined) -> undefined;
creds_from_header(Header) ->
  lager:info("Header: ~p", [Header]),
  Creds = case binary:split(Header, <<" ">>) of
            [<<"Basic">>, Base64Creds] -> Base64Creds;
            _ -> undefined
          end,
  case cowboy_http:authorization(Creds, <<"basic">>) of
    {<<"basic">>, Result} -> Result;
    _ -> undefined
  end.

run_query(Req0) ->
  {ok, RetReq} = case cowboy_req:method(Req0) of
                   {<<"GET">>, GetReq0} ->
                     {Action, GetReq1} = cowboy_req:qs_val(<<"action">>, GetReq0, <<"best_players">>),
                     {Response, GetReq2} = get_response(Action, GetReq1),
                     cowboy_req:reply(200, ?CONTENT_TYPE_JSON, Response, GetReq2);
                   {_, ElseReq0} ->
                     cowboy_req:reply(404, ?CONTENT_TYPE_TEXT, <<"Not found. \n\nP.S. Nginx guilty">>, ElseReq0)
                 end,
  RetReq.

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
