%%%-------------------------------------------------------------------
%%% @author regikul
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2017 19:09
%%%-------------------------------------------------------------------
-module(leaderboard).
-author("regikul").

%% API
-export([
  best/0,
  post/2,
  node/0
]).

-spec best() -> list().
best() -> leaderboard_table:best_players().

-spec post(pos_integer(), binary()) -> ok.
post(Score, Name) -> leaderboard_table:post_score(Score, Name).

-spec node() -> atom().
node() -> leaderboard_table:node().
