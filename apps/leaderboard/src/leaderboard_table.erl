%%%-------------------------------------------------------------------
%%% @author regikul
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2017 16:19
%%%-------------------------------------------------------------------
-module(leaderboard_table).
-compile({no_auto_import,[node/0]}).
-author("regikul").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  best_players/0,
  post_score/2,
  node/0
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  table :: ets:tab()
}).

-record(score_record, {
  scores :: {non_neg_integer(), integer()},
  name :: binary()
}).

-spec best_players() -> list().
best_players() ->
  gen_server:call({global, ?SERVER}, {best, 20}).

-spec post_score(non_neg_integer(), binary()) -> 'ok'.
post_score(Score, Name) ->
  gen_server:cast({global, ?SERVER}, {post, Score, Name}).

-spec node() -> atom().
node() ->
  Pid = global:whereis_name(?SERVER),
  erlang:node(Pid).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  Table = ets:new(score_table, [ordered_set, protected, {keypos, #score_record.scores}]),
  {ok, #state{
    table = Table
  }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({best, HowMany}, _From, #state{table = Tab} = State) ->
  Best = ask_best(Tab, HowMany),
  {reply, Best, State};
handle_call(_Request, _From, State) ->
  lager:info("unknown call: ~p", [_Request]),
  {reply, ok, State}.

-spec ask_best(ets:tab(), pos_integer()) -> [{non_neg_integer(), binary()}].
ask_best(Tab, Needed) ->
  Available = ets:info(Tab, size),
  MS = [{#score_record{scores = {'$1','_'},name = '$2'},
        [],
        [{{'$1','$2'}}]
  }],
  Returned = case Available >= Needed orelse Available of
               true -> ets:select_reverse(Tab, MS, Needed);
               0 -> '$end_of_table';
               Num when is_integer(Num) -> ets:select_reverse(Tab, MS, Available)
             end,
  case Returned of
    {Best, _Continuation} -> Best;
    _ -> []
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({post, Score, Name}, #state{table = Tab} = State) ->
  ets:insert(Tab, mk_score_record(Score, Name)),
  {noreply, State};
handle_cast(_Request, State) ->
  lager:info("unknown cast: ~p", [_Request]),
  {noreply, State}.

-spec mk_score_record(non_neg_integer(), binary()) -> #score_record{}.
mk_score_record(Score, Name) ->
  #score_record{
    scores = {Score, erlang:unique_integer()},
    name = Name
  }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  lager:info("unknown info: ~p", [_Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
