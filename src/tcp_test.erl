%%%-------------------------------------------------------------------
%%% @author mac
%%%-------------------------------------------------------------------
-module(tcp_test).
-author("mac").

-behaviour(gen_server).

%% API
-export([start_link/0,
  start_svr/0,
  start_svr/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_svr(Port,AccepterNum) ->
  SockOpts = [binary, {packet, 2}, {active, true}, {reuseaddr, true}, {nodelay, true},
    {delay_send, false}, {send_timeout, 5000},{keepalive, true},{ip, {0,0,0,0}} ],
  dhtcp:start(Port, SockOpts, AccepterNum, fun() -> start_link() end,agent).

start_svr() ->
  SockOpts = [binary, {packet, 2}, {active, true}, {reuseaddr, true}, {nodelay, true},
    {delay_send, false}, {send_timeout, 5000},{keepalive, true},{ip, {0,0,0,0}} ],
  dhtcp:start(10000, SockOpts, 1, fun() -> start_link() end,agent).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
  {ok, #state{}}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({tcp, Data}, State) ->
  lager:info("recv net data: ~p", [Data]),
  {noreply, State};
handle_info({tcp_closed, Pid, Reason}, State)->
  lager:info("recv tcp_closes, pid: ~p, reason: ~p",[Pid, Reason]),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
