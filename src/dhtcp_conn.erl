%%%-------------------------------------------------------------------
%%% @author mac
%%%-------------------------------------------------------------------
-module(dhtcp_conn).
-author("mac").

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  change_pid/2,
  send/2,
  close/1,
  ip/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {start_fun :: mfa(), pid :: pid(), sock :: gen_tcp:socket()}).

%%%===================================================================
%%% API
%%%===================================================================


%% 初始化函数
-spec start_link(fun()) -> {ok, pid()}.
start_link(Mfa) ->
  gen_server:start_link(?MODULE, [Mfa], []).

%% 同时在线，登陆统一账户，通知tcp_agent change_pid
-spec change_pid(pid(), pid()) -> ok.
change_pid(Conn, Pid) ->
  gen_server:call(Conn, {change_pid, Pid}).

-spec send(pid(), binary()) -> ok.
send(Pid, Data) -> gen_server:cast(Pid, {send, Data}).

-spec close(pid()) -> ok.
close(Pid) -> gen_server:cast(Pid, {close, self()}).

-spec ip(pid()) -> inet:ip4_address() | undefined.
ip(Pid) ->  gen_server:call(Pid, ip).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Mfa]) ->
  process_flag(trap_exit, true),
  {ok, #state{start_fun = Mfa}}.

handle_call({change_pid, Pid}, _, State) ->
  {reply, ok, State#state{pid = Pid}};

handle_call(ip, _, State) ->
  Ans = case inet:peername(State#state.sock) of
    {ok, {Addr, _}} -> Addr;
    _ -> undefined
  end,
  {reply, Ans, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({send, Data}, #state{sock = Sock} = State) ->
  case gen_tcp:send(Sock, Data) of
    ok -> {noreply, State};
    {error, Reason} ->
      State#state.pid ! {tcp_closed, self(), Reason},
      {stop, normal, State}
  end;

handle_cast({close, Pid}, State) when State#state.pid =:= Pid ->
  {stop, normal, State};

handle_cast(_Request, State) ->
  {noreply, State}.


handle_info({dhconn_start, Sock}, State) ->
  {noreply, State#state{sock = Sock}};

handle_info({tcp, Sock, Data}, #state{sock = Sock} = State) ->
  if
    State#state.pid == undefined ->
      {M, F, A} = State#state.start_fun,
      {ok, Pid} = erlang:apply(M, F, A),
      Pid ! {dhconn_start, self()};
    true -> Pid = State#state.pid
  end,
  Pid ! {dhtcp, self(), Data},
  {noreply, State#state{pid = Pid}};

handle_info({tcp_closed, Socket}, #state{sock = Socket} = State) ->
  {stop, normal, State};

handle_info({tcp_error, Socket, _}, #state{sock = Socket} = State) ->
  {stop, normal, State};

handle_info(_Info, State) ->

  {noreply, State}.

terminate(_Reason, #state{pid = Pid}) ->
   if
    is_pid(Pid) ->
      Pid ! {dhtcp_closed, self(), closed};
    true -> ok
  end,
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
