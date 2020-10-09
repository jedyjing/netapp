%%%-------------------------------------------------------------------
%%% @author mac
%%%-------------------------------------------------------------------
-module(dhtcp_acceptor).
-author("mac").

-behaviour(gen_server).

%% API
-export([
  start_link/3,
  set_sock_opts/2,
  name/2
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
  sock::gen_tcp:socket(),
  ref,
  port :: integer()
  }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, LSock,X) ->
  gen_server:start_link({local, name(Port,X)},?MODULE, [Port, LSock], []).

name(Port,X) -> list_to_atom("dhtcp_accetepor" ++ integer_to_list(Port) ++ "_" ++ integer_to_list(X)).

%%%=================================================================== %%% gen_server callbacks
%%%===================================================================


init([Port, LSock]) ->
  process_flag(trap_exit, true),
  State = #state{sock = LSock, port = Port},
  accept(State).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({inet_async, LSock, Ref, {ok, Sock}}, #state{sock = LSock, ref = Ref, port = Port} = State) ->
  case set_sock_opts(LSock, Sock) of
    ok -> ok;
    {error, Reason} -> exit({set_sock_opts, Reason})
  end,
  spawn_conn(Port, Sock),
  {ok, New} = accept(State),
  {noreply, New};

handle_info({inet_async, LSock, Ref, {error, closed}}, #state{ sock = LSock, ref = Ref} = State) ->
  lager:info("tcp accept error "),
  {stop, normal, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
accept(#state{sock = Sock} = State) ->
  case prim_inet:async_accept(Sock, -1) of
    {ok, Ref} ->
      {ok, State#state{ ref = Ref}};
  {error, Reason} ->
    {stop, {cannot_accept, Reason}, State}
  end.

-spec set_sock_opts(gen_tcp:socket(), gen_tcp:socket()) -> ok | {error, any()}.
set_sock_opts(LSock, Sock) ->
  true = inet_db:register_socket(Sock, inet_tcp),
  case prim_inet:getopts(LSock, [active, nodelay, keepalive, delay_send, priority, tos, packet, packet_size]) of
    {ok, Opts} ->
      case prim_inet:setopts(Sock, Opts) of
        ok -> ok;
        {error, Reason} ->
         gen_tcp:close(Sock),
          {error, Reason}
      end;
    {error, Reason} ->
      gen_tcp:close(Sock),
      {error, Reason}
  end.

spawn_conn(Port, Sock) ->
  {ok, Child} = dhtcp_conn_sup:start_child(Port),  %创建tcp读取进程
  Child ! {dhconn_start, Sock},
  case gen_tcp:controlling_process(Sock, Child) of
    ok -> ok;
    _ ->
      catch gen_tcp:close(Sock),
      Child ! close
  end.
