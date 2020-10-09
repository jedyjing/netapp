%%%-------------------------------------------------------------------
%%% @author mac
%%%-------------------------------------------------------------------
-module(dhtcp_sup).
-author("mac").

-behaviour(supervisor).

%% API
-export([start_link/5,
  name/1
  ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================


start_link(Port, SockOpt, NAccepter, Mfa, Mode) ->
  supervisor:start_link({local, name(Port)}, ?MODULE, [Port, SockOpt, NAccepter, Mfa, Mode]).

name(Port) -> list_to_atom("dhtcp_sup" ++ integer_to_list(Port)).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


init([Port, SockOpt, NAcceptor, Mfa, Mode]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  {ok, LSock} = gen_tcp:listen(Port, SockOpt),
  %%
  Acceptors = [{ {dhtcp_acceptor:name(Port,X), X}, {dhtcp_acceptor, start_link, [Port, LSock,X]}, Restart, Shutdown, worker, [dhtcp_acceptor] } ||
    X <- lists:seq(1, NAcceptor)],

  ConnSup = {dhtcp_conn_sup:name(Port), {dhtcp_conn_sup, start_link, [Port, Mfa, Mode]}, Restart, Shutdown, supervisor, [dhtcp_conn_sup]},
  {ok, {SupFlags, Acceptors ++ [ConnSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
