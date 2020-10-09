%%%-------------------------------------------------------------------
%%% @author mac
%%%-------------------------------------------------------------------
-module(dhtcp_conn_sup).
-author("mac").

-behaviour(supervisor).

%% API
-export([start_link/3,
  start_child/1,
  name/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================


start_link(Port, Mfa, Mode) ->
  supervisor:start_link({local, name(Port)}, ?MODULE, [Mfa, Mode]).

start_child(Port) ->
  supervisor:start_child(name(Port), []).

name(Port) -> list_to_atom("dhtcp_conn_sup" ++ integer_to_list(Port)).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


init([Mfa, Mode]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 60,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 2000,
  Type = worker,
  if
    Mode == agent ->
      AChild = {dhtcp_conn, {dhtcp_conn, start_link, [Mfa]}, Restart, Shutdown, Type, [dhtcp_conn]};
    Mode == direct ->
      {M, _, _} = Mfa,
      AChild = {M, Mfa, Restart, Shutdown, Type, [M]}
  end,
  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
