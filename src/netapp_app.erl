%%%-------------------------------------------------------------------
%% @doc netapp public API
%% @end
%%%-------------------------------------------------------------------

-module(netapp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    netapp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
