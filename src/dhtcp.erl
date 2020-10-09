%%%-------------------------------------------------------------------
%%% @author mac
%%%-------------------------------------------------------------------
-module(dhtcp).
-author("mac").

%% API
-export([
  start/5,
  stop/1,
  count/1,
  get_ip/1
]).


%%% @doc
%%% dhtcp模块,
%%% Mode == direct   Mfa 必须返回{ok, pid()}.
%%%          accept 成功后 调用Mfa，发送消息{dhconn_start, Sock},
%%%          模块自己处理sock接受网络数据，tcp_closed tcp_error 信息,
%%%          优点:进程挂在dhconn_sup上面，不用通过agent中转数据，效率高，使用用gate，login
%%%          等逻辑非常简单的服务，特别是gate
%%%          缺点: 控制不方便

%%% Mode == agent 使用默认的dhtcp_conn模块为agent,
%%%          首次收到数据，调用mfa, 发送消息 {dhconn_start, Pid}(Pid为dhtcp_conn的进程号)
%%%          tcp层接收到数据,发送{dhtcp, pid, Data}到目标进程
%%%          优点：进程可以挂在自己的sup上，重启，退出都可以控制，适用范围为逻辑服务,
%%%          缺点：数据通过agent进程转发到logic进程，性能损失
%%% @end

-spec start(integer(), gen_tcp:option(), integer(), mfa(), direct | agent) -> {ok, pid()}.
start(Port, SockOpts, NAcceptor, Mfa, Mode) ->
  ChildSpec = {dhtcp_sup:name(Port), {dhtcp_sup, start_link, [Port, SockOpts, NAcceptor, Mfa, Mode]},
    permanent, 2000, supervisor, [dhtcp_sup]},
  supervisor:start_child(netapp_sup, ChildSpec).


count(Port) ->
  C = supervisor:count_children(dhtcp_conn_sup:name(Port)),
  proplists:get_value(active, C).

stop(Port) ->
  supervisor:terminate_child(netapp_sup, dhtcp_sup:name(Port)),
  supervisor:delete_child(netapp_sup, dhtcp_sup:name(Port)).


-spec get_ip(gen_tcp:socket()) -> list().
get_ip(Sock) ->
  case inet:peername(Sock) of
    {ok, {{X1, X2, X3, X4}, _}} ->
      Fun = fun(X, Y) ->
        if
          Y == "" -> Y ++ integer_to_list(X);
          true -> Y ++ "." ++ integer_to_list(X) end end,
      lists:foldl(Fun, "", [X1, X2, X3, X4]);
    _ -> undefined
  end.




