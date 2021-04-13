## sys_corss

中央服与游戏服都会有sys_corss进程，它可以保证俩个节点的连接是稳定的。



### 进程状态

```erlang
-record(state, {
    center
}).
```

center字段用于存储中央服节点。



### 初始化

```erlang
init([]) ->
    Local = node(),
    Center = sys_env:get_env(center),
    case Local =:= Center of
        true -> ok;
        _ ->
            ?MODULE ! {connect_center, Center}
    end,
    net_kernel:monitor_nodes(true),
    State = #state{center = Center},
    {ok, State}.
```

如果是中央服节点，只订阅节点状态变化消息。

非中央服节点不仅订阅节点状态变化消息，同时进程也想自身发送一条connect_center消息。



### connect_center消息

```erlang
handle_info({connect_center, Node}, State) ->
    case net_kernel:connect_node(Node) of
        true -> 
            ?INFO_MSG("center connect success ~w~n", [Node]);
        _Err ->
            erlang:send_after(10000, self(), {connect_center, Node})
%%            ?INFO_MSG("center connect fial ~w:~w~n", [Node, _Err])
    end,
    {noreply, State};
```

子节点向中央服节点建立连接，如果连接建立失败，会10秒后重新尝试建立。



### nodeup

```erlang
handle_info({nodeup, Node}, State) ->
%%    cross_gdsyxw:send_all(Node),
    ?INFO_MSG("~w connect success ~n", [Node]),
    {noreply, State};
```





### nodedown

```erlang
handle_info({nodedown, Node}, State = #state{center = Node}) ->
    ?INFO_MSG("~w disconnect ~n", [Node]),
    erlang:send_after(10000, self(), {connect_center, Node}),
    {noreply, State};
handle_info({nodedown, Node}, State) ->
    ?INFO_MSG("~w disconnect ~n", [Node]),
    {noreply, State};
```

如果子节点监控到中央服节点断开，则重新建立连接。

对于中央服节点，自己与自己是不会断开连接的，后续要看下分布式内容。。。



这里有疑问？子节点不会断开连接么？这里的断开是什么意思？是取消消息订阅了还是俩个节点之间某个断开连接了。

