### send_buff_begin

```erlang
send_buff_begin() ->
    case get(send_buff) of
        undefined ->
            put(send_buff, []);
        _ ->
            ok
	end.
```

消息缓冲区。必须在Role进程调用，当调用send_buff_begin()后，必须调用send_buff_flush或send_buff_clean。



### send_buff_flush

```erlang
send_buff_flush() ->
    case get(socket_pid) of
        Pid when is_pid(Pid) ->
            case get(send_buff) of
                List when is_list(List) ->
                    [Pid ! {tcp_send, Data} || Data <- List],
                    put(send_buff, undefined);
                _ ->
                    ok
            end;
        _ ->
            ?ERR("不在Role进程调用")
	end.
```

向客户端推送缓冲区的所有内容，并请空缓冲区。



### send_buff_clean

```erlang
send_buff_clean() ->
    put(send_buff, undefined).
```

请空消息缓冲区。



### 扣除玩家资产

```erlang
%% 扣除指定资产
%% 金币
do_cost_coin(Role = #role{coin = Value}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = coin, num = Value - Cost}]}),
    {ok, Role#role{coin = Value - Cost}};
do_cost_coin(_, _) ->
    {false, ?error_coin}.

%% 钻石
do_cost_gold(Role = #role{gold = Value}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = gold, num = Value - Cost}]}),
    {ok, Role#role{gold = Value - Cost}};
do_cost_gold(_, _) ->
    {false, ?error_gold}.

%% 棒棒糖
do_cost_lollipop(Role = #role{item = Item = #role_item{lollipop = Value}}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = lollipop, num = Value - Cost}]}),
    {ok, Role#role{item = Item#role_item{lollipop = Value - Cost}}};
do_cost_lollipop(_, _) ->
    {false, ?error_item_num}.
```





## problems

1. 扣除玩家资产只是减少了Role进程的数据，数据库数据在什么时候进行更新？

