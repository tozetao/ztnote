## log_db进程

log_db进程的原理是使用一个List来存储要处理的日志，通过定时处理这些日志来减少MySQL服务器的压力。



### 进程状态

```erlang
-record(state, {
	list = []	%% {Type, Pool, values}                
})
```

- Type

  待操作的表

- Pool

  数据库操作

- values

  待插入的值，是由一个list转换成插入值的SQL语句。比如：[10086, "lisi", 20]会被转成(10086, "lisi", 20)。



### 初始化

```erlang
init([N]) ->
    process_flag(trap_exit, true),
    case N =< 10 of
        true ->
            erlang:process_flag(min_bin_vheap_size, 1024*1024),
            erlang:process_flag(min_heap_size, 1024*1024),
            erlang:process_flag(priority, high);
        _ ->
            ok
    end,
    Name = list_to_atom(lists:concat(["db_log_", N])),
    register(Name, self()),
    put(self_name, Name),
    State = #state{},
    ?INFO("[~w] 已经启动", [?MODULE]),
    {ok, State}.
```

log_db进程会开启多个，参数N是整数，它是进程标识，会被用于生成进程名。





### log消息

```erlang
handle_info({log, Log}, State = #state{list = List}) ->
    case List of
        %% 当List为空时，会向自身发送一条tick消息
        [] -> 
            erlang:send_after(?time_tick, self(), tick);
        _ -> ok
    end,
    {noreply, State#state{list = [Log | List]}};
```

Log是{Type, Pool, Data}的元组

- Type

  数据的类型，比如jd_card、mail_log等。

- Pool

  操作类型，比如insert

- Data

  将数据转成插入数值的SQL语句，比如：[10086, "lisi", 20]会被转成(10086, "lisi", 20)

log消息主要是向进程自身插入一条要处理的日志，同时在List为空时会设置一个定时器，该定时器出发时是向自身发送一条消息。





### tick消息

```erlang
handle_info(tick, State = #state{list = List}) ->
    %% 处理List中的max_num条记录
    NewList = do_db(?max_num, List, []),
    case List of
        [] -> ok;
        %% List不为空，表示List中仍然有数据要处理，因此再次开一个定时器
        _ ->
            erlang:send_after(?time_tick, self(), tick)
    end,
    {noreply, State#state{list = NewList}};
```

tick消息主要是从进程列表中取出一定数量的元素，批量插入到数据库。



```erlang
do_db(_N, [], List) -> 
    insert_db(List),
    [];
do_db(0, L, List) -> 
    insert_db(List),
    L;
do_db(N, [{Type, Pool, Sql}| L], List) -> 
    NewList = case lists:keyfind(Type, 1, List) of
        %% 从List中寻找操作同一张表的元素，如果找到并且数据库操作类型一致的话，
        %% 那么将当前SQL附加到该元素的SQL列表里面
        {Type, Pool, L1} ->
            lists:keyreplace(Type, 1, List, {Type, Pool, [Sql | L1]});
        _ -> 
            [{Type, Pool, [Sql]} | List]
    end,
    do_db(N -1, L, NewList).
```

参数2是进程存储的列表，即要处理的日志列表。

参数3的List是{Type, Pool, Sql}的元组，Type是表类型，Pool是操作类型，Sql是一个列表，存储着要插入的值列表。





```erlang
insert_db([]) -> ok;
insert_db([{Type, Pool, Values} | L]) ->
%%    ?ERROR_MSG("数据库入库类型:~w,长度:~w", [Type,length(Values)]),
    Sql = sql(Type),
    insert_db(Sql, Pool, Values),
    insert_db(L).

insert_db(Sql, _Pool, Values) ->
    List = string:join(Values, ","),
    Sql1 = util:flist(Sql, [List]),
%%    ?ERR("Sql:~ts", [list_to_binary(Sql1)]),
    case db:exec(list_to_binary(Sql1)) of
        ok -> ok;
        {error,{'EXIT',{mysql_timeout,5000,{}}}} -> ok;
        _Err ->
            ?ERR("mysql sql error:~ts:~w~n", [Sql1, _Err])
    end.
```











## 进程方法

### log/3

```erlang
log(Type, Pool, Data) ->
    %% 生成插入数值的SQL语句
    Data1 = formart_insert_values(Data),
    
    %% 根据Type，决定交给哪个进程处理
    case Type of
        role ->
            db_log_11 ! {log, {Type, Pool, Data1}};
        gold_cost_log ->
            db_log_12 ! {log, {Type, Pool, Data1}};
        mail_log ->
            db_log_12 ! {log, {Type, Pool, Data1}};
        jd_card ->
            db_log_12 ! {log, {Type, Pool, Data1}};
        phone_card ->
            db_log_12 ! {log, {Type, Pool, Data1}};
        pet_egg_log ->
            db_log_12 ! {log, {Type, Pool, Data1}};
        _ ->
            db_log_12 ! {log, {Type, Pool, Data1}}

    end.
```

我们先看京东卡，它是jd_card类型，交给db_log_12进程处理。





