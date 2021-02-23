## setting_mgr

setting_mgr.erl是游戏设置进程。



### 进程状态

```erlang
-record(state, {
	item,
    num,
	all_num
}).
```

进程状态的record。



```erlang
-record(setting, {
    type = 0,
    value
}).
```

setting record。





### init

```erlang
init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    
    %% 初始化setting ets表
    ets:new(setting, [set, named_table, public, {read_concurrency, true}, {keypos, #setting.type}]),
    %% 打开setting dets表
    dets:open_file(setting, [{file, "./dets/setting.dets"}, {keypos, #setting.type}, {type, set}]),
    
    %% 白名单ets表
    ets:new(white_role, [set, named_table, public, {read_concurrency, true}, {keypos, #white_role.role_id}]),
    %% 白名单dets表
    dets:open_file(white_role, [{file, "./dets/white_role.dets"}, {keypos, #white_role.role_id}, {type, set}]),
    
    %% 黑名单ets表
    ets:new(black_role, [set, named_table, public, {read_concurrency, true}, {keypos, #white_role.role_id}]),
    %% 黑名单dets表
    dets:open_file(black_role, [{file, "./dets/black_role.dets"}, {keypos, #white_role.role_id}, {type, set}]),

    %% 用dets表的数据填充ets表
    ets:from_dets(setting, setting),
    ets:from_dets(white_role, white_role),
    ets:from_dets(black_role, black_role),

    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.
```



### terminate

```erlang
terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭....", [?MODULE]),

    %% 使用磁盘（dets）存储ets setting表
    ets:to_dets(setting, setting),
    %% 关闭dets setting表
    util:close_dets(setting),

    %% 将dets white_role表的所有对象删除
    dets:delete_all_objects(white_role),
    %% 再将内存中的white_role表对象存储到dets white_role表中
    ets:to_dets(white_role, white_role),
    %% 关闭dets white_role表
    util:close_dets(white_role),
    
    dets:delete_all_objects(black_role),
    ets:to_dets(black_role, black_role),
    util:close_dets(black_role),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.
```

进程结束时需要将内存中的数据





## 进程方法

### set

```erlang
set(Type, Value) ->
    ets:insert(setting, #setting{type = Type, value = Value}).
```



### get

```erlang
get(Type) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> {ok, Value};
        _ -> false
	end.
```



### web_get

```erlang
%% ?setting_acc_sign是28，表示签到状态
web_get(Type = ?setting_acc_sign) ->
    case ets:lookup(setting, Type) of
        [#setting{value = {_, Value}}] -> [{result, Value}];
        _ -> [{result, 0}]
    end;
```

获取签到配置。

```erlang
%% ?setting_acc_sign_reward是29，表示签到配置列表
web_get(Type = ?setting_acc_sign_reward) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> 
            %% Value是p_sign_reward record的列表，这里做了列表解析，转换成后台需要的数据
            NewList = [[Day, mail_mgr:to_integer_assets(Type1), Num] || #p_sign_reward{id = Day, list = [#p_assets{type = Type1, num = Num}]} <- Value],
            [{result, NewList}];
        _ -> [{result, []}]
    end;
```

获取签到配置列表。





### web_set

```erlang
web_set(Type = ?setting_acc_sign, Value) ->
    ets:insert(setting, #setting{type = Type, value = {date:unixtime(), Value}}),
    true;
```

?setting_acc_sign是28，这里是设置签到开启状态。

Value是一个{Time, Status}的元组，Time是设置时间，Status是开启状态。



```erlang
web_set(Type = ?setting_acc_sign_reward, Value) ->
    NewList = [#p_sign_reward{id = Day, list = [#p_assets{type = mail_mgr:to_atom_assets(Type1), num = Num}]} || [Day, Type1, Num] <- Value],
    ets:insert(setting, #setting{type = Type, value = NewList}),
    true;
```







### add_black_role

add_black_role函数用于添加黑名单。

```erlang
%% 设置黑名单
add_black_role(RoleId) ->
    case role_data:get_online_role(RoleId) of
        %% 玩家在线的处理
        {ok, #online_role{pid = Pid, name = Name}} ->
            %% 向该玩家进程发送一个异步的apply_add_black消息
            role:apply(async, Pid, {?MODULE, apply_add_black, []}),
            %% ets black_role表中插入黑名单数据
            ets:insert(black_role, #white_role{role_id = RoleId, name = Name, time = date:unixtime()}),
            true;
        %% 玩家不在线的处理
        _ ->
            %% 从dets表中获取玩家数据
            case role_data:get_role_from_dets(RoleId) of
                {ok, Role} ->
                    %% 从dets表中获取的玩家数据，必须更新record结构
                    {ok, Role1} = role_var:update_var(Role),
                    
                    %% 将Role1的animal_flag字段改为97，97是加入黑名单处理的意思
                    {ok, NewRole} = apply_add_black(Role1),
                    
                    %% 将黑名单玩家数据保存到dets表中
                    role_data:save(NewRole),

                    %% 在ets表中插入玩家数据
                    ets:insert(black_role, #white_role{role_id = RoleId, name = NewRole#role.name, time = date:unixtime()}),
                    true;
                _ ->
                    false
            end
    end.
```





## record

```erlang
-record(setting, {
	type = 0,	%% 类型
	value
}).
```

- type

  配置类型

- value

  配置类型对应的值