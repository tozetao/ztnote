router.erl

router是路由模块，它将根据Cmd来决定要调用的模块和函数，来处理客户端传递过来的数据包。

```erlang
-module(router).
-export([handle/4]).

-include("conn.hrl").
-include("common.hrl").
-include("error_msg.hrl").

handle(Cmd, Data, Flag, State = #conn{pid_object = Pid}) ->
    case Cmd div 100 of
        10 -> 
            login_rpc:handle(Cmd, Data, State);
        _ -> 
            Mod = get_mod(Cmd),
            Need_Send = case Mod of
                farm_rpc -> 
                    case setting_mgr:get(?setting_farm) of
                        {ok, 1} -> 1;
                        _ -> 0
                    end;
                pet_rpc -> 
                    case setting_mgr:get(?setting_pet) of
                        {ok, 1} -> 1;
                        _ -> 
                            0
                    end;
                reward_rpc -> 
                    case setting_mgr:get(?setting_reward) of
                        {ok, 1} -> 1;
                        _ -> 0
                    end;
                _ ->
                    1
            end,

            case Need_Send of
                1 ->
                    role:handle_rpc(Pid, Mod, Cmd, Data, Flag),
                    ok;
                0 ->
                    {false, ?error_not_open}                    
            end
    end.

%% 获取协议号对应的模块
get_mod(Cmd) ->
    case Cmd div 100 of
        11 ->
            role_rpc;
        12 ->
            rank_rpc;
        13 ->
            animal_rpc;
        14 ->
            mail_rpc;
        15 ->
            area_rpc;
        16 ->
            shop_rpc;
        17 ->
            task_rpc;
        18 ->
            treasure_rpc;
        19 ->
            exchange_rpc;
        20 ->
            activity_rpc;
        21 ->
            animal_zoo_rpc
    end.
```

Cmd是协议号，Cmd除以100取整后的结果决定要调用什么模块，而Cmd自身的值是该模块要调用的协议。



### 调用模块与方法

```erlang
role:handle_rpc(Pid, Mod, Cmd, Data, Flag),
```

- Pid

  pid_object是玩家进程id，当玩家登陆成功后会返回新的State，里面就会包含玩家进程的pid。

- Cmd

  协议号

- Mod

  根据Cmd计算得到的模块名

- Data

  前端传递的数据

该函数会向玩家进程发送rpc消息，消息格式：{rpc, Mod, Cmd, Data, Flag}。接着由玩家进程的handle_info函数处理该消息。

```erlang
handle_info({rpc, Mod, Cmd, Data, Flag}, Role = #role{socket_pid = Pid}) ->
    case Mod:handle(Cmd, Data, Role) of
        ok ->    
            {noreply, Role};
        {ok, NewRole} ->
            do_change(Cmd, NewRole, Role),
            role_data:update_online_role(NewRole),
            {noreply, NewRole#role{need_sync = true}};
        {ok, Reply, NewRole} ->
            do_change(Cmd, NewRole, Role),
            role_data:update_online_role(NewRole),
            sys_conn:pack_send(Pid, Cmd, Flag, Reply),
            {noreply, NewRole#role{need_sync = true}};
        {reply, Reply} ->
            sys_conn:pack_send(Pid, Cmd, Flag, Reply),
            {noreply, Role};
        {false, Reply} ->
            sys_conn:pack_send_error(Pid, Cmd, Flag, Reply),
            {noreply, Role};
        {false, Reply, NewRole} ->
            do_change(Cmd, NewRole, Role),
            role_data:update_online_role(NewRole),
            sys_conn:pack_send_error(Pid, Cmd, Flag, Reply),
            {noreply, NewRole#role{need_sync = true}}
    end;
```

这个函数负责调用协议对应模块与模块方法，返回的结果分别有6种情况：

- ok

  成功，无需任何处理。

- {ok, NewRole}

  成功，需要更新玩家状态。

- {ok, Reply, NewRole}

  成功，需要更新玩家状态并响应前端数据

- {reply, Reply}

  成功，响应前端数据

- {false, Reply}

  失败，响应前端错误数据

- {false, Reply, NewRole}

  失败，需要更新玩家状态并响应前端错误数据。

