sys_conn模块负责读写客户端链接的数据，它是客户端连接的一种映射。当客户端与服务器建立握手成功后就会建立连接，这是会实例化一个连接进程来处理客户端Socket。



### conn.hrl

conn.hrl包含conn记录，它存储了连接进程相关信息。

```erlang
-record(conn, {
               %% 外部进程?
               object,
               
               %% 受控对象的进程
               pid_object,
               
               type = tester      ::game_server | monitor | tester,
               account = "",
               platform = "",
               zone_id = 0,
               role_id = 0,
               
               %% 客户端Socket
               socket,
               %% 客户端连接IP
               ip = {0, 0, 0, 0},
               %% 客户端连接端口
               port,
               %% 连接时间
               connect_time = {0, 0, 0},
               
               %% 已接受消息的数量
               recv_count = 0,
               last_recv_count = 0,
               
               %% 已发送的消息数量
               send_count = 0,
               %% 发送错误次数
               error_send = 0,
               
               %% 客户端发送错误数据包的个书
               bad_req_count = 0,
               
               %% 内部循环计数器
               loop_counter = 0,
               %% 包体长度
               length = 0,
               %% 序号
               seq = 0,
               
               %% read_head用于表示解析数据包的步骤
               %% false表示尚未读取，true表示读取中，要解析数据包长度；ok表示解析完毕。
               %% 其实解析数据包就三个步骤：读取包头；解析数据包长度；解析完毕，开始读取数据包。
               %% 我觉得使用三种状态来表示更好：read_head -> parse_payload_len -> read_data
               read_head = false,
               
               %% 托管账号
               custodian = "",
               %% 托管角色ID
               custodian_id,
               
               debug = false,
               cmd = 0,
               data_status = 0,
               flag = 0,
               
               %% 数据帧用额外字节表示数据长度，len表示该额外字节的bit位数
               len = 0,
               
               %% 帧类型
               open_code = 0
              })
```



### 创建连接进程

在第一次握手成功后就会调用create/2函数创建连接进程。

```erlang
create(Socket, Ip, Port) ->
    gen_server:start(?MODULE, [Socket, Ip, Port], []).
```

create/3函数创建一个connector进程去处理socket，但是不与调用函数的进程建立连接关系。



```erlang
init([Socket, Ip, Port]) ->
    process_floag(trap_exit, true),
    
    self() ! read_next,
    erlang:send_after(60000, self(), client_check),
    erlang:send_after(300000, self(), account_check),
    erlang:send_after(180000, self(), loop),
    
    State = #conn{socket = Socket, ip = Ip, port = Port},
    {ok, State}.
```

接着初始化连接进程，init/1在初始化时，会立刻向自身发送一条read_next消息，表示读取数据包头。接着会开启三个向自身发送消息的定时器，后面再分析这3条消息的定时器用处。



### 读取包头

```erlang
handle_info(read_next, State) ->
    read_next(State).

%% conn记录的read_head = false才匹配
read_next(State = #conn{socket = Socket, recv_count = RecvCount, read_head = false}) ->
    prim_inet:async_recv(Socket, 2, 60000),
    {noreply, State#conn{recv_count = RecvCount + 1, read_head = true}};
%% 匹配所有conn记录的参数
read_next(State) ->
    {noreply, State}
```

red_next便是读取第一个数据帧。

State参数是conn记录，read_head字段默认是false，表示尚未读取包头，当客户端传递第一个数据帧的时候就会匹配第一个read_next/1函数。

接着异步读取2个字节的数据，这2个字节的数据会以一条{inet_async, Socket, Ref, {ok, Data}}格式的消息发给连接进程自己处理。





### 解析包体长度

```erlang
%% 包体长度的检测
handle_info({inet_async, _Socket, _Ref, {ok, _Bin = <<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, Len:7>>}}, State = #conn{role_id = Account, read_head = true}) when Len > ?MAX_LEN ->
    {stop, normal, State};
%% 包体长度的解析
handle_info({inet_async, _Socket, _Ref, {ok, _Bin = <<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, Len:7>>}}, State = #conn{read_head = true}) ->
    case Opcode of
        %% Opcode = 0，则read_head = ok，猜测是读取包头成功的意思
        0 ->
            {noreply, State#conn{read_head = ok}};
        %% Opcode != 0，则进一步处理
        _ ->
            case Len of
                126 ->
                    ;
                127 ->
                    ;
                _ ->
                    ;
            end
    end;
```

这部分代码是对WebSocket数据帧包体长度的解析。





### 处理数据包

接下来开始处理包体数据。

```erlang
handle_info({inet_async, _Socket, _Ref, {ok, Bin = <<_Masking:4/binary, _Payload/binary>>}}, State = #conn{length = Len, open_code = Opcode, bad_req_count = Count, read_head = false, role_id = Account, debug = _Debug}) ->
    %% 对数据帧进行解析，得到包体数据
    DataBin = packet:unpacket(Bin, Len),
    
    case Opcode of
        2 ->
            case DataBin of
                ...
            end;
        _ ->
            %% 非2进制数据帧直接停止进程。
            {stop, normal, State}.
    end.
```

上述的代码会对数据帧进行解析，得到包体数据。接着处理前端传递过来的包体数据。

包体数据的结构如下：

```erlang
DataBin = <<DataSize:16, DataStatus:8, Flag:32, Cmd:16, Data/binary>>
```

- DataSize

  前端传递数据的长度

- DataStatus

  数据状态

- Cmd

  Cmd决定路由的模块和要执行的方法，服务器会将Cmd除以100并取整，根据结果决定要执行的模块。比如说Cmd是1001，除以100取整得10，所以执行10所对应的模块。

  同时Cmd自身也决定要匹配模块的哪个方法，比如1001就是执行微信登陆的方法。

- Flag

  标志位

- Data

  前端传递的数据。

```erlang
case DataBin of
	<<DataSize:16, DataStatus:8, Flag:32, Cmd:16, Data/binary>> ->
        case DataSize of
            %% 数据长度为0的处理
            0 ->
                ...;
            %% 数据长度非0的处理
            _ ->
                %% 这一步应该是profubber解包
                case catch packet:unpack(Cmd, DataStatus, Data) of 
                    {ok, Cmd, Data1} ->
                        %% 根据Cmd来进行路由，选择要执行的模块和函数，根据返回的结果进行处理
                        case router:handle(Cmd, Data1, Flag, State) of
                            ok -> 
                                read_next(State);
                            {ok, NewState} ->
                                read_next(NewState);
                            %% 需要响应客户端
                            {ok, Reply, NewState} ->
                                pack_send(self(), Cmd, Flag, Reply),
                                read_next(NewState);
                            %% 处理出错，响应错误给客户端
                            {false, Reason} ->
                                pack_send_error(self(), Cmd, Flag, Reason),
                                read_next(State);
                            _Error ->
                                read_next(State);
                        end;
                    _Error ->
                        {stop, normal, State}
                end
        end;
	_ ->
        ?ERR("~w发送空包", [Account]),
        {stop, normal, State}.
end.
```

解析客户端传递的包体数据后，把Cmd、DataStatus、Data等数据传递给router模块，由router模块决定要调用的模块和函数（或者说协议）。

router模块执行完毕后会根据返回的结果，决定是否响应数据给客户端。接下来分析pack_send/4函数，看看如何给客户端发送数据。

```erlang
pack_send(ConnPid, Cmd, Flag, Data) ->
    case catch packet:pack(Cmd, Data, Flag) of
        {ok, Bin} ->
            %% 从进程字典获取东西???
            case get(send_buff) of
                undefined ->
                    %% 获取失败则直接发送数据
                    ConnPid ! {tcp_send, Bin};
                List ->
                    %% 获取成功存储到进程字典
                    put(send_buff, List ++ [Bin])
            end;
        Err ->
            ?ERR("打包数据出错[Cmd: ~w][Err: ~w][Data: ~w][stacktrace:~w]", [Cmd, Err, Data, util:get_stacktrace()])
    end.
```

当一个请求需要响应数据时，就会调用pack_send/4函数。

首先根据协议号打包数据，接着先连接进程自身发送一个tcp_send消息，由handle_info()来处理。

```erlang
handle_info({tcp_send, Bin}, State = #conn{role_id = Account, socket = Socket, send_count = SendCount, error_send = ErrSend, debug = _Debug}) ->
    NewBin = packet:packet(Bin),
    case catch erlang:port_command(Socket, NewBin, [nosuspend]) of
        true ->
            {noreply, State#conn{send_count = SendCount + 1}};
        false ->
            {noreply, State#conn{error_send = ErrSend + 1}};
        Else ->
            ?ERR("帐号[~w]发送socket数据失败:~w", [Account, Else]),
            {noreply, State#conn{error_send = ErrSend + 1}}
    end;
```

在发送数据时，由于使用的是WebSocket协议，因此先要将数据打包成符合WebSocket协议的数据包然后再发送。发送数据是使用erlang:port_command/3函数，具体使用看说明。当发送完数据后会继续处理下一个数据包。

至此，我们已经分析完服务端是如何处理一个链接，是如何处理一个数据包，如何响应一个数据包的过程了。







### 连接进程的活跃状态

client_check消息用于验证连接的活跃状况，主要是通过在一定时间内检测连接接收的数据包是否有增加来判断用户是否活跃的。

我们会用俩个字段来实现：

- recv_count

  当前接收数据包的个数，连接进程每次接收到数据包都会累计加1，可以在read_next函数中看到。

- last_recv_count

  默认0，当在检测连接活跃状态时，last_recv_count会存储当前连接接收的数据包个数（recv_count）

连接进程每60秒向自身发送一条检测消息，如果发现当前进程接收的数据包个数（recv_count）大于上次检测时接收的数据包个数（last_recv_count)，那么就表示在这段时间内用户一直是停止状态的，这时候就断开连接进程。

```erlang
%% 在初始化连接进程的时候会向自身发送一条消息
erlang:send_after(60000, self(), client_check).

%% 客户端状态检查，如果客户端一定时间内不发送指令则认定为已断线
handle_info(client_check, State = #conn{role_id = _Account, recv_count = RecvCount, last_recv_count = LastRecvCount}) ->
    case RecvCount > LastRecvCount of
        true ->
            erlang:send_after(60000, self(), client_check),
            {noreply, State#conn{last_recv_count = RecvCount}};
        false ->
%%            ?ERR("[~w]的客户端长时间未发送指令，可能已经断线", [_Account]),
            {stop, normal, State}
    end;

read_next(State = #conn{socket = Socket, recv_count = RecvCount, read_head = false}) ->
    prim_inet:async_recv(Socket, 2, 60000),
    {noreply, State#conn{recv_count = RecvCount + 1, read_head = true}}.
```



### 验证身份

```erlang
%% 检查是否已经验证了身份
handle_info(account_check, State = #conn{role_id = 0, ip = Ip}) ->
%%    ?ERR("客户端[~w]连接后长时间未登录帐号，强制断开连接", [Ip]),
    {stop, normal, State};
```

每180秒检查一次。



### 停止连接进程

- WebSocket断开连接，那么会收到下面的消息。

```erlang
handle_info({inet_async, _Socket, _Ref, {error, closed}}, State = #conn{role_id = Account}) ->
    {stop, normal, State}.
```

- 超时

```erlang
handle_info(timeout, State) ->
    {stop, normal, State};
```

- 关联进程（玩家进程）正常退出

```erlang
handle_info({'EXIT', Pid, normal}, State = #conn{pid_object = Pid, role_id = _Account}) ->
    {stop, normal, State};
```

- 关联进程（玩家进程）异常退出

```erlang
handle_info({'EXIT', Pid, _Why}, State = #conn{role_id = _Account, pid_object = ObjectPid}) when Pid =:= ObjectPid ->
    {stop, normal, State};
```



当向进程发送stop响应时，因为连接进程被设置为系统进程，所以当退出时会执行terminate函数，在这里只是简单的关闭客户端socket。

```erlang
terminate(_Reason, #conn = {socket = Socket}) ->
    catch gen_tcp:close(Socket),
    ok.
```





### gc处理














