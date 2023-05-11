### 控制进程

创建某个套接字的进程被称为该套接字的控制进程。所有来自套接字的消息都会被发送到控制进程。如果控制进程挂了，该套接字就会被关闭。

某个套接字的控制进程可以通过调用gen_tcp:controlling_process(Socket, NewPid)为该套接字指定新的控制进程。



### 套接字模式

erlang的套接字有三种打开模式：主动（active）、被动（passive）和单次主动（active once）。

这是通过gen_tcp:connect()或gen_tcp:listen()的Options参数的active选项来设置的。



#### 主动模式

被动模式通过{active: true}打开。

当一个主动套接字被创建后，它会在收到数据时向控制进程发送{tcp, Socket, Data}消息。控制进程是无法控制这些消息流的。

主动模式是非阻塞式的，使用这种方式的服务器被称为非阻塞式服务器。它的缺点在于客户端可以随意地向服务器发送数据，如果服务器处理消息的速度慢于客户端，那么消息会被积压在消息缓存区，系统可能会异常或者崩溃。



#### 被动模式

被动模式通过{active: false}打开。

当一个被动套接字被创建后，控制进程只能通过gen_tcp:recv(Socket, N)来从这个套接字读取数据。N表示读取N个字节，如果N=0将会读取全部可用字节。

被动模式的套接字是阻塞式的，如果控制进程不从套接字读取数据，客户端发送数据时会阻塞，知道服务器条用recv为止。



### 混合模式

单次主动的套接字可以认为是混合模式，通过{active: once}打开。

单次模式既不是阻塞的也不是非阻塞的，而是俩种模式的混合。设置成单次主动的套接字是主动的，但是只针对一次消息，控制进程在接收一次消息后，必须通过inet:setopts()才能重启下一个消息的接受，在重启之前系统是阻塞的。

```erlang
{ok, Listen} = gen_tcp:listen(Port, [...{active: once}...]),

```





### prim_inet.erl

prim_inet是内部模块，不公开给开发人员使用的。

```erlang
prim_inet:async_recv(Sock, Length, Timeout)
```

async_recv/3是异步接收数据，在gen_server里是这样子写的：

```erlang
handle_info({inet_async, Socket, Ref, {ok, Data}}, State)
```

这俩个函数要配合使用。

prim_inet:async_recv/3函数接收的数据会以一条{inet_async, Socket, Ref, {ok, Data}, State}消息发送给自身进程，需要handle_info()来进行处理。

example：

```erlang
loop(Socket) ->
    prim_inet:async_recv(Socket, 0, 1000),
    receive
        {inet_async, _, _, {ok, Msg}} ->
            io:format("message received ~p~n",[Msg]),
            loop(Socket);
        {inet_async, _, _, {error,timeout}} ->
            io:format("timeout !"),
            catch gen_tcp:close(Socket);
        {fake, Msg} ->
            io:format("Message = ~p~n", [Msg]),
            loop(Socket)
    end.
```





```erlang
port_command(Port, Data, OptionList) -> boolean()
```

发送数据到一个端口。

如果port command被终止，则返回false，否则返回true。如果端口繁忙，则暂停调用进程，直到端口不再繁忙。

- Port

  port() | atom()，端口

- Data

  iodata，输入/输出数据

- Option

  force | nosuspend

- OptionList

  [Option]，选项列表

Options选项说明：

- force选项

  如果端口繁忙则调用进程不会被挂起，而是强制穿过port command执行。如果端口的驱动程序不支持则调用失败，并出现notsup异常。有关更多信息要参考驱动标志[CDATA[ERL_DRV_FLAG_SOFT_BUSY]]。

- nosuspend

  该选项表示如果端口忙，不允许调用进程不会被挂起，而是终止port command执行并返回false。

这里的port是指什么？什么情况下会繁忙。



Failures （失败时的错误信息）

- badarg

  如果端口不是一个开放端口的标识符，或者不是一个开发端口的注册名称。如果调用进程先前链接到由端口标识的关闭端口，则保证在发生此badarg异常之前发送该端口的退出信号。

- badarg

  If Data is an invalid I/O list.如果数据是一个无效的I/O列表。

- notsup

  如果是force选项，但是端口驱动不支持强制穿过一个繁忙的端口





```erlang
erlang:system_monitor(MonitorPid, Options) -> MonSettings
```

Options = [system_monitor_option()]

参数2是监视选项的列表，system_monitor_option()的选项有：

- busy_port

  如果系统中的进程因为发送到繁忙的接口而被挂起，则会向MonitorPid发送消息{monitor, SusPid, busy_port, Port}。SusPid是发送至Port时而被暂时的pid。





