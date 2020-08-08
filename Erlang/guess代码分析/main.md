项目应用分析

> erl -pa ebin -s main start

该命令将调用main模块的start/0函数，启动游戏应用以及所依赖的其他应用。



### main.erl

main.erl不仅是项目的入口文件，也是游戏的主体应用。它有以下接口：

```erlang
-spec start()
```

项目的启动函数，它会启动main（游戏主体）应用和依赖的其他应用。



以下是main application行为函数的实现：

```erlang
-spec start(_, _) -> {ok, Pid} | {error, reason}
```

start/2函数负责启动督程，督程由main_sup.erl文件实现。

```erlang
-spec stop(_State)
```

关闭应用。





### main_sup.erl

主督程模块，主要开启4个服务进程，分别是：

- sys_env

  系统环境进程，提供了管理系统环境配置的接口

- sys_code

  热更新进程

- sys_rand

  随机种子进程

- sys_boot

  系统引导进程



### sys_boot.erl

系统引导模块，这是一个gen_server模块。它会负责启动游戏的各个服务进程，并将这些子进程添加到main_sup督程之下。

```erlang
init([Type]) ->
    State = #state{},
    db:init(),
    self() ! {init, Type},
    {ok, State}.
```

init()函数会初始化数据库，接着向自身发送一条init消息，在handle_info()函数中进行初始化的操作。

- [Type]

  Type是命令行附加的额外参数，目前Type的值是原子local。用于从services模块获取服务配置列表（services:config(local)）。

handle_info()函数会调用services模块，获取要启动的服务列表并启动这些服务。



services.erl

服务配置模块。

该模块存储要启动的服务进程列表，同时也保存各个服务进程的配置信息。

```erlang
-spec config(atom()) -> {ok, list()} | {error, undefined}.
```

config/1函数会返回一个元组列表，每个元素都是{Id, Args}的元组，id是服务进程的标识，Args是存储着一个原子元素的列表，用于标识进程类型（本地或中央服进程）。









## 服务进程模块

可以认为游戏的主体应用是由各个服务进程构成的，以下是各个服务进程模块的分析。



### sup_acceptor.erl

sup_acceptor.erl是服务进程模块，由main_sup督程管理。

它是一个simple_one_for_one的字督程，定义了初始化sys_acceptor模块的内容。

注：sys_acceptor模块并不会立即初始化。





### sys_acceptor.erl

sys_acceptor模块是gen_server实现，该模块主要监听Listen Socket是否有新的连接建立，并处理该连接。

sys_acceptor进程虽然是sup_acceptor督程的子进程，当时它并不是由sup_acceptor督程启动的，而是由sys_listen进程启动的。

```erlang
init([ListenSocket]) ->
    self() ! loop,
    {ok, {ListenSocket}}.

handle_info(loop, State = {ListenSocket}) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            inet:setopts(Socket, [{active, once}]),
            gen_tcp:controlling_process(Socket, spawn(fun() -> accept(Socket) end));
		{error, _Reason} ->
    		ignore.
	end,
    
    self() ! loop,
    
    {noreply, State}.
```

处理一个新连接的逻辑大体如上，监听Listen Socket，当有新的连接时，将该链接对应的Socket模式设置为once模式，同时创建一个新的进程执行accept()函数来处理它，并将该进程设置为Socket的控制进程。

接着循环处理下一个请求。

```erlang
accept(Socket) ->
    receive
        {tcp, Socket, HeaderData} ->
            %% HTTP协议是以\r\n作为换行的，因此使用\r\n来进行分隔，得到每一行请求头
            HeaderList = binary:split(HeaderData, <<"\r\n">>, [global]),
            
            %% 请求头是以Key: Value的形式组成的，因此对每个请求头以": "进行分隔，再转成元组
            %% 得到一个元素是元组{Key, Value}形式的列表
            HeaderList1 = [list_to_tuple(binary:split(Header, <<": ">>)) || Header <- HeaderList],
            
            %% 找到Sec-WebSocket-Key请求头的值
            case lists:keyfind(<<"Sec-WebSocket-Key">>, 1, HeaderList1) of
                false ->
                    gen_tcp:close(Socket);
                {_, SecWebSocketKey} ->
                    %% 做特定的加密
                    Sha1 = crypto:hash(sha, [SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
                    Base64 = base64:encode(Sha1),
                    
                    %% 组成响应头
                    Handshake = [
                        <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                        <<"Upgrade: websocket\r\n">>,
                        <<"Connection: Upgrade\r\n">>,
                        <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
                        <<"\r\n">>
                    ],
                    gen_tcp:send(Socket, Handshake),
                    
                    %% 完成握手后创建一个新的连接进程来处理该socket
                    create_conn(Socket)
            end;
        _Else ->
            gen_tcp:close(Socket)
	end.
```

accept函数是被作为一个进程启动并执行的，它会监听连接Socket，处理请求发来的数据。

accept/1函数会处理WebSocket协议的第一次握手，客户端会发起一次HTTP GET请求，并携带SecWebSocketKey请求头。服务端要做的就是解析出Sec-WebSocket-Key请求头的值，再进行加密返回给客户端，完成握手的验证。







### sys_listener.erl

sys_listener.erl模块是gen_server实现。它主要负责监听端口创建Listen Socket，然后创建多个acceptor来侦听Listen Socket。

```erlang
-define(acceptor_num, 10).

init([]) ->
    process_flag(trap_exit, true),
    Port = sys_env:get_env(port),
    
    case gen_tcp:listen(Port, [...]) of
		{ok, Sock} ->
			start_acceptor(?acceptor_num, Sock),
			{ok, state};
		{error, Reason} ->
    		{stop, listen_failure, state}
    end.
```

将sys_listener进程设置为系统进程，接着监听端口，开启多个acceptor去监视Listen Socket，判断是否有新的请求进来。

```erlang
start_acceptor(0, _) -> ok;
start_acceptor(N, ListenSocket) ->
    supervisor:start_child(sup_acceptor, [ListenSocket]),
    start_acceptor(N - 1, ListenSocket).
```

在这里会给sup_acceptor督程添加N个sys_acceptor子进程，用于监听Listen Socket。



































问题：

- 分析玩家登陆
- 分析充值实现
- 分析打动物实现
- profubber的使用
- 自己实现一个聊天服务器，包含充值功能。
- sys_listener模块和sys_conn模块为什么是系统进程?
