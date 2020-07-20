OTP

开放电信平台，它包含：

- 一组库和实现方式
- 实现了强大的工具，例如Web服务器，FTP服务器等。
- 包含构建电信应用程序的工具，实现了电信行业常用的协议。



OTP行为

行为封装了常见的行为模式，可以把它看作一个用回调函数作为参数的应用程序框架。









服务器框架的代码模式是相似的，比如：

- 启动服务的start()函数
- 服务器loop()主体函数，它会一直接收消息和处理消息，同时会有一个State参数存储服务器的处理结果。
- rpc()函数，用于向服务器自己发送消息

不同的点在于处理消息的逻辑，这部分可以定义成行为函数，由外部回调模块代码来解决。



一般会定义以下行为代码：

- init()：该函数负责回调模块的初始化，同时返回存储处理消息结果的数据结构。

- handle()：处理消息的逻辑代码

以及向服务器发送消息的代码，这部分代码并没有具体的函数名字。我们每定义一个新的消息格式的代码，handle()对应要有处理该消息格式的代码。

example：服务端代码

```erlang
-module(server3).
-export([start/2, rpc/2, swap_code/2]).

%% 实现热更新的server
start(Name, Module) ->
	register(Name, spawn(fun() -> loop(Name, Module, Module:init()) end)).


swap_code(Name, NewMod) ->
	rpc(Name, {swap_code, NewMod}).


rpc(Name, Request) ->
	Name ! {self(), Request},
	receive
		{Name, Response} -> Response
	end.


%% 服务器主体代码
loop(Name, Module, State) ->
	receive
		{From, {swap_code, NewMod}} ->
			From ! {Name, ack},
			loop(Name, NewMod, State);
		{From, Request} ->
			{Response, NewState} = Module:handle(Request, State),
			From ! {Name, Response},
			loop(Name, Module, NewState)
	after 300 * 1000 ->
			  timeout
	end.
```

可以看到rpc与loop是对应的。



example：回调模块代码

```erlang
-module(name_server).
-import(server3, [rpc/2]).
-export([add/2, find/1, init/0, handle/2]).

%% 客户端方法
add(Name, Place) ->
	rpc(name_server, {add, Name, Place}).

find(Name) ->
	rpc(name_server, {find, Name}).


%% 对应的回调方法
init() ->
	dict:new().

handle({add, Name, Place}, Dict) ->
	{ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict) ->
	{dict:find(Name, Dict), Dict}.
```





gen_server是不断优化服务器代码形成的成果，它是上述案例的加强版。

```erlang
terminate(_Reason, _State) -> ok
```

服务器即将终止时将会调用的接口，它是Moudle:init/1的逆操作，并进行必要的清理。

无论是服务器崩溃并生成{'Exit', reason}，还是由handle_开头的函数返回一个{stop, Reason, NewState}，都会调用该接口。



```erlang
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

