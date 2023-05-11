OTP

开放电信平台，它包含：

- 一组库和实现方式
- 实现了强大的工具，例如Web服务器，FTP服务器等。
- 包含构建电信应用程序的工具，实现了电信行业常用的协议。



### OTP行为

行为封装了常见的行为模式，可以把它看作一个用回调函数作为参数的应用程序框架。

我们从最常见的客户端与服务端实现来分析OTP行为。

在erlang中服务器框架的代码模式是相似的，比如：

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



### gen_server

gen_server是不断优化服务器代码形成的成果，它是上述案例的加强版。



#### 启动gen_server

```erlang
start_link(ServerName, Module, Args, Options) -> Result
```

创建一个gen_server进程作为监督树的一部分。这个函数将直接或间接被监督者调用，它可以确保gen_server进程与督程建立链接。

gen_server进程将会调用Module:init/1来初始化。为了保证启动过程是同步的，start_link并不会立即返回，只有当Module:init/1执行完毕才返回。



ServerName参数说明：

- ServerName = {local, Name}

  gen_server进程将会在本地（当前节点）使用register/2注册为Name（可以直接通过Name来发送消息）。

- ServerName = {global, GlobalName}

  将会使用global:register_name/2将gen_server进程id在全局范围内注册为GlobalName。如果没有名称则不注册gen_server进程。

- ServerName = {via, Module, ViaName}

  gen_server进程将使用模块的导出的注册回调函数进行注册。

  Module要导出register_name/2、unregister_name/1、whereis_name/1和send/2函数，这些函数的行为要与global模块中的相应函数一致。



Module是回调模型的名字。

Args是任意数据项，它将会作为参数传递给Module:init/1。

Options是初始化选项：

- {timeout, Time}

  如果存在该选项，则允许gen_server进程花费Time毫秒进行初始化，否则进程将被终止，同时start函数会返回{error, timeout}。

- {hibernate_after, HibernateAfterTimeout}

  该选项会使进程在HibernateAfterTimeout毫秒内等待任何消息，如果没有接收到消息则进程会进入睡眠状态。

- {debug, Dbgs}

  略

- {spawn_opt, Sopts}

如果gen_server进程被成功创建并初始化，函数返回{ok,Pid}，其中Pid是gen_server进程的pid。如果指定ServerName的进程已经存在，函数返回{error,{already_started,Pid}}，其中Pid是该进程的pid。

如果Module:init/1以Reason失败，函数返回{error,Reason}。如果Module:init/1返回{stop,Reason}或ignore，则进程被终止，函数分别返回{error,Reason}或ignore。



```erlang
Module:init(Args) -> Result
```

当gen_server进程启动时就会调用该函数来初始化。

Args参数是start函数的Args参数提供的。

如果初始化成功，该函数返回{ok, State}，{ok, State, Timeout}，{ok, State, hibernate}，State是gen_server进程的内部状态。



Timeout说明：

如果提供了一个整数的超时值，除非在超时时间内接收到一个请求或消息，否则将会发生超时。

当发生超时的时候，gen_server进程会向自身发送一条超时消息，超时消息由原子timeout表示，它会被handle_info/2回调函数处理。

如果返回值Timeout是infinity，gen_server进程会无限期的等待消息，infinity也是Timeout的默认值。



hibernate说明：

如果指定了hibernate，那么在等待下一个消息到达时，进程就会进入休眠状态（通过调用proc_lib:hibernate/3）。



如果初始化失败，函数要返回{stop,Reason}，其中Reason为任意项，或者忽略。











#### 同步请求

```erlang
call(ServerRef, Request) -> Reply
call(ServerRef, Request, Timeout) -> Reply
```

向ServerRef进程发起一个同步请求，并等待进程的回复或者超时（有传递Timeout参数）。gen_server进程通过调用Module:handle_call/3来处理该请求。

ServerRef可以是：

- 进程pid
- 进程在本地注册的Name
- {Name, Node}，进程是在其他节点注册的名字
- {global, GlobalName}，进程是在全局注册的名字
- {via, Module, ViaName}，这个选项不是很懂。

Request可以是任意数据项，它将会传递给Module:handle_call/3函数。

Timeout指定等待回复的时间，它可以一个大于0的整数或原子infinity，单位是毫秒。

默认超时是5000（5秒），如果在指定时间内没有收到回复则调用失败。如果调用者失败后继续运行，而服务器只是延迟了回复，那么回复会在后面的任意时间发送到调用者的消息队列中。

因此调用者对此消息要有准备，并丢弃任何这类型的消息，这些垃圾消息是一个拥有俩个元素的元组，且第一个元素是应用{Ref, Data}。

返回值Reply定义在Module:handle_call/3的返回值中。



```erlang
Module:handle_call(Request, From, State) -> Result
```

每当进程收到使用call/2,3或mulit_call/2,3,4函数发出的请求时，都会调用该函数来处理请求。

Request参数是call/2,3提供的Request参数。

From是一个{Pid, Tag}元组，Pid是客户端的pid，Tag是一个唯一的标识。

State参数表示进程的内部状态。



Result返回值说明：

- {reply, Reply, NewState}

- {reply, Reply, NewState, hibernate}

- {reply,Reply,NewState,Timeout}

  如果返回上面结果，Reply将作为返回值反馈给From，然后进程继续执行可能更新的内部状态NewState.

  For a description of Timeout and hibernate, see **Module:init/1**.

- {noreply,NewState}

- {noreply,NewState,Timeout}

- {noreply,NewState,hibernate}

  如果返回上面结果，则进程会继续更新内部状态，任何对From的回复必须使用reply/2（也就是说这种返回值是配合reply/2使用的）。

- 如果返回{stop, Reason, Reply, NewState}，则将Reply反馈给From。

- 如果返回{stop, Reason, NewState}，则必须使用Reply/2显式指定对From的任何答复。然后gen_server进程调用Module:terminate(Reason, NewState)并终止。





#### 异步请求



#### 处理自发性消息

自发性消息指的是没有通过call()和cast()来发送请求的消息。

比如gen_server链接到另外一个进程并捕捉到退出信号，gen_server进程就会收到一个{'EXIT', Pid, Why}错误信息，这时候可以使用handle_info/2来处理该消息。

```erlang
handle_info({'EXIT', Pid, Reason}, State) ->
    ..code to handle exits here..
    {noreply, State1}.
```



函数说明：

```erlang
handle_info(Info, State) -> Result
```

这个回调是可选的，所以回调模块不需要导出它。gen_server模块提供了这个函数的默认实现，它可以记录意外的Info消息，丢弃它并返回{noreply, State}.

当发生超时或接收到除同步或异步请求以外的任何消息时，gen_server进程就会调用该函数。

Info可以是原子timeout（如果发生超时）或接收到的消息。

- Info

  timeout | term()

- State

  term()

Result返回值参考Module/handle_call/3。







#### 停止gen_server

如果gen_server是监督树的一部分，则不需要实现停止函数，它的监督树会管理它的生命周期。

当gen_server由监督树管理并且要在终止之前做一些处理工作，那么gen_server必须设为系统进程，它在终止时才会调用terminate()函数。

```erlang
init([]) ->
    process_flag(trap_exit, true),
    ...
    {ok, State}
```





如果是独立的gen_server，则需要停止功能。

```erlang
export([stop/0]).

stop() ->
    gen_server:call(mod, stop).

handle_call(stop, State) ->
    {stop, normal, stopped, State}.

terminate(_Reason, _State) -> ok
```

我们向gen_server发出stop请求要求停止服务器，处理stop请求的回调函数返回一个元素 {stop, normal, stopped, State}，stopped会返回给stop()接口，normal表示它是一个正常的终止，会作为第一个参数传递给terminate()接口。

在处理完stop请求后，terminate()接口会被调用。terminate是init的逆操作，在最近进行必要的清理。

注：无论是服务器崩溃并生成{'Exit', reason}消息，还是主要发起stop请求，最后都是调用terminate接口。











问题：测试terminate()在监督树和独立模式下的运行情况。

我在测试过程中，独立的gen_server停止后会调用terminate()，并没有设置成系统进程。

问题2：cast调用与主动向gen_server发送消息的行为是否都是异步的？





通过exit/2去关闭gen_server进程时，normal错误无效，无法关闭。其他错误会关闭。并且通过exit/2关闭gen_server进程，不会执行terminate，除非gen_server是系统进程。











#### 代码更新

```erlang
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```





example：gen_server模板

```erlang
-module(my_bank).

-behavior(gen_server).
-export([start/0, stop/0, new_account/1]).

%% gen_server回调函数
-export([init/1, handle_call/3, handle_cast/2,
		 handle_info/2, terminate/2, code_change/3]).
-define(SERVER, bank).

start() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:call(?SERVER, stop).


%% 客户端接口
new_account(Who) ->
	gen_server:call(?SERVER, {new, Who}).


%% 行为接口
init([]) ->
	{ok, []}.

handle_call({new, Who}, _From, State) ->
	State1 = [Who| State],
	{reply, State1, State1};
handle_call(stop, _From, State) ->
	{stop, normal, stopped, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) -> 
	{noreply, State}.

terminate(Reason, _State) ->
	io:format("terminate reason: ~p~n", [Reason]),
	ok.
code_change(_OlsVsn, State, _Extra) ->
	{ok, State}.
```









### 事件处理框架

事件是指已经发生的事情。当某个事件发生时，就会发送一个evetn消息给某个注册进程（事件处理器），由它去进行处理。

事件处理框架的通用接口有：

- make(Name)

  创建一个Name的事件处理器

- evetn(Name, X)

  发送消息X到Name的事件处理器

- add_handler(Name, Fun)

  给Name的事件处理器添加一个处理函数Fun。

```erlang
-module(event_handler).
-export([make/1, event/2, add_handler/2]).

%% 创建的事件处理器，在执行时如何随时替换掉执行函数。

make(Name) ->
    register(Name, spawn(fun() -> handle(fun no_op/1) end)).

event(Name, X) ->
    Name ! {event, X}.

add_handler(Name, Fun) ->
    Name ! {add, Fun}.

handle(F) ->
    receive
        {event, E} ->
            F(),
            handle(F);
        {add, NewFun} ->
            handle(NewFun)
	end.

no_op() ->
    void.
```









































### 错误记录器（error report）

OTP自带一个可定制的错误记录器。简单的理解它就是用于一个记录错误日志，配置如何记录错误日志的东西。



> erl -boot start_clean

标准错误记录器，这是创建一个开发环境系统，只提供一种简单的错误记录形式。



> erl -boot start_sasl

sasl是启动一个运行生产系统的环境。

系统架构支持库（System Architecture Support Libraries, SASL）将负责错误记录和过载保护等工作。

监督树似乎只能运行在这种模式下?





#### 记录错误

错误记录器会自动生成多种报告类型。

- 监控器报告

  这些报告会在OTP监控器启动或停止被监控进程生成。

- 进度报告

  同上，也是在OTP监控器启动或停止时生成。

- 崩溃报告

  如果某个被OTP行为启动的进程因为normal或shutdown以外的原因终止，这些报告就会生成。

以上类型的错误报告会自动生成，可以通过配置错误记录器使用日志文件来存储这些报告信息。





除了自动生成的报告，还可以使用error_logger模块手动生成日志报告。

可以生成错误、警告和信息消息三种类型的日志报告。这三种类型只是用于区分错误日志的严重级别，由程序员自己决定。





#### 配置错误记录器

我们可以针对输出目标和输出内容进行配置。

日志的输出目标可以是shell或文件，也可以配置要输出哪种类型的日志，比如错误日志、信息日志、进度日志等。

```erlang
%% 无tty
[{sasl, [
	{sasl_error_logger, false},
	{errlog_type, error},
	{error_logger_mf_dir, "d:/server"},
	{error_logger_mf_maxbytes, 10285760},
	{error_logger_mf_maxfiles, 10}
]}].
```

sasl_error_logger应该是进度日志，false表示输出到shell中。

error相关项用于配置错误日志，error_logger_mf_dir配置日志的输出目标，error_logger_mf_maxbytes配置日志的最大存储量，error_logger_mf_maxfiles配置日志文件书。

当错误日志超过指定的大小时，会删除老的日志会新日志腾出空间。





#### 分析日志 

rb模块用于分析日志。 







if the gen_server process is part of the a supervisor tree and is ordered by its supervisor to terminate, this function 