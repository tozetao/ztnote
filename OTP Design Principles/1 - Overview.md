## Overview

OTP设计原则定义了如何以进程、模块和目录的方式来构建erlang代码。



### 监督树

Erlang/OTP的一个基本概念是监督树，这是一个基于supervisors和workers设想的进程结构模型。

- workers是执行计算的进程，也就是说它们执行具体的工作。

- supervisors是监视workers行为的进程，如果一个worker出错，那么对应的supervisor能够重启它。
- 监督树是一颗将代码按层次组织成supervisors和workers的树，这让设计和编写容错软件成为一种可能。



### Behaviours

在监督树中，很多进程拥有相似的结构，它们遵循相同的模式。例如监督者（supervisors）的结构是相似的。它们之间唯一的区别是监督哪些子进程（workers）。也有很多进程（workers）是服务端/客户端关系模型的服务器，是有限状态机或者是事件处理其（Event Handler）。

行为模块是Erlang/OTP的一部分。比如要实现一个supervisor进程，用户只需实现回调模块，回调模块就是一组导出的预定义的回调函数。

下面的示例充分说明了代码如何分为通用部分和特定部分。考虑下面一个简单服务器的代码，它跟踪一些"channels"，其他进程可以分别调用函数alloc/0和free/1来分配和释放"channels"。

```erlang
-module(ch1).
-export([start/0, init/0]).
-export([alloc/0, free/1]).

start() ->
    spawn(ch1, init, []).

init() ->
    register(ch1, self()),
    Chs = channels()

loop(Chs) ->
    receive
        {From, alloc} ->
            {Ch, Chs2} = allow(Chs),
            From ! {ch1, Ch},
            loop(Chs2);
        {From, free, Ch} ->
            Chs2 = free(Ch, Chs),
            loop(Chs2);
	end.
```

服务器的代码可以写成一个通用的部件，server.erl：

```erlang
-module(server).
-export([start/1]).
-export([call/2, cast/2]).
-export([init/1]).

start(Mod) ->
    spawn(server, init, [Mod]).

call(Name, Req) ->
    Name ! {call, self(), Req},
    receive
        {Name, Res} ->
            Res
    end.

cast(Name, Req) ->
    Name ! {cast, Req},
    ok.

init(Mod) ->
    register(Mod, self()),
    State = Mod:init(),
    loop(Mod, State).

loop(Mod, State) ->
    receive
        {call, From, Req} ->
            {Res, State2} = Mod:handle_call(Req, State),
            From ! {Mod, Res},
            loop(Mod, State2);
        {cast, Req} ->
            State2 = Mod:handle_cast(Req, State),
            loop(Mod, State2)
    end.
```

还有一个回调模块ch2.erl：

```erlang
-module(ch2).
-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0, handle_call/2, handle_cast/2]).

start() ->
    server:start(ch2).

alloc() ->
    server:call(ch2, alloc).

free(Ch) ->
    server:cast(ch2, {free, Ch}).

init() ->
    channels().

handle_call(alloc, Chs) ->
    alloc(Chs). % => {Ch,Chs2}

handle_cast({free, Ch}, Chs) ->
    free(Ch, Chs). % => Chs2
```

请注意几点：

- server中的代码可以被重用于建立许多不同的服务器。在本例中服务器名称是ch2，服务器名字对调用客户端函数的用户是隐藏的。这意味着可以在不影响他们的情况下修改服务器名字。
- 服务器的协议（向服务器发送和接收消息）也是隐藏的，这是很好的编程实践，允许人们去改变协议而不改变使用接口函数的代码。
- 服务器的功能可以在不改变ch2或任何其他回调模块的情况下进行扩展。

在上面的ch1.erl和ch2.erl中，有意省略了channel/0、alloc/1和free/2的实现，因为它与本例无关。为了完整起见，下面给出了这些函数的一种写法。这只是一个例子，现实中的实现必须能够处理诸如用完通道分配等情况。

```erlang
channels() ->
   {_Allocated = [], _Free = lists:seq(1,100)}.

alloc({Allocated, [H|T] = _Free}) ->
   {H, {[H|Allocated], T}}.

free(Ch, {Alloc, Free} = Channels) ->
   case lists:member(Ch, Alloc) of
      true ->
         {lists:delete(Ch, Alloc), [Ch|Free]};
      false ->
         Channels
   end.       
```



在不使用行为的情况下编写代码可以提高效率，但效率的提高是以牺牲通用性为代价的。以一致的方式管理系统中应用程序的能力是很重要的。

使用行为编写的代码有助于其他人理解，即兴的编程结构虽然更有效率，但总是难以理解。

上面的server模块（server.erl）相当于Erlang/OTP中大大简化的gen_server模块。



标准的OTP行为有：

- gen_server

  用于实现客户-服务器关系的服务器。

- gen_statem

  用于实现状态机

- gen_event

  用于实现事件处理功能

- supervisor

  用于在监督树中实现督程（supervisor）

编译器能够理解 -behaviour(Behaviour) 模块属性，并会对缺失的回调函数发出警告。例如：

```erlang
-module(chs3).
-behaviour(gen_server).
...

3> c(chs3).
./chs3.erl:10: Warning: undefined call-back function handle_call/3
{ok,chs3}
```



### Applications

Erlang/OTP带有许多组件，每个组件都实现了一些特定的功能。组件用Erlang/OTP的术语叫做应用程序。

Erlang/OTP应用程序的例子有Mnesia，它拥有数据库服务编程所需的一切，还有Debugger，它是用来调试Erlang程序的。

基于Erlang/OTP的最小系统由以下两个应用程序组成。

- Kernel

  运行Erlang所需的功能。

- STDLIB

  Erlang标准库

Application概念也适用于进程和模块。

最简单的Application没有任何进程，而是由一系列的模块组成。这样的Application称为库应用程序。STDLIB就是一个库应用程序。

而带有进程的应用程序最简单的实现方式是使用标准行为实现为监督树。

在Application章节中描述了如何编写应用程序。



### Releases

一个发布版本是由Erlang/OTP应用程序的子集和用户指定的应用程序子集组成的。

在Releases章节描述了如何编写发行。在第2节系统原理（System Principles）中，关于目标系统的部分有描述如何在目标环境中安装一个发布版。



### Release Handling

发布处理是指在（可能的）正在运行的系统中，在一个发布版本的不同版本之间进行升级和降级。如何做到这一点，在 Release Handling 中有介绍。