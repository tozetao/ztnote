在监督树模型中，怎么启动一个子进程的同时并传递参数。







## Supervisor Behaviour

本节应与STDLIB中的supervisor(3)手册页一起阅读，其中描述了督程行为的细节。

在这里将Supervisor翻译为督程。



### Supervision Principles

译为监督原则。

supervisor负责启动、停止和监控它的子进程。supervisor的基本思想是在必要的时候通过重启它们来保持子进程的存活。

要启动和监控的子进程由子进程规范列表指定，子进程按照这个列表指定的顺序启动，并按照相反的顺序停止。



example：

supervisor启动服务的回调模块来自于gen_server Behaviour，看起来如下所示：

```erlang
-module(ch_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

    ChildSpecs = [#{id => ch3,
                   start => {ch3, start_link, []}
                   restart => permanent,
                   shutdown => brutal_kill,
                   type => worker,
                   modules => [cg3]}],

    {ok, {SupFlags, ChildSpecs}}.
```

init/1返回值中的SupFlags变量表示supervisor flags，ChildSpecs变量是子进程规范列表。





### Supervisor Flags

Supervisor Flags翻译为督程标志，定义了督程自身的一些属性。

这是督程标志的类型定义：

```erlang
sup_flags = #{
	strategy => strategy(),
	intensity => intensity(),
	period => period()
}.
```

- strategy指定重启策略

- idtensity和period指定最大重启强度。




#### Restart Strategy

Restart Strategy译为重启策略。

重启策略是由init回调函数返回的督程标志中的strategy key指定的：

```
SupFlags = #{strategy => Strategy, ...}
```

SupFlags是一个Map，在Map中strategy key是可选的。如果没有提供，默认策略是one_for_one。



one_for_one

如果一个子进程终止，只有该子进程会被重启。

注：one_for_one监督模式中，任意一个子进程死亡都会被重启。



one_for_all

如果一个子进程终止，其他的子进程都会被终止。接着所有的子进程，包含刚才终止的子进程都会被重启



rest_for_one

如果一个子进程终止，其余的子进程（指的是按照启动顺序排在终止进程之后的子进程）也被终止。然后终止的子进程和其余的子进程都会被重启。



simple_one_for_one

参数simple_one_for_one章节。



#### Maximum Restart Intensity

译为最大重启强度。

Supervisors（督程）有一个内置机制用于限制在给定的时间间隔内可以重启的次数。这是通过init回调函数所返回的Supervisor Flags（督程标志）中的intensity、period俩个key指定的。

```erlang
SupFlags = #{intensity => MaxR, period => MaxT}.
```

在最近的MaxT秒内发生的重启次数超过MaxR，则督程会终止所有子进程，接着终止它自己。督程的终止原因将是shutdown。

当督程终止时，那么紧挨着的更高级别的督程会采取一些行动，它要么重启刚才终止的督程，要么终止它自己。

重启机制的用意在于避免当一个进程因为同样的原因反复死亡却又被重启的情况。

在supervisor flags（督程标志）中intensity和period是可选的，如果没有设置它们的默认值分别时1和5.



### 子进程规范

子进程规范列表定义如下：

```erlang
child_spce() = #{id => child_id(),
                start => {module, fun, args},
                restart => permanent | transient | temporary,
                shutdown => brutal_kill | timeout(),
                type => worker | supervisor,
                modules => [module()] | dynamic}
```

- id

  督程用于区分子进程的标识。

- start

  子进程的启动函数，它是一个用作apply(M, F, A)的模块函数参数元组。

- restart

  定义已终止的进程如何重新启动。

  permanent：表示子进程会永远重启。

  temporary：临时子进程永远不会重新启动。

  transient：只有当子进程在异常终止时才重新启动，即退出原因不是normal、shutdown。

- shutdown

  定义如何终止子进程。

  brutal_kill意味着会使用exit(child, kill)无条件终止子进程。

  整数值表示督程通过调用exit(child, shutdown)通知子进程终止，然后等待返回的退出信号。如果在指定的时间内没有收到退出信号，则调用exit(child, kill)无条件终止子进程。

  如果子进程是另外一个督程，则必须设置为无穷大，以便使子树有足够的时间关闭。如果子进程是一个worker，也可以设置为无穷大（这里有风险）。
  
  shutdown猜测应该是与supervisor:terminate_child()函数有关，因为该选项是督程主动终止子进程。

- type

  定义子进程的类型

  supervisor：子进程是一个督程。

  worker：子进程是一个工作进程。

- modules

  modules是一个包含一个元素[Module]的列表，如果子进程是supervisor、gen_server、gen_statem，那么Module是回调模块的名称。如果子进程是gen_event，Module是动态的。





### 启动Supervisor

```erlang
start_link() ->
    supervisor:start_link(ch_sup, []).
```

以上是ch_sup.erl文件的代码，在上面的示例中，会通过调用ch_sup:start_link()来启动一个督程。



第一个参数ch_sup是回调模块的名称，也是init函数所在的模块。

第二个参数[]，是一个传递给init回调函数的任意数据项。

supervisor:start/2并没有注册督程的名字，如果要对其进程注册需要使用supervisor:start/3函数。



当supervisor:start_link/2函数执行时，会启动一个督程并执行回调模块的init函数。

调用start_link()的进程会与督程链接在一起，如果调用进程终止了，督程也会被终止，这会导致所有子进程也被终止。

同时督程的启动是同步的，在子进程没有全部初始化完毕之前是不会返回的。





函数说明：

```erlang

```











### 添加子进程

除了静态监督树之外，还可以使用以下命令将动态子进程添加到现有监督树中：

```erlang
supervisor:start_child(Sup, ChildSpec)
```

Sup是督程的pid或名称，ChildSpec是子进程规范。

使用start_child/2添加的子进程与其他子进程的行为相同，但有一个重要的例外：如果一个督程死亡并重新创建，那么所有动态添加到督程的子进程会消失。





### 停止子进程







### Simplified one_for_one Supervisor

具有simple_one_for_one重启策略的督程是one_for_one督程的简化版，其中所有的子进程都是督程动态添加的实例对象。



下面是一个简单的示例：

```erlang
-module(simple_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(simple_sup, []).

init(_Args) ->
    SupFlags = #{strategy => simple_one_for_one,
                intensity => 0,
                period => 1},
    
    ChildSpecs = [#{id => call,
                   start => {call, start_link, []},
                   shutdown => brutal_kill}],
    
    {ok, {SupFlags, ChildSpecs}}.
```

在启动时，督程不会启动任何子进程。相反所有的子进程都是通过调用以下命令动态添加的：

```erlang
supervisor:start_child(Sup, List)
```

Sup参数可以是督程的pid或名称，List是一个任意项的列表，它会被添加到子进程规范指定的参数列表中。

如果启动函数参数指定为{M, F, A}，那么会通过调用apply(M, F, A ++ List)来启动，简单的说List参数会被附加到回调模块的参数中，由回调模块处理。



比如给上面的simple_sup添加一个子进程：

```erlang
supervisor:start_child(Pid, [id1])
```

其实是通过apply(call, start_link, []++[id1])来启动子进程。或者实际上：

```erlang
call:start_link(id1).
```



simple_one_for_one督程下的子进程可以通过以下方式来终止：

```erlang
supervisor:terminate_child(Sup, Pid).
```

Sup是督程的id或名字，Pid是子进程的pid。


因为一个simple_one_for_one督程可以有很多的子进程，所以它是异步的关闭所有子进程。这意味着这些子进程将并行地进行清理工作，因此没有定义它们终止地顺序。





### 停止Supervisor

由于督程是监督树的一部分，所以它会被其上级自动终止。当被要求关闭时，它根据各自的规范，以启动顺序的反向顺序终止所有子进程，然后自己终止。









遗留问题：

为什么在erlang shell中直接启动督程，明明子进程重启次数没有超过最大重启强度却会无法重启子进程呢?

问题1：最大重启强度是针对单个子进程还是针对所有子进程？

问题3：gen_event的遗留问题，触发一个事件，是所有已经注册的时间处理器一起处理这个事件，还是指定某个事件处理器来进行处理。



目前我只测试了某个进程崩溃的次数超过最大重启强度，督程也会结束。对于多个子进程的重启次数还未测试。

