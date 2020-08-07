Supervisor Behaviour

督程行为。

Supervisor是监督者的意思，在erlang代表监督进程，简称督程。主要用于管理子进程的。



### 监督原则

监督进程负责启动、停止和监视子进程。监督进程的基本思想是在必要的时候通过重启它们来保持子进程的活动。

要启动的子进程由子进程规范列表指定。子进程会按照列表的顺序进行初始化，并以相反的顺序终止。



example：

```erlang
-module(ch_sup).
-export([start_link/0]).

-behaviour(supervisor).
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





### 督程标志（supervisor flags）

```erlang
sup_flags = #{strategy => strategy(),
             intensity => intensity(),
             period => period()}.
```

- strategy

  配置督程的重启策略

- idtensity、period

  这俩个参数指定的重启频率，如果一个子进程在period时间内执行了超过idtensity次重启，就会终止所有子进程然后退出。



### 重启策略

重启策略是由init回调函数返回的督程标志Map中的strategy键指定的。重启策略有以下选项：

- one_for_one

  如果子进程终止，则只有该进程可以重新启动。

- one_for_all

  如果子进程终止，则所有其他子进程都会终止，然后重新启动所有子进程（包括已终止的子进程）。

- rest_for_one

  如果一个子进程终止，其余的子进程（指以开始顺序结束的进程之后的子进程）也将终止。然后重新启动终止的子进程和其余的子进程。





### 最大重启强度

督程有一个内置机制来限制在给定时间间隔内可以重启的次数。由init回调函数返回的督程标志的intensity、period键指定。

```erlang
SupFlags = #{intensity => MaxR, period => MaxT}.
```

如果在过去MaxT秒时间内重启的次数大于MaxR，则督程将终止所有子进程，然后终止自身。这种情况下自身以shutdown原因终止的。

重启机制的目的在于防止一个进程因为相同的原因反复死亡，但又重新启动的情况。

idtensity和period的默认值分别是1和5，即在过期5秒内重启的次数大于1，





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
start_link(Module, Args) -> startlink_ret();
start_link(SupName, Module, Args) -> startlink_ret();    
```

通过调用Supervisor模块这俩个函数来实现启动督程。

supervisor:start_link()是同步的，在子进程没有全部初始化完毕之前是不会返回的。

调用start_link()时，当前调用进程会与督程链接在一起，如果调用进程终止了，督程也会被终止，这会导致所有子进程也被终止。



### 添加子进程

除了静态监督树之外，还可以使用以下命令将动态子进程添加到现有监督树中：

```erlang
supervisor:start_child(Sup, ChildSpec)
```

Sup是督程的pid或名称，ChildSpec是子进程规范。

使用start_child/2添加的子进程与其他子进程的行为相同，但有一个重要的例外：如果一个督程死亡并重新创建，那么所有动态添加到督程的子进程会消失。





### 停止子进程







### Simplified one_for_one Supervisor

使用simple_one_for_one的督程是简化版的one_for_one督程，其中所有的子进程都是督程动态添加的实例对象。

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

Sup参数可以是督程的pid或名称，List是一个任意项的列表，它会被添加到子进程规范中指定的参数列表中。如果启动函数参数指定为{M, F, A}，那么会通过调用apply(M, F, A ++ List)来启动。



例如给上面的simple_sup添加一个子进程：

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

因此一个simple_one_for_one督程可以有很多的子进程，所以它是异步的关闭所有子进程。这意味着这些子进程将并行地进行清理工作，因此没有定义它们终止地顺序。



### 停止Supervisor



遗留问题：

为什么在erlang shell中直接启动督程，明明子进程重启次数没有超过最大重启强度却会无法重启子进程呢?

问题1：最大重启强度是针对单个子进程还是针对所有子进程？

问题3：gen_event的遗留问题，触发一个事件，是所有已经注册的时间处理器一起处理这个事件，还是指定某个事件处理器来进行处理。



目前我只测试了某个进程崩溃的次数超过最大重启强度，督程也会结束。对于多个子进程的重启次数还未测试。

