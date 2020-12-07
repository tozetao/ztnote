## Applications

本节应与Kernel中的app(4)和app(3)手册页一起阅读。

注：在这里我将Application翻译为应用。





### 概念

当你编写了实现一些特定功能的代码后，你可能想把这些代码做成一个应用，也就是一个可以作为一个单元来启动和停止的组件，并且它也可以在其他系统中重用。

要做到这一点，请创建一个应用回调模块，并描述如何启动和停止应用。

然后，需要在应用资源文件中编写应用规范。这个文件规定了应用由哪些模块组成，以及回调模块的名称。

如果你使用systools，即Erlang/OTP工具来打包代码（见发布章节），每个应用的代码都要按照预先定义的目录结构放在一个单独的目录中。







### 应用回调模块

如何启动和停止应用的代码，也就是监督树，由两个回调函数描述。

```erlang
start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State}
stop(State)
```

- 当要要通过顶级督程创建一颗监督树来启动应用时，start函数会被调用。预期返回顶级督程的pid和一个可选的State term，默认值是[]。State会作为参数传递给stop/1函数。
- StartType通常是原子normal，只有在takeover或failover时才有其他值，参见Distributed Applications。

- StartArgs是由应用资源文件中的mod字段定义的。
- stop/1是在应用停止时调用，用于进行必要的清理工作。应用的停止，实际上就是监督树的关闭。

在Starting and Stopping Applications小结描述了如何启动和停止应用。



一个封装监督树监督行为的application回调模块示例：

```erlang
-module(ch_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    ch_sup:start_link().

stop(_State) ->
    ok.
```



对于一个类库应用，它不能被启动和停止，因为它是不需要任何应用程序回调模块的。











### 应用资源文件

要定义一个应用，就要定义应用规范，这些规范放在应用资源文件中，简称为.app文件。

```erlang
{
	application,
	Application,
	[Opt1, ..., OptN]
}
```

- Application是一个原子，它是应用的名称，应用资源文件必须命名为Application.app

- 每个Opt都是一个{Key, VAlue}的元组，它定义了应用的某个属性。所有的Key都是可选的，任何省略的Key都使用默认值。



一个最小的库应用libapp.app文件内容如下：

```erlang
{
	application,
	libapp,
	[]
}
```

像ch_app.app这样的监督树应用，它的最小资源文件如下：

```erlang
{
	application,
	ch_app,
	[
		{mod, {ch_app, []}}
    ]
}
```

键mod定义了应用的回调模块和启动参数，在这里分别是ch_app和[]。这意味着当应用启动时，会调用以下内容：

```
ch_app:start(normal, []).
```

而当应用停止时会调用：

```
ch_app:stop([]).
```



当使用systools时，还需要指定用于打包代码的Erlang/OTP工具（详见Releases章节）以及description，vsn，modules，registered、applications等键。

```erlang
{
	application,
	ch_app,
	[
    	{description, "channel allocator."},
     	{vsn, "1"},
    	{moduels, [ch_app, ch_sup, ch3]},
     	{registered, [ch3]},
     	{applications, [kernel, stdlib, sasl]},
     	{mod, {ch_app, []}}
    ]
}.
```

- description

  一个简短的描述，字符串，默认值""

- vsn

  版本号，字符串，默认""

- modules

  所有模块都会被应用引用，systools在生成启动脚本和tar文件时会使用该列表。一个应用中只能定义一个模块，默认[]。

- registered：

  应用中所有已注册进程的名称。

  systools使用该列表来检测应用之间的名称冲突，默认值[]

- applications

  在应用启动之前列表中所有的应用都要被启动。

  systools使用这个列表来生成正确的启动脚本。默认值为[]。注意所有的应用至少都有对Kernel和STDLIB的依赖性。

说明：关于应用资源文件的语法和详细描述，详见：https://erlang.org/doc/man/app.html





### 目录结构

当使用systools打包代码时，每个应用的代码都会放在一个单独的目录中，lib/Application-Vsn，其中Vsn是版本号。

即使不使用systools这一点也是很有用的，因为Erlang/OTP打包代码是遵循OTP原则的，从而导致特定的目录结构。如果一个应用有多个版本，代码服务器将会使用版本号中最高的目录中的代码。详见：https://erlang.org/doc/man/code.html



开发环境目录结构指南：

只要发布的目录结构符合下面的描述，任何用于开发的目录结构都满足了，但是鼓励在开发环境中使用相同的目录结构。应用目录名应该省略版本号，因为这是发布步骤的产物。

有些子目录是必须的。有些子目录是可选的，这意味着只有在应用本身需要时才应该使用它。最后，有些子目录是推荐的，意味着鼓励使用它，并按照这里的描述使用。例如，鼓励在应用中同时存在文档和测试，以使其被认为是一个适当的OTP应用程序。

```
─ ${application}
      ├── doc
      │   ├── internal
      │   ├── examples
      │   └── src
      ├── include
      ├── priv
      ├── src
      │   └── ${application}.app.src
      └── test
```

- src

  必须的，包含erlang源码，.app源文件和应用自身内部使用的include文件。

  src中的子目录可以作为命名空间来组织源文件，这些目录的深度不应该超过一级。

- priv

  可选，应用使用的特定文件。

- include

  可选，公共的include文件，也能够被其他应用使用。

- doc

- doc/internal

- doc/example

- doc/src

- test









### application controller

应用程序控制器进程，它是作为Kernel应用程序的一部分被启动，注册为application_controller。

对应用程序的所有操作都由应用程序控制器协调，它通过application模块中的函数进行交互。



load和unload

在应用程序启动前必须加载它，应用程序会从.app文件中读取并存储信息。

```erlang
application:load(ch_app).
```

也可以卸载停止或从未启动的应用，应用控制器会从内存数据库删除应用的的相关信息。

```erlang
application:unload(ch_app).
```





### 启动和停止应用

通过调用以下命令启动应用：

```erlang
application:start(ch_app).
```

如果应用程序未加载，需要调用application:load/1加载应用。它会检查applications键的值，以确保在该应用运行之前所有应用程序已启动。

启动时应用控制器会为应用创建一个应用主进程，应用主进程是应用程序中所有进程的组长。应用主进程通过调用应用模块的回调函数start/2，以及由.app文件中的mod键定义的start参数来启动程序。

通过调用以下命令停止应用，但不卸载：

```erlang
application:stop(ch_app).
```

应用主进程通过通知顶级督程来关闭应用，督程会通知所有子进程关闭，整个树以相反的启动顺序终止。然后应用主进程调用mod键定义的应用模块的stop/1回调函数，最终整个应用终止。





### 配置应用

.app文件中的env键可以配置应用的配置参数，env键的值是一个{Par, Val}的元组列表。

```erlang
{application, ch_app,
 [{description, "Channel allocator"},
  {vsn, "1"},
  {modules, [ch_app, ch_sup, ch3]},
  {registered, [ch3]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {ch_app,[]}},
  {env, [{file, "/usr/local/log"}]}
 ]}.
```

Par是一个原子，Val是任意项。配置项的值可以通过调用application:get_env(App, Par)来获取。



注：.app文件中的值和系统配置文件中的值可以直接从命令行覆盖。





启动类型







问题：

启动和停止应用。

配置文件怎么编写。

其他章节说了什么？