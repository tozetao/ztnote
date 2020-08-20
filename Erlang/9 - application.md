### 概念

Application是指将代码作成一个应用程序，作为一个单元启动和停止的组件来使用。

为此需要创建应用程序回调模块，描述如何启动和停止应用。同时还需要一个应用程序资源文件，用于定义应用程序规范，指定应用程序由哪些模块组成以及回调模块的名称。

如果使用systools、erlang/otp工具来打包代码，那么应用程序代码必须遵循一定的目录结构。



### 应用程序回调模块

```erlang
-spec start(Type, Args) -> {ok, Pid} | {ok, Pid, State}.
```

该函数描述如何启动监督树。

start在启动应用时被调用，并通过启动顶级督程来创建监督树。它会返回顶级督程的pid和一个可选的State数据项，State默认值是[]。State会作为参数传递给stop函数。

- Type

  一般是normal原子，它只有在接管和故障转移情况下才有其他值。参见分布式应用。

- Args

  该参数由应用资源文件中的mod键定义。

```erlang
-spec stop(State)
```

该函数描述如何停止监督树。

在应用停止时调用，应用的停止就是监督树的关闭，它会按照启动和停止应用程序中的描述自动处理。







### 应用程序资源文件

要定义应用需要创建一个规范文件，这些规范存储在后缀名为.app的文件中。

```erlang
{
	application, Application, [Opt1, ..., OptN]
}
```

- Application

  一个原子，它是应用规范文件的名称，文件名必须是Application.app。

- 每个Opt都是一个键值对的元组，它定义了应用程序的某个属性。

库应用程序的最小.app文件如下：

一个监督树最小应用程序如下：

```erlang
{application, ch_app,
	[mod, {ch_app, []}]
}
```

key mod定义了应用程序的回调模块和start参数，ch_app是回调模块，[]是start参数。这意味着在启动本应用程序时会执行：

```erlang
ch_app:start(normal, []).
```

当停止应用时调用：

```erlang
ch_app:stop([]).
```

当使用systools时，还需要指定用于打包代码的otp工具，以及其他一些键的配置。

```erlang
{application, ch_app, 
 [{description, "Channel allocator"},
  {vsn, "1"},
  {modules, [ch_app, ch_sup, ch3]},
  {registered, [ch3]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {ch_app, []}}
 ],
}
```

- vsn

  版本号

- modules

  这个程序引入的所有模块，systools在生成引导脚本和tar文件时使用这个列表。默认值为[]。

  一般包含application回调模块，监督树模块，server模块。

- registered

  应用程序所有注册进程的名称，systools会使用这个列表来检测应用程序之间的名称冲突。

  默认值[]。

- applications

  所有在应用程序启动前必须启动的应用程序，systools使用这个列表来生成正确的启动脚本。默认值为[]。注意所有的应用程序至少都有对Kernel和STDLIB的依赖性。



### 目录结构

使用systools打包时，要遵循一定的目录结构。



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