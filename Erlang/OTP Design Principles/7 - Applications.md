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

在开发环境中可能需要其他目录。如果除了erlang以外的其他语言的代码，例如NIF的C代码，这些代码应该放在单独的目录中。

按照惯例，建议在这些目录前加上语言名称，例如c_src代表C，java_src代表Java，go_src代表Go。

后缀为_src的目录表示它是应用程序和编译步骤的一部分。最终构建的artifacts 应该以priv/lib或priv/bin目录为目标。

priv目录存放着应用在运行中需要的资源，可执行文件应该放在priv/bin中，动态链接库应该放在priv/lib中，其他资源文件可以自由的放在priv目录中，但是建议它们以结构化的方式来存放。

来自其他语言的生成Erlang代码的源文件，如ASN.1或Mibs，应该放在顶层目录或src的目录中，名称与源语言相同，例如asn1和mibs。

构建的artifacts应该放在各自的语言目录下比如src代表Erlang代码，java_src代表Java代码。

在开发环境中，用于发布的.app文件可能位于ebin目录中，但我们鼓励将其作为构建步骤的产物。按照惯例，会使用.app.src文件，它位于src目录下。这个文件与.app文件几乎相同，但某些字段可能会在构建步骤中被替换，例如应用程序版本。

目录名不应该用大写。

鼓励省略空目录。



已发布系统的目录结构：

一个已发布的应用必须遵循一定的结构：

```
    ─ ${application}-${version}
      ├── bin
      ├── doc
      │   ├── html
      │   ├── man[1-9]
      │   ├── pdf
      │   ├── internal
      │   └── examples
      ├── ebin
      │   └── ${application}.app
      ├── include
      ├── priv
      │   ├── lib
      │   └── bin
      └── src
```

- src

  可选的。

  包含erlang源代码和应用自身内部使用的include文件。在已发布的应用程序中不需要该目录。

- ebin

  必须的。包含erlang对象代码，beam文件，.app文件必须放在这里。

- priv

  可选的。应用使用的特定文件。code:priv_dir/1用于访问该目录。

- priv/lib

  推荐的。任何被应用使用的共享对象（shared-object）文件。例如NIFs或linked-in-drivers都应该放在这里。

- priv/bin

  推荐的。任何被应用程序使用的可执行文件，比如port-programs都应该放在这里。

- include

  可选的。公共的include文件，这些文件要能够被其他应用访问。

- bin

  可选的。Any executable that is a product of the application, such as escripts or shell-scripts, should be placed here.

src目录对发布调式有用，但不是必须的。对于include目录来说，只有当应用有公开的include文件时，才应该发布include目录。

以man pages这种方式发布的文档是被推荐的，HTML和PDF通常会以其他方式发布。

最后我们鼓励省略空目录。





### 应用控制器

Application Controller。

当一个erlang运行时系统启动时，一些进程会作为Kernel应用的一部分被启动。其中一个进程是应用控制器进程，注册为application_controller。

对应用的所有操作都是由应用控制器协调的，我们是通过application模块的函数与其交互的，具体参见Kernel中的application(3)手册页。

特别是应用可以被加载、卸载，启动和停止。





### 加载和卸载应用

在启动应用之前必须加载它。应用控制器从.app文件中读取并存储信息。

```
1> application:load(ch_app).
ok
2> application:loaded_applications().
[{kernel,"ERTS  CXC 138 10","2.8.1.3"},
 {stdlib,"ERTS  CXC 138 10","1.11.4.3"},
 {ch_app,"Channel allocator","1"}]
```

可以卸载已停止或从未启动的应用，关于该应用的信息会从应用控制器的内部数据库删除。

```
3> application:unload(ch_app).
ok
4> application:loaded_applications().
[{kernel,"ERTS  CXC 138 10","2.8.1.3"},
 {stdlib,"ERTS  CXC 138 10","1.11.4.3"}]
```

注意：

Loading/unloading一个应用不会去load/unload应用使用的代码。代码的加载是以一般的方式进行的。





### 启动和停止应用

通过调用以下命令启动应用：

```erlang
application:start(ch_app).
```

如果应用程序未加载，应用控制器会调用application:load/1第一时间加载。它会检查applications键的值，以确保在该应用运行之前所以来的所有应用已启动。

在之后应用控制器会为该应用创建一个application master，application master会成为在该应用中所有进程的组长。

I/O转发到上一个组长，不过，这只是一种识别属于应用程序的进程的方式。比如用来从任何进程中找到自己，或者，对等地，当它终止时，将它们全部杀死。

application master会调用应用模块实现的回调函数（start/2）来启动应用，start函数的参数是定义在.app文件中的mod键中。



通过调用以下命令停止应用，但不卸载：

```erlang
application:stop(ch_app).
```

application master会告诉顶级督程去shutdown来关闭应用，接着顶级督程会通知所有子进程去shutdown，以此类推。

整个树以相反的启动顺序终止。然后应用主进程调用mod键定义的应用模块的stop/1回调函数，最终整个应用终止。





### 配置应用

应用程序可以使用配置参数进行配置，这些参数是由.app文件中的env键指定的{Par, Val}元组列表。

```erlang
{
	application, ch_app,
	[
     	{description, "Channel allocator"},
  		{vsn, "1"},
  		{modules, [ch_app, ch_sup, ch3]},
  		{registered, [ch3]},
  		{applications, [kernel, stdlib, sasl]},
  		{mod, {ch_app,[]}},
  		{env, [
			{file, "/usr/local/log"}
        ]}
	]
}.
```

Par是一个原子，Val是任意项。配置项的值可以通过调用application:get_env(App, Par)来获取。

在应用中可以通过调用application/get_env(App, Par)或一些类似的函数来检索配置参数的值，参见Kernel中的application(3)手册页面。

example：

```erlang
application:start(ch_app).
application:get_env(ch_app, file).
```

.app文件中的值可以被系统文件中的值覆盖，这个一个包含了相关应用配置参数的文件：

```erlang
[{Application1, [{Par11,Val11},...]},
 ...,
 {ApplicationN, [{ParN1,ValN1},...]}].
```

系统配置命名为Name.config，Erlang要使用命令行参数-config Name来启动。更详细的信息参数Kernel中的config(4)手册页。

example：

使用以下内容创建一个test.config文件：

```erlang
[{ch_app, [{file, "testlog"}]}].
```

文件中file的值会覆盖.app文件中file字段的值。

```erlang
% erl -config test
Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]

Eshell V5.2.3.6  (abort with ^G)
1> application:start(ch_app).
ok
2> application:get_env(ch_app, file).
{ok,"testlog"}
```

如果要使用发布处理（release handing），正好要使用到一个叫做sys.config的系统配置文件。

最后要注意.app文件中的值和系统配置文件中的值可以通过命令行参数来覆盖：

```erlang
% erl -ApplName Par1 Val1 ... ParN ValN
```

example：

```erlang
% erl -ch_app file '"testlog"'
Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]

Eshell V5.2.3.6  (abort with ^G)
1> application:start(ch_app).
ok
2> application:get_env(ch_app, file).
{ok,"testlog"}
```



### 应用启动类型

启动应用时，定义启动类型：

```erlang
application:start(Application, Type).
```

application:start(Application)和调用application:start(Application, temporary)是一样的。Type也可以是permanent或transient：

- 如果一个permanent应用终止，所以其他应用和erlang运行时系统也会被终止。
- 如果一个transient应用以normal原因终止，则会报告终止信息，但是其他应用不会被终止；如果一个transient应用非正常终止，也就是以normal以外的原因终止，所有其他应用和运行时系统也会被终止。
- 如果一个temporary应用被终止，会报告终止信息但是其他应用不会被终止。

transient模式的实际作用不大，因为当一颗监督树终止时，会将原因设置为shutdown而非normal。



应用程序总是可以通过调用application:stop/1显式地停止。不管是哪种模式，都不会影响其他应用程序。





