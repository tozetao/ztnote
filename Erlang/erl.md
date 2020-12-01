

## erl

erl命令的详细介绍在文档：https://erlang.org/doc/man/erl.html，这里只关注常用部分。

erl用于启动一个erlang运行时系统，具体的细节取决于系统。



windows用户大概需要使用werl来替代，它会启动一个有滚动条，并支持命令行编辑的窗口。

windows上的erl程序在shell中不提供编辑功能。如果你想在管道中或标准输入或输出时，必须使用erl程序。



> erl arguments

arguments可以分为普通参数、flag和emulator flag。

- 任何以字符+开头的参数都被解释为emulator flag，如名称所示，emulator flag控制着emulator的行为。

- 任何以字符-开头的参数都被解释为一个flag。这个flag要传递给erlang运行时系统，跟具体的说是传递给init系统进程。

  注意有少量的 - flag实际上是emulator flag，见下面描述。

- 普通参数不会被任何方式解释，它们也由init进程存储，可以通过调用init:get_plain_arguments/0进行检索。

  普通参数可以出现在第一个标志之前，也可以出现在 - flag之后。另外-extra会使后面的所有参数都变成普通参数。

example：

> erl +W w -sname arnie +R 9 -s my_init -extra +before

这里+W和+R都是emulator flag。

-s my_init是一个init标志，由init进程解释；-sname arnie是一个用户标志，由init进程存储，它会被Kernel读取并让erlang运行时系统成为分布式的。

最后-extra标志后的参数都是普通参数，调用init:get_plain_arguments()可以获取到。

> erl -myflag 1

这里用户标识-myflag 1会传递给init进程并存储，它是一个用户定义的标志，可能会被用户定义的程序使用。





### flags

- --

  init flag。

  所有在 -- 之后的直到下一个flag中的参数都被视为普通参数。这些参数可以通过init:get_plain_argments/0得到

- -s Mod [Func [Arg1, Arg2, ...]]

  init flag。

  使init进程调用指定模块的函数，Func默认值是start。

  如果没有提供参数值，则假定函数参数个数为0，否则假定为1，参数将是一个[Arg1, Arg2, ...]的列表。

  所有参数将被作为原子传递，详见init(3)

- -extra

  init flag。

  所有再-extra后面的参数都被认为是普通参数。可以通过使用inet:get_plain_argemtns/0获取普通参数。





- -pa Dir1 Dir2

  将指定的目录添加到代码路径的头部，相当于code:add_pathsa/1。注意指定的目录在结果目录中会被颠倒。（什么意思？？？）

  作为-pa的替代方法，如果要在代码路径中添加多个目录，并且这些目录有一个共同的父目录，可以在环境变量ERL_LIBS中指定父目录，参见code(3)。



- -setcookie Cookie

  设置节点的cookie，详见erlang:set_cookie/2

- -config Config

  指定一个或多个配置文件的名称，Config.config，用于配置应用程序。



- -name Name

  使Erlang运行时系统成为一个分布式节点，此标志调用节点成为分布式网络节点中的一员，参见net_kernel(3)。在erlang启动之前，还需要确保epmd在当前主机上运行，参见empd(1)和-start_empd选项。

  节点名将是Name@Host，Host是当前主机的完全限定主机名。对于简短的主机名，可以使用-sname标志来替代。

  如果Name是undefined，节点将以特殊模式启动并被优化为另一个节点的临时客户端。启用时，节点将从它链接到的第一个节点请求动态节点名。

  此外这些分布式配置也会被设置：-dist_listen false -hidden -dist_auto_connect never，因为-dist_auto_connect被设置为never，所以系统必须手动调用net_kernel:connect_node/1才能启动分布式。

  如果分布式通道是关闭的，当一个节点使用动态节点名时，该节点将停止分布，必须重新调用net_kernel:connect_node/1。

  注：如果丢弃了分布，再重新设置，节点名可能会被改变。

  这里翻译不完整，回头再仔细翻译下。







### emulator flags

- +P Number

  设置当前系统中可同时存在的进程数量。Number有效范围是[1024-134217727]，默认值是262144。

  注意：实际运行时系统选择的最大值可能比传递的Number大得多。目前运行时系统通常会选择一个2的幂数，但将来可能改变。

  可以通过erlang:system_info(process_limit)来检查实际选择的值。

  

  











## werl

erlang模拟器。

在windows中，启动erlang系统进行交互的首选方法如下：

> werl arguments

这将在windows中启动erlang，并提供完整的命令行编辑器。除了-oldshell之外的所有标志都与erl(1)是一样的。







