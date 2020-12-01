### net_kernel

net kernel是一个系统进程，注册为net_kernel，它必须运行分布式的erlang才能工作。这个进程的目的是实现部分BIF的spawn/4和spawn_link/4，并对网络进程监控。



一个erlang节点是通过-sname或-name标志来启动的。

> erl -sname foobar

也可以直接从普通的erlang shell中调用net_kernel:start([foobar])。

> net_kernel:start([foobar, shortnames])

当调用成功后该erlang shell就转换成一个分布式节点了。



通常情况下，当另一个节点被应用时，连接会自动建立。这个功能可以通过内核参数dist_auto_connect设置为never禁用，参见kernel(6)。在这种情况下必须显示通过connect_node/1来显示建立连接。



哪些节点允许通信是由cookie系统处理的，具体参考erlang reference manual的distributed章节，讲了关于分布式内容。



警告：

启动分布式节点时，如果不同时指定-proto_dist inet_tls，就会使节点收到攻击，从而使攻击者可以完全访问节点，并扩展到集群。

当使用不安全的的分布式节点时，请确保网络的配置能将潜在的攻击者拒之门外。

see the using SSL for erlang distribution user's guide for details on how to setup a secure distributed node.







```erlang
monitor_nodes(Flag) -> ok | Error
monitor_nodes(Flag, Options) -> ok | Error
                                 
Flag = boolean()
Options = [Option]
Option = {node_type, NodeType} | nodedown_reason
NodeType = visible | hidden | all
Error = error | {error, term()}
```

调用进程订阅或者取消定于节点的状态消息。

当一个新节点被连接时，会先所有订阅进程发送节点nodeup消息；当一个节点被断开连接时，会发送nodedown消息。

如果Flag为true，则开始一个新的订阅。如果Flag为false，则停用所有之前用相同选项启动的订阅。如果俩个option lists包含相同的选项集，那么它们被认为是相同的。



从Kernel 2.11.4版本和ERTS 5.5.4版本开始，保证以下几点：

- nodeup消息是通过新建立的连接从远程节点传递过来的，它可以保证在传递任何消息之前先传递nodeup消息。
- nodedown消息不会被传递，直到所有来自远程节点的消息通过连接被传递。





通过新建立的连接

从远程节点