## net_kernel

net kernel是一个系统进程，注册为net_kernel，它必须运行分布式的erlang才能工作。这个进程的目的是实现部分BIF的spawn/4和spawn_link/4，并对网络进程监控。



一个erlang节点是通过-sname或-name标志来启动的。

> erl -sname foobar

也可以直接从普通的erlang shell中调用net_kernel:start([foobar])。

> net_kernel:start([foobar, shortnames])

当调用成功后该erlang shell就转换成一个分布式节点了。



通常情况下，当另一个节点被应用时，连接会自动建立。这个功能可以通过内核参数dist_auto_connect设置为never禁用，参见kernel(6)。在这种情况下必须显示通过connect_node/1来显示建立连接。

哪些节点允许通信是由cookie系统处理的，具体参考erlang reference manual的distributed章节，讲了关于分布式内容。



- 警告：

  启动分布式节点时，如果不同时指定-proto_dist inet_tls，就会使节点收到攻击，从而使攻击者可以完全访问节点，并扩展到集群。

  当使用不安全的的分布式节点时，请确保网络的配置能将潜在的攻击者拒之门外。

  see the using SSL for erlang distribution user's guide for details on how to setup a secure distributed node.





### monitor_nodes/1

```erlang
monitor_nodes(Flag) -> ok | Error
monitor_nodes(Flag, Options) -> ok | Error
                                 
Flag = boolean()
Options = [Option]
Option = {node_type, NodeType} | nodedown_reason
NodeType = visible | hidden | all
Error = error | {error, term()}
```

调用进程订阅或者取消订阅节点的状态消息。

当一个新节点被连接时，会先所有订阅进程发送节点nodeup消息；当一个节点被断开连接时，会发送nodedown消息。

如果Flag为true，则开始一个新的订阅。如果Flag为false，则停用所有之前用相同选项启动的订阅，俩个option lists包含相同的选项集，那么它们被认为是相同的。



从Kernel 2.11.4版本和ERTS 5.5.4版本开始，保证以下几点：

- nodeup消息是通过新建立的连接从远程节点传递过来的，它可以保证在传递任何消息之前先传递nodeup消息。
- nodedown消息不会被传递，直到所有来自远程节点的消息通过连接被传递。

请注意对于2.13之前的内核版本，这一点不保证。



节点状态变化消息的格式取决于Options参数，如果Options是[]，这是默认值，它的消息格式如下：

```erlang
{nodeup, Node} | {nodedown, Node}
Node = node()
```

如果Options非空，则消息格式如下：

```erlang
{nodeup, Node, InfoList} | {nodedown, Node, InfoList}
Node = node()
InfoList = [{Tag, Val}]
```

InfoList是一个元组列表，元素内容依赖于Options。

另外当OptionList == []时，只监控可见的节点，也就是出现在erlang:nodes/0结果中的节点。

Option的值可以是{node_type, NodeType}，NodeType的有效值可以是：

- visible：仅订阅可见节点的状态变化消息，InfoList包含了元组{node_type, visible}。
- hidden：仅订阅隐藏节点的状态变化消息，InfoList包含了元组{node_type, hidden}。
- all：订阅所有节点的状态变化消息，InfoList将会包含元组{node_type, visible | hidden}。

Option的值也可以是nodedown_reason，这种情况在nodedown消息中的InfoList会包含元组{nodedown_reason, Reason}。

根据使用的分布式模块或进程的不同，Reason可以是任意元组。但对于标准的TCP分布式模块来说，它是以下任何一种。

- connection_setup_failed

  连接设置失败，在发送nodeup消息之后。

- no_network

  没有网络

- net_kernel_terminated

- shutdown

  未指定的连接关闭

- connection_closed

  连接被关闭

- disconnect

  连接被断开，被迫从当前节点断开

- net_tick_timeout

  Net tick超时

- send_net_tick_failed

  未能通过连接发送net tick

- get_status_failed

  从保持连接的端口检索状态信息失败。









### connect_node/1

```erlang
connect_node(Node) -> bool
Node = node()
```

建立一个Node节点连接，如果连接建立或者已经建立，或者Node是本地节点则返回true。如果连接建立失败则返回false。如果本地节点不存在则忽略。







erlang:nodes/0

获取除了自身节点外的所有节点。





我的理解：

进程去订阅节点的状态消息，也就是说相当于有一个net_kernel进程，它会关注有哪些节点连接或断开连接，节点所触发的这种动作都会通知已订阅的进程。



问题：

连接的断开是分为多少种情况呢？

一个节点断开会通知其他节点，那么其他节点会收到什么消息？

当前节点可以断开吗？



比如说P节点与C节点都订阅了节点状态消息，P节点作为父节点，C节点作为子节点，接着C节点与P节点建立连接，那么一般来说应该是C节点