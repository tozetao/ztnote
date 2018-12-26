TCP是一种面向连接的、可靠的、基于字节流的通信协议，数据在传输时需要建立连接，传输完毕后要断开连接。







TCP数据报结构

```c
struct tcp_package {
    short source_port;	//源端口
    short target_port;	//目标端口
    int seq;	//Sequence Number，序号，占32位
    int ack;	//Acknowledge Numb，确认号，占32位
    //6个标志位
    //其他略...
};
```

重点关注以下字段：

- seq

  Sequence Number，序号，占32位，用来标识从计算机A发送到计算机B的数据包的序号，在发送数据包时会对此进行标记。

- ack

  Acknowledge Number，确认号，32位，客户端和服务端都可以发送，ack = seq + 1

标志位有6个，每个标志位占1个字节，分别是：

-  URG：紧急指针（urgent pointer）有效。
-  ACK：确认序号有效。
-  PSH：接收方应该尽快将这个报文交给应用层。
-  RST：重置连接。
-  SYN：建立一个新连接。
-  FIN：断开一个连接。

SYN是Synchronous的缩写，表示同步的；FIN是Finish的缩写，表示完成；seq是Sequence的缩写，表示序列；ack是Acknowledge，表示确认。



三次握手

使用connect()建立连接时，客户端与服务端会相互发送3个数据包。

客户端调用connect()发起连接。

TCP协议会组建一个数据包，并设置SYN标志位，同时随机生成一个数字1000填充到Seq字段，表示该数据包的序号，组建完后将数据包发出，这时客户端处于SYNC-SEND状态。



响应连接

服务端收到数据包后进行解析，检测到只有SYN标志位，因此知道该数据包是建立请求的数据包。服务端也会组建一个数据包，它会设置数据包的SYN标志位和ACK标志位，



俩个标志位，俩个字段，Seq字段，Ack字段，ack标志位，SYN标志位。



三次握手的关键是要确认对方收到了自己的数据包，这个目标是通过"确认号（Ack）"字段实现的。计算机会记录下自己发送的数据包序号Seq，待收到对方的数据包后，检测"确认号（Ack）"，看Ack = Seq + 1是否成立，如果成立说明对方收到自己的数据包。