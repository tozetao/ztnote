基础接口

基础接口定义在框架根目录下。

/peer.go文件定义了通信端的基本接口。

```go
//GenericPeer定义了基本通用端接口，它由Peer、PeerProperty俩个接口组合而成。
type GenericPeer interface {}

//定义访问Session的接口
type SessionAccessor interface {}
```



/peer_tcp.go文件定义了TCP通信端的基本接口。

```go
//tcp接收器的接口
type TCPAcceptor interface{}

//tcp连接器的接口
type TCPConnector interface{}
```



/processor.go文件定义事件的基本接口。

```go
type Event interface {
    Session() Session
    
    Message() interface{}
}

type MessageTransmitter interface {}

type EventHooker interface {}
```



/session.go文件定义了Session的操作接口。

```go
type Session interface {}
```











### Peer

在cellnet框架中，客户端使用Connector连接服务器，而服务器使用Acceptor接收多个客户端。在连接建立成功后，客户端和服务端都会各自生成一个会话（Session）用于处理数据的收发流程。

对于Connector和Acceptor，cellnet统称为通信端（Peer）。





/tcp/acceptor.go文件实现了接收器。

```go
type tcpAcceptor struct {
    peer.SessionManager
	peer.CorePeerProperty
	peer.CoreContextSet
	peer.CoreRunningTag
	peer.CoreProcBundle
	peer.CoreTCPSocketOption
	peer.CoreCaptureIOPanic

	// 保存侦听器
	listener net.Listener
}
```

tcp接收器是由多个核心结构体组成的结构体。



peer.CoreRunningTag，该结构体位于/peer/runningtag.go文件中，定义通信端运行状态的相关信息。

```go
type CoreRunningTag struct {
    //运行状态
    running int64
    //停止状态
    stopping int64
    //
    stoppingWaiter sync.WaitGroup
}
```

peer.CoreProcBundle，该结构体位于/peer/procbundle.go文件中，实现了事件相关的方法。





### Session

客户端和服务器使用Session处理收发包流程。收发包的流程将事件通过事件回调派发。

session的基础接口定义在/session.go中，在不同的通信端包中都实现了各自的Session。







### SessionManager

会话管理器，用于管理连接建立后产生的会话。

SessionManager定义了管理Session的新增、移除等基本接口，位于/peer/sessmgr.go文件中。

```go
type SessionManager interface {
    Add(cellnet.Session)
    Remove(cellnet.Session)
    Count() int
    
    cellnet.SessionAccessor
}
```

sessmgr.go文件不仅定义Session管理的接口，同时定义了CoreSessionMananger结构体来实现这些接口。



SessionAccessor定义了访问会话的接口，位于/peer.go文件中。

```go
type SessionAccessor interface {
    // 获取一个连接
	GetSession(int64) Session

	// 遍历连接
	VisitSession(func(Session) bool)

	// 连接数量
	SessionCount() int

	// 关闭所有连接
	CloseAllSession()
}
```







### 事件

事件是在某种情况满足条件下产生的。

/event.go定义了收发数据时触发的事件类型。

```go
//接收消息事件对象，在接收到消息时会触发该类型的事件
type RecvMsgEvent struct {
    Ses cellnet.Session
    Msg interface{}
}

//发送消息事件对象，在发送消息时会触发该类型的事件
type SendMsgEvent struct {
    Ses cellnet.Session		//触发事件对应的会话
    Msg interface{}			//发送的消息
}
```



ReplyEvent定义了普通消息接口。

```go
// 普通消息接口
type ReplyEvent interface {}
```





### 事件队列

/queue.go文件定义并实现了事件队列接口。

事件队列是通过数据通道来实现的，/pipe.go文件实现了一个数据通道（Pipe），Pipe可以将数据加入到通道中，当需要这些数据时，可以将数据列表复制到指定的参数中。

```go
type eventQunue struct {
    *Pipe
}

type Pipe struct {
    list	   []interface{}
    listGuard  sync.Mutex
    listCond   *sync.Cond
}
```









处理数据时发生错误也会触发事件，分别是：RecvErrorEvent和SendErrorEvent事件。

```go
type RecvErrorEvent struct {}
type SendErrorEvent struct {}
```

这俩种事件表示用户需要检查收发包流程是否发生问题，以及收集日志错误。



在建立连接后会触发SessionStartEvent事件。

- ConnectedEvent事件

  Connector发起连接成功时触发的事件，代码中使用别名将SessionStartEvent事件命名为ConnectedEvent事件。

- AcceptedEvent事件

  如果是Acceptor接收到连接，则是AcceptedEvent事件。

- SessionClosedEvent

  该事件表示会话连接关闭时触发的事件。







编码

编码是指将数据以指定的格式进行组织，定义了通信端之间的数据交互格式。

在框架的/codec.go定义了编码接口，只要实现该接口就可以自定义自己的编码格式。同时框架在/codec文件夹中也实现了默认编码器。



### 消息元信息

将要发送的消息ID、消息名称、消息类型（结构体或对象）和编码抽象为消息元信息，即MessageMeta。在/meta.go文件定义了消息元信息接口和对象。



### 消息的编码和解码

cellnet在这里将消息类型、消息ID分别与消息元信息建立了关联关系，通过消息类型或消息ID就可以找到消息元信息。



### 封包与拆包

变成封包由俩部分组成：包体长度与包体数据。包体长度为2个字节，它的类型为uint16，包体长度决定了包体数据的字节大小。

/util/packet.go文件实现了封包与拆包的过程。







问题：

- Peer的开启与关闭?

  tcpAcceptor

- 如何处理一个新连接?

  每个新的连接都是一个新的Session

- 如何拆包与解包

  MessageTransmittera负责拆包与解包，使用util目录的工具类来处理的。



- 如何接收数据

  Session会开启一个协程，专门接收数据。

  将数据拆包成消息后，就是一个接收消息事件，将消息事件交给Processor去处理，Processor则会调用客户端回调函数来处理接收到的消息。

- 如何发送数据

  Session会开启一个协程，循环遍历数据队列，如果有数据时，则发送数据。



- 如何处理一个事件?

  触发一个事件时，会调用ProcEvent()方法来进行处理，其实就是使用EventCallback事件回调方法来处理一个事件的发生。

  EventCallback会调用SessionQueuedCall()方法来处理事件，如果有事件队列则投递给事件队列处理，如果没有则执行执行回调函数。

  

- 如何投递事件?

  

- 处理了哪些socket选项?

- 会话管理的实现?

  由一个SessionID为键，Session对象为值的map对象实现。

  





- 事件队列的实现?
- 发送数据队列的实现?



Processor

- Processor的注册

  不同的通信端有不同的Processor，而Processor针对不同的通信端依赖的处理模块也不同。

  Processor的注册便是根据通信端来注册Processor的依赖模块：MessageTransmitter、EventHooker和Callback。

- Processor的绑定

  绑定是将通信端（Peer）的Processor（处理器）所依赖的处理模块与客户回调函数一起绑定起来。

  依赖模块就是上面的MessageTransmitter、EventHooker和EventCallback。









