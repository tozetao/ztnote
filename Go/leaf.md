### Module

模块的注册与销毁分析。

在leaf中，游戏服务器是以module组成的。

在module包中，module.go文件定义了Module接口，并提供了注册、初始化、销毁Module的公共方法。

```go
type Module interface{
    func OnInit()
	func Run(closeSig chan bool)
    func OnDestory()
}

var mods []*Module

type module struct {
    mi	Module
    closeSig	chan bool
    wg	sync.WaitGroup
}

func Register(mi Module){}

// 初始化模块
func Init() {
    for i := 0; i < len(mods); i++ {
        mods[i].mi.OnInit()
    }
    
    // 为每个Module启动一个goroutine来运行
    for i := 0; i < len(mods); i++ {
        m := mods[i]
        m.wg.Add(1)
        go run(m)
    }
}

func run(m *module) {
    // 阻塞，等待关闭信号
    m.mi.Run(m.closeSig)
    // wg计数器减1
    m.wg.Done()
}
```

closeSig字段是一个channel，它是Module的关闭信号，在每个Module运行时会作为参数传入。Module在运行时会读取closeSig，如果没有向closeSig发送信号模块是会一直阻塞的，直到收到信号。

```go {
// gate/gate.go
// 比如gate模块的Run()方法，在实现时就是一直读取closeSig通道的值，等待关闭信号
func Run(closeSig chan bool) {
    //...
    <-closeSig
    //...
}

// module/module.go
// 调用Destory方法时，就会向每个模块发送结束信号。
func Destory() {
    for i := len(mods) - 1; i >= 0; i-- {
        m := mods[i]
        m.closeSig <- true
        m.wg.Wait()
        destory(m)
    }
}

func destory(m *module) {}
```







### leaf/gate

网关模块，负责连接的接入。gate模块决定了消息会路由给内部的哪个模块来处理。

leaf/gate/gate.go实现了消息转发。它有俩个重要的对象：Gate对象与agent对象。

Gate对象实现了gate模块的主要逻辑，它会根据配置来初始化服务器并启动，开始监听连接的接入。

```go
type Gate struct {
    // 消息处理器
    Processor     *network.Processor
    
    // rpc服务
    AgentChanRPC  *chanrpc.Server
    
    // tcp config..
    // websocket config..
}

// Run(closeSig chan bool)的实现
func (gate *Gate)Run(closeSig chan bool) {
    // tcpServer的初始化
    var tcpServer *network.TCPServer
    if gate.TCPAddr != "" {
        tcpServer = new(network.TCPServer)
        tcpServer.Addr = gate.TCPAddr
		tcpServer.MaxConnNum = gate.MaxConnNum
        // ...
    }
    
    // 启动TCPServer
    if tcpServer != nil {
        tcpServer->Start()
    }
    
    // 等待关闭信号
    <-closeSig
    
    // 关闭TCPServer
    if tcpServer != nil {
        tcpServer->Close()
    }
}
```



agent对象是代理的意思，每个连接都会委托一个agent对象来进行处理。agent会一直读取连接的消息，而消息则交由gate模块设置的消息处理器来处理，消息的解码和路由都是由消息处理器负责。

agent对象实现了network.Conn接口。

```go
type agent struct {
    // TCPConn对象
    conn     network.Conn
    
    // gate模块对象
    gate     *Gate
    userData interface{}
}

// 读取消息，将消息交由消息处理器来处理。
func (a *agent) Run(){}

// 写入数据，响应连接
func (a *agent) WriteMsg(msg interface{}) {}

// 调用TCPConn对象Close方法
func (a *agent) Close(){}

// 同上，调用TCPConn对象Destory方法
func (a *agent) Destory(){}

// 出发OnClose事件
func (a *agent) OnClose(){}
```

agent对象相当于会话一样，它是由Gate对象、network.Conn对象的实现组成。除了读取连接的消息，通过agent可以写入数据，响应连接。



### leaf/network

TCPServer分析：network/tcp_server.go包含了TCP服务器的主要实现。

```go
type TCPServer struct {
    PendingWriteNum    int
    
    conns              ConnSet
    mutexConns         sync.Mutex
    wgConns            sync.WaitGroup
    
    wgLn               sync.WaitGroup
    
    NewAgent           func(*TCPConn) Agent
    
    // msg parser
}
```

ConnSet是一个map，通过类型定义的。ConnSet字面意思是一个集合，用于存储net.Conn对象。

```go
type ConnSet map[net.Conn]struct{}
```

wgLn是为了保证net.Listener对象能够被正常关闭。

mutexConns是读写锁，为了保证conns集合添加、移除连接的原子性。

wgConns是为了保证所有连接能够被正确关闭。

这三个字段后续会看到如何实现的。





TCPServer的初始化：

在初始化时会开启一个goroutine来运行，因为leaf可以同时处理tcp连接和websocket，tcp在accept是会处于阻塞状态，因为需要开启一个goroutine运行，避免阻塞。

```go
func (server *TCPServer) Start() {
    server.init()
    go server.run()
}
```



TCPServer的运行：

```go
func (server *TCPServer) run() {
    // wgLn保证了服务器能够被正确关闭
    server.wgLn.Add(1)
    defer server.wgLn.Done()
    
    for {
        // 接受请求
        conn, err := server.ln.Accept()
        
        // 计数器加1
        server.wgConns.Add(1)
        
        // 封装net.Conn对象
        tcpConn := newTCPConn(conn, server.PendingWriteNum, server.msgParser)
        // 开启一个goroutine，交给agent处理连接
        agent := server.NewAgent(tcpConn)
        go func() {
            agent.Run()
            // 既然连接委托给agent对象处理，为什么要主要调用tcpConn来关闭呢?
            tcpConn.Close()
            // 出发close事件
            agent.OnClose()
        }()
    }
}
```

服务器在运行时，wgLn会进行计数，因为run方法是开启一个goroutine在运行的，为了保证该协程运行完毕后服务器才去清理资源，在TCPServer的Close()方法可以看到实现。

没接受一个连接就会创建一个agent对象，有agent对象处理连接发送过来的消息。每个agent都是运行在单独的goroutine中。



TCPServer的关闭：

```go
func (server *TCPServer) Close(){
    // 关闭ln对象，
    server.ln.Close()
    
    // 等待Run()方法执行完毕
    server.lnWg.Wait()
    
    // 关闭所有连接
    for conn := range server.conns {
        conn.Close()
    }
    
    // 等待所有连接执行完毕，才关闭服务器
    server.wgConns.Wait()
}
```





TCPConn的分析：

tcp_conn.go是对net.Conn的再次封装，提供了公共接口来操作net.Conn。

TCPConn的初始化：

```go
func newTCPConn(conn net.Conn, pendingWriteNum int, msgParser *MsgParser) *TCPConn {
    // TCPConn的初始化
    tcpConn := new(TCPConn)
    tcpConn.writeChan = make(chan []byte, pendingWriteNum)
    
    // 监听要写入的数据
    go func() {
        for b := range tcpConn.writeChan {
            // 在收到nil字节时，终止监听可写入数据
            if b == nil {
                break
            }
            // 写入发生错误终止循环
            _, err := conn.Write(b)
            if err != nil {
                break
            }
        }
        // 关闭连接
        conn.Close()
    }
    
    return tcpConn
}
```

TCPConn在初始化时会开启一个goroutine用于监听要写入的数据。

TCPConn对象会有一个writeChan，它是一个带有缓冲的字节类型的channel，如果要向该连接写入数据，只需要向该通道写入数据即可。初始化开启的goroutine会收到向writeChan写入的数据，然后输出连接。

PendingWriteNum决定了writeChan的缓冲大小。





疑问1：accept不是顺序执行的?为什么TCPServer的Run方法中添加conn时需要加锁?

疑问2：TCPCon在写入数据的时候，为什么要判断writeChan的长度等于writeChan的容量大小。





### MsgParser

消息解析器，根据消息格式读取数据与写入数据。一般在长连接中都会使用以下格式：

> len | data

len字段表示data字段的长度，len本身的长度决定了单个消息的最大大小。data字段可以使用json活probuffer编码。在leaf中，MsgParser是network/tcp_msg.go实现的。



消息体写入分析：

```go
type MsgParser struct {
    // len字段的长度
    lenMsgLen    int
    
    // data字段的最小长度
    minMsgLen    int
    
    // data字段的最大长度
    maxMsgLen    int
    
    littleEndian bool
}

func (mp *MsgParser) Write(conn *TCPConn, args ...[]byte) {
    // get len
    var msgLen uint32
    for i := 0; i < len(args); i++ {
        msgLen += uint32(len(args[i]))
    }
    
    msg := make([]byte, mp.lenMsgLen + msgLen)
    
    // write len
    binary.LittleEndian.PutUint16(msg, uint16(msgLen))
    
    // write data
    
    // write conn
}
```

MsgParser的Write()方法支持写入多个字节切片，整个消息体的字节序列组成分为俩个步骤：

- 写入len字段：首先计算消息体内容的长度，再转换字节序写入到一个字节切片中。
- 将消息体的字节数据附加到之前写入的字节切片中。



消息体读取分析：

- 先读取len，获取data的长度
- 根据data的长度读取后续数据。





### Processor

消息处理器，有json和probuffer俩种类型的消息处理器，位于leaf/network包中。

Processor负责对消息体进行编码和解码，编码与解码方式取决于具体实现，leaf自带了json、probuffer俩种编码实现。

network/processor.go文件定义了Processor接口。

```go
type Processor interface {
    // 路由消息体
    Route(msg interface{}, userData interface{}) error
    
    // 解码数据
    Unmarshal(data []byte) (interface{}, error)
    
    // 编码数据
    Marshal(msg interface{}) ([][]byte, error)
}
```



以下是network/json.go的分析，它实现了JSON格式消息体的解析、编码以及消息体的路由。每个消息体会路由到对应的Module、处理该消息体的回调函数。我们使用MsgInfo结构体来封装这些信息。

```go
type Processor struct {
    msgInfo    map[string]*MsgInfo
}

type MsgInfo struct {
    msgType    reflect.Type
    msgRouter  *chanrpc.Server
    msgHandler    MsgHandler
    msgRawHandler MsgHandler
}

type MsgRaw struct {
	msgID    string
    msgRawData json.RawMessage
}

type MsgHandler func([]interface{})
```



消息体的注册：

Register实现了消息体的注册，该方法会初始化MsgInfo结构体，然后使用消息体的名字与MsgInfo结构体做一个映射。

```go
func (p *Processor) Register(msg interface{}) string {
    // 源代码略
}
```



消息体的路由设置：

SetRouter用于设置消息体的chanrpc，之前说过模块之间是通过chanrpc来交互的，设置消息体的chanrpc，就是将该消息体交由给该chanrpc的模块来处理。

```go
function (p *Processor) SetRouter(msg interface{}, msgRouter *chanrpc.Server) {
    // 反射，解析msg参数
    msgType := reflect.TypeOf(msg)
    
    // 获取msg的结构体名称
    msgID := msgType.Elem().Name()
    
    // 设置消息体的chanrpc
    i, ok := p.msgInfo[msgID]
    i.msgRouter = msgRouter
}
```





消息体是如何路由的?

路由一个消息体便是执行前面所设置的chanrpc和回调函数。

```go
func (p *Processor) Route(msg interface{}, userData interface{}) {
    // raw，原生消息的处理。
    
    // 反射，解析msg参数
    msgType := reflect.TypeOf(msg)
    
    msgID := msgType.Elem().Name()
    
    i, ok := p.msgInfo[msg]
    // 执行对应的回调函数
    if i.msgHandler != nil {
        i.msgHandler([]interface{}{msg, userData})
    }
    // 执行对应的chanrpc
    if i.msgRouter != nil {
        i.msgRouter.Go(msgType, msg, userData)
    }
}
```



原材料：

- 面包片
- 培根
- 午餐肉
- 饺子
- 榨菜
- 鸡蛋



饮品：

- 牛奶
- 麦片



水果：

- 香蕉





### skeleton

skeleton实现了Module接口的Run方法，并提供了：

- ChanRPC
- goroutine
- 定时器



除了Gate模块，其他模块的创建依赖于skeleton。

模块的初始化就是skeleton的初始化，它会创建chanrpc的server端和client端，skeleton的结构体如下：

```go
type Skeleton struct {
    GoLen    int
    TimerDispatcherLen    int
    AsynCallServer        int
    
    g                     *g.Go
    dispatcher            *timer.Dispatcher
    
    ChanRPCServer         *chanrpc.Server
    server                *chanrpc.Server
    client                *chanrpc.Client
    commandServer         *chanrpc.Server
}
```

ChanRPCServer与server字段指向同一个chanrpc.Server对象，同时ChanRPCServer也是暴漏给外部的字段。



skeleton的运行：

skeleton会利用多路复用，监听多个channel，执行对应操作。

```go
func (s *Skeleton) Run(closeSig chan bool) {
    for {
        select {
        // 监听关闭信号   
        case <-closeSig:
        // 监听server的ChanCall通道
        case <-s.server.ChanCall:
        // 监听其他...
        }
    }
}
```









### leaf/go

用于创建能够被leaf管理的goroutine。







### leaf/module

提供注册模块、释放模块，启动框架的方法。





### leaf/chanrpc

每个模块运行在单独的goroutine中的，leaf提供一套基于channel的RPC机制，用于游戏模块间通讯。

调用有三种方式：

- 同步调用
- 异步调用
- Go调用

```go
type RetInfo struct {
    ret  interface{}
    err  error
    cb   interface{}
}
```

cb字段是回调函数，函数可以是以下类型：

```go
func (err error)
func (ret interface{}, err error)
func (ret []interface{}, err error)
```

ret字段允许的类型有：nil、





```go


type CallInfo struct {
    f     interface{}
    args  []interface{}
    
}

type Server struct {
    functions map[interface{}]interface{}
    
    ChanCall  chan *CallInfo
}
```

Server结构体使用map来保存消息体与回调函数之间的映射关系，ChanCall是一个CallInfo类型的通道。







chanrpc是如何交互的？







chanrpc如何使用?

websocket通讯?