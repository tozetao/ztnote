## Workerman对象关系

- Worker进程容器：Worker主进程，fork以及管理多个Worker子进程 
- Worker子进程：管理请求连接，与Connection连接对象是一对多关系
- Connection对象：将客户端的socket连接抽象成Connection对象

## Worker
在workerman中，进程被抽象成Worker类，Worker进程负责监听端口，并行的接受客户端连接，并处理连接事件。

### 1. 连接事件
Worker进程负责管理和处理请求连接，也能设置请求连接（Connection）触发的事件，由于Worker进程负责管理所有连接，在Worker进程设置的事件是作用于所有请求连接的。

具体连接事件有：
- onConnect：socket建立连接触发的事件，每个连接都会触发一次回调。
- onMessage：当客户端连接上有数据发来时触发

### 2. Worker::listen()
listen()方法可以用于实例化worker后执行监听，最重要的作用是能够实现一个进程监听多个端口，在worker进程启动后动态创建新的Worker实例对象。

注：php版本小鱼7.0的都不支持多个进程监听多个端口，在实例化相同端口的Worker是会报端口被占用。
	




## TcpConnection
TcpConnection代表请求的连接对象，即socket连接对象。
TcpConnection对象与Worker对象是多对一关系，一个进程管理者多个连接对象。

### 1. 属性
TcpConnection对象有自己的属性，例如id，就是该连接对象的标识，用于区别其他连接对象。

### 2. 事件
TcpConnection对象也有自己的事件，与Worker进程对象的事件相同，不同的是该事件只会针对该连接触发。

### 3. 数据的发送
	
