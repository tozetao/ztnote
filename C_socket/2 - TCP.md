### Functions

在linux中，一切都是文件，硬件、socket都可以认为是文件。所有在Linux系统中创建的文件都有一个int型的编号，称为文件描述符，使用文件时只需要知道编号即可。

socket也是一种文件，所以在网络数据传输时可以使用与文件I/O相关的函数，可以认为俩台计算机之间的通信，实际上是俩个socket文件的相互读写。



```c
#include <sys/socket.h>
int socket(int af, int type, int protocal)
```

创建一个socket。

- af

  address family，地址族的意思。表示IP地址的类型，常用的有AF_INET和AF_INET6，AF_INET表示IPv4地址，AF_INET6表示IPv6地址。

  也可以使用PF前缀，Protocal Family的意思。PF_INET等于AF_INET，PF_INET6等于AF_INET6。

- type

  数据传输方式，例如SOCK_STREAM、SOCK_DAGRM

- protocal

  表示传输协议，TCP协议为IPPROTO_TCP，UDP协议为IPPROTO_UDP。

说明：因为可能有多张协议支持同一个数据传输方式和地址族，所以需要protocal参数。TCP协议与UDP协议都只有一种情况满足，所以protocal参数可以写0



```c
bind(int sock, struct sockaddr *addr, socklen_t addrlen);
```

sockt()用于确定套接字的属性，bind()函数用于将套接字与特定的IP地址和端口绑定起来。只有这样，流经过该IP地址和端口的数据才能交给套接字处理，而客户端需要使用connect()函数建立连接。

sock参数是socket文件描述符，addr参数是一个sockaddr的结构体，指定绑定的IP与端口等信息，addrlen参数是该结构体的大小。

```c
struct sockaddr {
    sa_family_t sin_family;	//地址族
    char sa_data[14];		//IP地址和端口号
}

struct sockaddr_in {
    sa_family_t sin_family;	//地址族，也就是地址类型
    uint16_t sin_port;		//16位的端口号
    struct in_addr;			//32位IP地址
    char sin_zero[8];		
}
```

sockaddr和sockaddr_in结构体的长度一致，都是16个字节，sockaddr是使用sa_data成员将IP地址和端口合并到一起，它的赋值比较麻烦，所以一般会使用sockaddr_in结构来代替sockadd结构。

sockaddr_in的各个参数意义如下：

- sin_family

  地址族，与socket()函数的参数一致，占用俩个字节。

- sin_port

  端口号，uint16_t长度为俩个字节，端口号需要使用htons()转换。

- in_addr

  in_addr是一个只包含一个成员的结构体。

  ```c
  #include <netinet/in.h>
  in_addr_t s_addr;
  ```

  in_addr等价于unsigned long，长度为4个字节。s_addr是一个整数，所以需要使用inet_addr()函数转换。

- sin_zero

  一般填充为0，没什么用。

sockaddr_in结构用于表示IPv4的地址，如果要表示IPv6的地址，可以使用sockaddr_in6结构。

```c
struct sockaddr_in6 { 
    sa_family_t sin6_family;   //地址类型，取值为AF_INET6
    in_port_t sin6_port;  	   //16位端口号
    uint32_t sin6_flowinfo;    //IPv6流信息
    struct in6_addr sin6_addr; //具体的IPv6地址
    uint32_t sin6_scope_id;    //接口范围ID
};
```



```c
int connect(int sock, struct sockaddr *serv_addr, socklen_t addrlen); 
```

用于连接连接，它的参数与bind()基本相同。



```c
listen(int sock, int backlog);
```

在绑定IP地址与端口后，就需要让套接字进入被动监听状态。sock是需要进入监听状态的套接字，backlog为请求队列的最大长度。

- 所谓被动监听

  指当没有客户端请求时，套接字处于"睡眠"状态，只有当收到客户端请求时，套接字才会被"唤醒"来响应请求。

- 请求队列

  当套接字正在处理客户端请求时，如果有新的请求进来，套接字是无法处理的，只能把它放到缓冲区，待当前请求处理完毕后，再从缓冲区中读取出来处理。

  如果不断有新的请求进来，它们会按照先后顺序放到缓冲区中，直到缓冲区满，该缓冲区就被成为请求队列。

  如果缓冲区满了，对于Linux，客户端会收到ECONNREFUSED 错误。



```c
accpet(int sock, struct sockaddr *addr, socklen_t *addrlen);
```

当套接字处于被动监听状态时，可以通过accept()来接收客户端请求。

accept()函数会 从请求队列中去除当前要处理的请求，它会返回一个新的socket来和客户端通信，addr参数保存了客户端的IP地址和端口号，在后面的通信中会使用这个新生成的套接字，而不是原来服务器端的套接字。

值得注意的是，listen()只是让套接字进入监听状态，并没有真正接受客户端请求，listen()后面的代码会继续执行，直到遇到accept()。

accept()会阻塞程序执行，直到有新的请求进来。







### 数据的接收与发送

由于套接字也是一种文件，因此可以使用I/O相关的函数进行操作。发送数据可以使用wirte()函数，读取数据可以使用read()函数。



### socket缓冲区

每个socket被创建后，都会分配俩个缓冲区，输入缓冲区和输出缓冲区，这俩个缓冲区是自动生成的。

- 输出缓冲区

  write()不会立即向网络中传输数据，而是把数据写入到输出缓冲区中，再由TCP协议将数据从缓冲区发送到目标机器。一旦数据写入缓冲区，函数就返回成功。

  TCP协议是独立于write()函数的，数据有可能刚写入就发送到网络，也可能再缓冲区中不断积压，多次写入的数据被一次性发送到网络，这些不是由程序员控制的。

  如果一个套接字被关闭，只要在输出缓冲区中有遗留数据都会进行发送。

- 输入缓冲区

  read()函数也是如此，从输入缓冲区中读取数据，而不是直接从网络中读取。

  如果一个套接字被关闭，将会丢失输入缓冲区的数据

注：一端的输入对应另一端的输出，所以socket是双通道的数据传输方式。



example：打印缓冲区大小

```c
unsigned optVal;
int optLen = sizeof(int);
getsockopt(servSock, SOL_SOCKET, SO_SNDBUF, (char*)&optVal, &optLen);
printf("Buffer length: %d\n", optVal);
```



阻塞模式

TCP套接字默认情况下是阻塞模式。在这种模式下，调用write()发送数据时：

- 首先检查缓冲区，如果缓冲区的可用长度小于要发送的数据，那么write()会被阻塞。直到缓冲区中的数据被发送到目标机器，腾出足够的空间，才能唤醒write()函数继续写入数据。

  (这里其实是不是就是输出缓冲区还有数据未发送?)

- 如果TCP协议正在发送数据，那么输出缓冲区会被阻塞，直到输出所有数据write()才会被唤醒。

- 如果要写入的数据大于缓冲区，那么会分批写入。

- 直到所有数据被写入到输出缓冲区，write()函数才返回。

而调用read()读取数据时:

- 首先检查输入缓冲区，如果缓冲区中没有数据将会阻塞，直到有数据为止。
- 如果读取的数据小于缓冲区中的数据长度，那么就不能一次性将缓冲区的所有数据读出，剩余数据将不断积压，只有有read()函数再次读取。

这便是TCP的阻塞默认，所谓阻塞就是上一步动作没有完成，下一步动物将暂停，直到上一步动作完成后才能继续，以保持同步性。



















TCP/IP协议栈

TCP/IP协议栈分为4层，也就是说数据的收发分为4个层次化过程，分别是应用层、TCP层、IP层和链路层，各个层次之间是互通的。

分层的好处：分层是为了制定标准，层与层之间依赖标准制定的接口而不关注具体实现，例如路由器是按照IP层的标准来实现的，那么只要符合IP层的标准，哪个厂商的路由器都是通用的。

- 链路层

  计算机与计算机连同的物理层面，它指定了LAN、WAN、MAN等标准。

- IP层

  决定向目标传输数据时要选择的路径。

  IP本身是面向消息的、不可靠的协议；每次传输时都会选择路径，但是不一致；如果传输过程中发生路径错误会重新选择其他路径，但是发生数据错误或丢失则无法解决。

- TCP层

  以IP层提供的路径来传输数据，因此也被叫做传输层。

  数据传输的基本单位叫做包，IP层只关注1个数据包的传输过程。也就是说多个数据包传输时，数据包传输的顺序和数量是否正确IP层是无法确定的。

  基于IP层按照TCP协议来传输数据包可以保证数据的正确性，在每次传输数据包的时候，发送放都会等待接收方回传信息来判断数据包是否传输正确，包括数据包是否完整接收，数据包的数量和发送次序是否正确。

  如果不正确将会发送方将会重传以此来保证数据的正确性。

- 应用层

  选择数据传输路径、数据确认的过程都被隐藏到套接字内部。

  我们只需要利用套接字来进行编程，根据程序特点决定服务器端和客户端之间的数据传输规则（规定），这便是应用层协议。

  网络编程大部分都是设计并实现应用层该协议。





### 











### TCP原理

TCP套接字从创建到消失的过程分3步：

- 与对象套接字建立连接
- 与对方套接字进行数据交换
- 断开与对方套接字的连接





套接字的连接：

TCP在实际通信过程中也会经过3次对话过程，简称三次握手（Three-way handshaking）。套接字是以全双工（Full-duplex）方式工作的，也就是说它可以双向传递数据，因此收发数据前需要一些准备。

假设现在主机A向主机B发起连接，它的3次握手过程如下：

- [SYN] SEQ:1000,  ACK::-

  这是首次连接时使用的信息，又称SYN，表示收发数据前传输的同步消息。

  SEQ是1000，ACK为空。SEQ1000的意思是当前数据包的序号是1000，如果主机B接收无误的话，请通知我向您传递1001序号的数据包。

- [SYN+ACK] SEQ:2000, ACK:1001

  这是主机B对主机A首次传输的数据包的确认消息，ACK1001表示SEQ1000的数据包接收无误，请传递1001的数据包。

  SEQ2000是为主机B传输数据做准备的同步消息的捆绑发送，它表示现在传递的数据包序号是2000，如果接收无误请向我传递2001的数据包。

- [ACK] SEQ:1001, ACK:2001

  ACK2001表示收到2000序号的数据包，现在传递SEQ为2001序号的数据包，在这一步主机A和主机B建立了连接。





套接字的数据交换

假设有200字节的数据需要进行传输，传输过程中分2次（2个数据包），具体过程如下：

- SEQ 1200, 100byte data

  主机A向主机B传递100字节数据，数据包序列号是1200

- ACK 1301

  主机B为了确认收到数据，回传1301消息，这里1301的序号是为了确定接收到数据，并且确定下一次接收的数据包序号，计算公式是：ACK号 = SEQ号 + 传递的字节数 + 1

- SEQ 1301, 100byte data

- ACK 1402

如果在传递第二个数据包时发生数据包丢失，例如超时等原因，那么主机A在迟迟等不到主机B回传的ACK就会尝试数据包重传。

为了完成数据包重传，TCP套接字是会启动计时器以等待ACK的应答，若相对应的计时器发生超时则重传。



套接字连接的断开

套接字的断开经由4次过程：

- 主机A向主机B发送断开连接的信息

  FIN  SEQ  5000  ACK  -

- 主机B向主机A发送确认收到断开连接的信息

  ACK  SEQ  7500  ACK  5001

- 主机B向主机A发送可以断开连接的消息

  FIN   SEQ  7501  ACK  5001

- 主机A同样发出确认消息

  ACK  SEQ  5001  ACK  7502





### TCP的半关闭

假设俩台主机在双向通信中，主机A发送完最后的数据后，调用close()函数断开连接，这时候主机A是无法接收主机B回传的信息的，为了解决这种情况出现了TCP的半关闭。

- 套接字与流

  俩台主机的套接字建立连接后进入可交换数据的状态，这种状态可以叫"流形成的状态"。建立套接字后每台主机都会有单独的输入流和输出流，其中一台主机的输入流与另外一台主机的输出流对应，而输出流则与另外一台主机的输入流对应。

TCP的半关闭指的是只断开其中一个流。

断开输入流意味着套接字无法接受数据，即使数据在输入缓冲区中也会被清除；断开输出流意味着无法传输数据，但是输出缓冲区的数据仍然会被发送





### Socket选项

套接字是可以设置的，主要分为3层设置：

- IPPROTO_IP

  IP层，设置IP协议相关事项

- IPPROTO_TCP

  设置TCP协议相关事项

- SOL_SOCKET

  套接字相关的通用选项



SOL_SOCKET层相关选项：

- SO_TYPE

  套接字的类型

- SO_SNDBUF

  输出缓冲区大小配置，可用于更改输出缓冲区的大小。

- SO_REVBUF

  输入缓冲区大小配置，可读写



SO_REUSEADDR和Time-wait状态

- Time-wait状态

  俩个套接字连接时，先断开连接的套接字，即先发送FIN消息的套接字会处于Time-wait状态。

  例如主机A是服务端，主机B是客户端，当主机A发送FIN消息后，套接字会经过四次握手过程而非立即消除，所以这时候主机A的套接字仍然会占用端口，因此会发生bind()失败现象。

- Time-wati状态的作用

  套接字在断开连接是经过四次握手的，假设经过三次握手后主机A在向主机B发送最后一次ACK消息，这时候立即中断，那么该ACK消息就会丢失。

  这时候主机B会认为自己发送的FIN消息（SEQ 7502  ACK  5001）未抵达主机A，继而继续重传，相反主机A的套接字处于Time-wait状态则会向主机B重传最后一条ACK消息，从而结束这次套接字的连接。

SO_REUSEADDR选项可将Time-wait状态下的套接字端口号重新分配给新的套接字，该选项默认值为0，改为1即可将端口进行新的分配。

注：客户端套接字也有Time-wait状态的，只不过由于客户端套接字端口是随机绑定的。



TCP_NODELAY | Nagle算法

Nagle算法应用于TCP层，是为了防止数据包过多而发生网络过载。

TCP套接字默认使用Nagle算法交换数据，只有收到前一数据的ACK消息时，Nagle算法才会发送下一数据，因此能够最大限度的进行输出缓冲。

例如将字符串"world"传输到输出缓冲，头字符w之前没有其他数据，没有需接收的ACK，因此即可传输。之后开始等w字符的ACK消息，剩下的字符传输到输出缓冲区。接下来收到w字符的ACK消息后，将输入缓冲区的字符装入一个数据包发送，也就是说使用4个数据包来发送一个字符串。

- TCP_NODELAY选项表示是否启用Nagle算法，1启用，0禁用。



















相关API

```c
#include <sys/socket.h>

int getsockopt(int sock, int level, int optname, void *optval, socklen_t *optlen);
//sock：套接字文件描述符
//level：要查看的可选项的协议层
//optname：要查看的可选项名
//optval：保存查看结果的缓冲地址值
//optlen：向参数optval传递缓冲大小

int setsockopt(int sock, int level, int optname, const void *optval, socklen_t optlen);
//sock：用于更改的套接字
//level
//optval：保存要更改的选项的缓冲地址值
//optlen：向optval参数传递的可选项信息的字节数
```

环城中路67号













### API

```c


int shutdown(int sock, int howto);
//文件头：#include <sys/socket.h>
//sock：需要断开的套接字
//howto：断开类型，SHUT_RD是断开输入流，SHUT_WR断开输出流，SHUT_RDWR：同时断开I/O流



struct hostent * gethostbyname(const char *hostname);
//根据域名获取IP地址
//文件头：#include <netdb.h>
//hostent 结构 
struct hostent
{
    char * h_name;
    //官方网域名
    
    char ** h_aliases;
    //其他域名
    
    int h_addrtype;
    //网络协议的类型
    
    int h_length;
    //ip字节长度，
    
    char ** h_addr_list;
    //IP地址列表，有可能把多个IP地址映射到同一个域名，因此是IP地址列表
}

struct hostent * gethostbyaddr(const char *addr, socklen_t len, int family);
//根据IP地址获取域名
//文件头：netdb.h
//addr：含有IP地址信息的in_addr结构体指针
//len：向第一个参数传递的地址信息的字节数，IPv4为4，IPv6为16
//family：地址族信息，AF_INET，AF_INET6
```







