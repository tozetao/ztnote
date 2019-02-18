### Socket

在linux中，一切都是文件，硬件、socket都可以认为是文件。所有在Linux系统中创建的文件都有一个int型的编号，称为文件描述符，使用文件时只需要知道编号即可。

socket也是一种文件，所以在网络数据传输时可以使用与文件I/O相关的函数，可以认为俩台计算机之间的通信，实际上是俩个socket文件的相互读写。





### 套接字地址结构

每个协议组都各自定义了自己的套接字地址结构

```c
#include <netinet/in.h>

struct sockaddr_in {
    uint8_t sin_len;		//结构长度
    
    sa_family_t sin_family;	//地址族，也就是地址类型
    in_port_t sin_port;		//16位的端口
    struct in_addr sin_addr;			//32位IP地址
    
    char sin_zero[8];		//未使用
}
```

sockaddr_in结构体长度为16个字节，sockaddr_in的各个参数意义如下：

- sin_family

  地址族，与socket()函数的参数一致，占用俩个字节。

- sin_port

  端口号，in_port_t一般是uint16_t类型，无符号的16位整数，端口号总是以网络字节序存储。

- in_addr

  32位的IPv4地址，一般是一个32位的无符号整数。

- sin_zero

  一般填充为0，没什么用。

```c
#include <netinet/in.h>

struct in_addr {
    in_addr_t s_addr;	// 32位的IPv4地址
}
```

in_addr表示IPv4地址，它是只包含一个成员的结构体。

in_addr等价于unsigned long，长度为4个字节，也是以网络字节序存储的。



```c
struct sockaddr_in6 { 
    uint8_t sin6_len;			//该结构体的长度，IPv6结构体的长度时28个字节
    sa_family_t sin6_family;   //地址类型，取值为AF_INET6
    in_port_t sin6_port;  	   //16位端口号
    
    struct in6_addr sin6_addr; //具体的IPv6地址
    
    uint32_t sin6_scope_id;    //接口范围ID
    uint32_t sin6_flowinfo;    //IPv6流信息，未定义
};

struct in6_addr {
    unit8-t s6_addr[16];		//128位的IPv6地址，占用16个字节，以网络字节序存储
}
```

sockaddr_in6是一个28个字节长度的结构体。



### 通用套接字地址结构

```c
#include <sys/socket.h>

struct sockaddr {
    uint8_t sa_len;
    sa_family_t sa_family;
    char sa_data[14];	//协议具体地址
}
```

套接字地址结构是以指针来传递的，但是套接字函数都需要处理任意协议族的套接字地址结构，因此定义了一个通用的套接字地址结构，以该通用套接字地址结构的指针类型作为参数传递。

因此在使用套接字函数时，需要将特定协议的套接字地址接结构转换成通过的套接字地址结构，例如：

```c
bind(sock, (struct sockaddr*)&serv_addr, sizeof(serv_addr));
```



### 字节排序

数据存储在内存中是二进制字节的，从右往左数是从低序走向高序的一个过程。而内存地址也是从小往大一个递增的过程。

- 小端序：低序字节存储在起始地址是小端序

- 大端序：高序字节存储在起始地址是大端序

网络协议的数据传输必须是大端序（网络字节序），数据存储时是按照主机字节序存储的，等传输数据时会自动转换成网络字节序，然后由于历史原因，套接字地址接口中的某些字段必须是以网络字节序进行存储。

```c
#include <netinet/in.h>

uint16_t htons(uint16_t host_16bit_value);
uint16_t htonl(uint32_t host_32bit_value);
//主机字节序转网络字节序


uint16_t ntohs(uint16_t net_16bit_value);
uint32_t ntohl(uint32_t net_32bit_value);
//网络字节序转主机字节序

#include <arpa/inet.h>

int inet_pton(int family, const char *strptr, void *addrptr);
//将IP地址转为网络字节序，成功返回1，失败返回-1，如果不是有效的表达式则返回0.

const char *inet_ntop(int family, const void *strptr, char *strptr, size_t len);
//将网络字节序转换成IP地址
```

example:

```c
//将IP地址转换为网络字节序
char ip[] = "192.12.34.90";
struct sockaddr_in serv_addr;
memset(&serv_addr, '0', sizeof(serv_addr));

inet_pton(AF_INET, ip, &serv_addr.sin_addr);
printf("%d\n", (int)serv_addr.sin_addr.s_addr);
```





### Functions

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
int connect(int sock, struct sockaddr *serv_addr, socklen_t addrlen); 
```

客户端使用connect来与TCP服务器建立连接，它的参数与bind()基本相同。



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

accept()函数会 从请求队列中返回当前要处理的请求，它会返回一个新的socket来和客户端通信，addr参数保存了客户端的IP地址和端口号，在后面的通信中会使用这个新生成的套接字，而不是原来服务器端的套接字。

值得注意的是，listen()只是让套接字进入监听状态，并没有真正接受客户端请求，listen()后面的代码会继续执行，直到遇到accept()。

accept()会阻塞程序执行，直到有新的请求进来。





```c
#include <unistd.h>

int close(sockfd);
```

close将一个TCP套接字标记为已关闭，然后立即返回调用进程。文件描述符被标记关闭后就不能在调用进程中使用了。

注：close()函数并不会引发socket关闭的4次握手过程，只有当socket的文件描述符引用计数为0时，内核才会真正的去关闭该socket对应的资源。





### socket缓冲区

每个socket被创建后，都会分配俩个缓冲区，输入缓冲区和输出缓冲区，这俩个缓冲区是自动生成的。

- 输出缓冲区

  write()不会立即向网络中传输数据，而是把数据写入到输出缓冲区中，再由TCP协议将数据从缓冲区发送到目标机器。一旦数据写入缓冲区，函数就返回成功。

  TCP协议是独立于write()函数的，数据有可能刚写入就发送到网络，也可能再缓冲区中不断积压，多次写入的数据被一次性发送到网络，这些不是由程序员控制的。

  如果一个套接字被关闭，只要在输出缓冲区中有遗留数据都会进行发送。

- 输入缓冲区

  read()函数也是如此，从输入缓冲区中读取数据，而不是直接从网络中读取。

  如果一个套接字被关闭，将会丢失输入缓冲区的数据

注：一端的输出缓冲区对应另一端的输入缓冲区，所以socket是全双工通信的数据传输方式。



example：打印缓冲区大小

```c
unsigned optVal;
int optLen = sizeof(int);
getsockopt(servSock, SOL_SOCKET, SO_SNDBUF, (char*)&optVal, &optLen);
printf("Buffer length: %d\n", optVal);
```


















