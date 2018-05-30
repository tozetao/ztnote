### UDP

UDP是一种数据交换协议，它与TCP的不同地方在于：

- 面向消息的数据交换协议
- 有数据边界
- 强调传输速度而非可靠性，数据可能发生丢失

为了提供可靠的数据传输服务，TCP在不可靠的IP层进行流控制，而UDP则没有这种机制，流控制是俩个协议最重要的区别。



TCP比UDI慢的原因：

- 传输数据前后进行的连接设置以及清除过程
- 传输数据过程中为保证数据正确的流控制

如果传输数据量小但需要频繁的连接时，UDP比TCP高效。





### 工作原理

在传输数据时，IP的作用是选择正确的路径来发送UDP数据包，但是把UDP包最终交给接收方主机的某个UDP套接字的过程则是由UDP完成的。

UDP最主要的作用是根据端口号将传到主机的数据包交给最终的UDP套接字。





### UDP特性

- UDP只有创建套接字的过程和数据交换过程，它是没有连接的。

  TCP的套接字是一对一关系，若要接待10个连接请求，除了请求等待队列的套接字外，还需要10个服务端套接字，而UDP服务端和客户端都是只需要一个套接字就可以收发数据，只需要一个UDP套接字就可以向任意主机传输数据。

- 存在数据边界

  由于UDP存在数据边界，因此传输中调用I/O函数的次数是非常重要的，输入函数的调用和输出函数的调用是一致的，这样才能保证数据的完整接受，而在TCP数据传输过程中I/O函数的调用次数是没有意义的。

  可以试试调用三次输入函数，必须调用三次输出函数才能完整读取数据

- IP地址与端口的分配

  调用sendto()函数时，如果发现套接字没有分配IP地址和端口号，函数将会自动进行分配，IP地址等于本机地址，端口随机分配。

  当然也可以使用bind()函数手动绑定IP和端口。

  ​

从某种意义上来说无法区分服务端和客户端，只是根据提供的服务来区分UDP套接字的。UDP传输的数据包又称数据报，由于存在边界一个数据包就是完整的数据。



example：UDP客户端和服务端实现

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>

#define BUF_SIZE 30
void error_handling(char *message);

int main(int argc char*argv[])
{
    int serv_sock;
    char message[BUF_SIZE];
    int str_len;
    socklen_t clnt_adrs_sz;
    struct sockaddr_in serv_addr, clnt_addr;
    
    if(argc != 2)
    {
        printf("usage: %s \n", argv[0]);
        exit(1);
    }
    
    serv_sock = socket(PF_INET, SOCK_STREAM, 0);
    
}

void error_handling(char *message)
{
    fput();
    fputc();
    exit(1);
}
```



- UDP套接字和为连接UDP套接字

  这里的连接指的是UDP套接字注册IP地址和端口的过程，并不是说真正的建立连接。

  sendto()传输数据大体分为3个阶段，1是向UDP套接字注册IP和端口号，2是传输数据，3是删除UDP套接字中注册的目标地址信息。如果UDP套接字总是向同一个地址发送数据，总是向套接字注册、销毁同样的地址是不合理的。

  因此可以手动的注册UDP套接字的IP和端口号，通过connect()函数进行注册，同时配合write()、read()函数进行读写即可。









### API

```c
#include <sys/socket.h>

ssize_t sendto(int sock, void *buff, size_t mbytes, int flags, struct sockaddr *to, socklen_t addrlen){}
//sock: 用于传输数据的UDP套接字
//buff: 待传输数据的缓冲地址值
//nbytes: 待传输数据长度，字节单位
//flags: 可选参数
//to:   目标地址信息的sockaddr结构体
//addrlen: to参数的字节长度

ssize_t recvfrom(int sock, void *buff, size_t nbytes, int flags, struct sockaddr *from, socklen_t addrlen){}
//sock: 用于接受数据的套接字
//buff: 保存接受数据的缓冲地址
//nbytes：缓冲地址大小，即可接受的最大字节数
//from: 存有发送端地址信息的sockaddr结构体
//addrlen: from结构体的大小
```
