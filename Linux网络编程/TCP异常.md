### TCP的可靠性

TCP的可靠性，对于内核是可靠的，但是对于应用程序来说，TCP不是可靠的。



从发送端来说，当调用write函数之后，数据流是存储在发送缓冲区的，如何发送、何时发送由网络协议栈决定。

当对应的数据发送给接收端，接收端回应ACK，存储在发送缓冲区的这部分数据就可以删除了。从协议设计上来看是可靠的，但是对于应用程序来说是不可靠的。

这里的不可靠指的是发送端无法获取ACK对应的数据流的情况，也就是说，发送端没有办法判断接收端应用程序是否已经接收到发送的数据流。如果需要知道这部分信息，就必须由应用程自己添加处理逻辑，比如显式的报文确认机制。



从接收端来说，也没有办法保证ACK过的数据可以被应用程序处理。因为数据需要接收端程序从接收缓冲区中拷贝。可能出现的情况是，已经ACK后的数据保存在接收缓冲区中，接收端处理程序就崩溃了，那么这部分数据就没有办法被应用程序处理了。



TCP协议实现没有提供给上层应用程序过多的异常处理细节，我们只能通过读操作read、写操作write这俩种行为来感知异常情况。





### TCP故障

TCP故障可以按照发生的情况分成俩类，一类是未收到FIN包的故障，一类是收到FIN包的故障。



#### 未收到FIN包的故障

> 网络连接中断

对于网络中断，TCP是不能直接感受到异常的。

在网络中断情况下程序调用write函数发送数据后，TCP协议栈会不断尝试发送缓冲区的数据，大约重传12次，合约9分钟左右，协议栈才会标识该连接异常。

这时如果调用阻塞的read会返回一条TIMEOUT的错误信息；如果调用write，写操作会立即失败并返回SIGPIPE信号。



> 系统崩溃造成对端无FIN包

当系统崩溃，比如系统断电，来不及发送FIN包。这种情况与网络中断很相似，在没有ICMP报文的情况下，TCP程序也只能通过write和read调用来得到网络连接异常信息。





#### 收到FIN包的故障

在一个双方正常关闭的流程中，收到FIN包的一端将剩余数据发送给对面（通过一次或多次write），接着关闭socket以完成四次挥手，这里主要说明非正常关闭的情况。



> 向一个已经关闭的套接字发送（write）数据，接着再读取该套接字会怎么样？

根据TCP协议，连接是双向的，收到对方的FIN包只意味着对方不会再发送任何消息，因此收到FIN包的一端是可以继续向对端发送数据的。

那么向一个已经关闭的套接字发送数据，当数据到达对端时，系统内核发现这是一个指向关闭的套接字，会直接向发送方发送一个RST包，发送端执行read操作，内核就将RST错误信息通知给应用程序，因此会读取到一个RST错误。



注意：在ubuntu18系统中read仍然可以读取到EOF，而在抓包的时候是可以看到有RST数据包的。





> 另外一种情况是向一个已close的socket重复执行write会怎么样？

通过上面的例子可以知道第一次write是可以成功发送数据的，但是对端会返回一个RST包，接着再次write，会立即返回一个RST错误信息并产生一个SIGPIPE信号，如果不捕捉该信号会导致程序异常退出。

服务端程序：

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <strings.h>
#include <errno.h>
#include <unistd.h>

void error(char *str, int status)
{
    printf("%s, res: %d\n", str, status);
    exit(status);
}

int main(int argc, char const *argv[])
{
    // 创建一个server，监听9100端口的连接
    int port = 9100;
    int res = 0;
    int listenfd = socket(AF_INET, SOCK_STREAM, 0);

    

    struct sockaddr_in server_addr;
    bzero(&server_addr, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    server_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    // bind
    res = bind(listenfd, (struct sockaddr *)&server_addr, sizeof(server_addr));
    if (res < 0) {
        error("bind error", res);
    }
    // listen
    res = listen(listenfd, 128);
    if (res < 0) {
        error("listen error", res);
    }

    // 接受一个连接并进行处理
    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);
    int connfd = accept(listenfd, (struct sockaddr *)&client_addr, &client_len);
    if (connfd < 0) {
        error("accept error", connfd);
    }

    char buffer[1024];
    int cnt = 0;
    // 处理该连接
    while (1) {
        cnt = read(connfd, buffer, 1024);

        if (cnt < 0) {
            error("read error", cnt);
        } else if (cnt == 0) {
            error("socket close\n", cnt);
        }

        sleep(8);

        int write_cnt = send(connfd, buffer, cnt, 0);
        if (write_cnt < 0) {
            error("write error", write_cnt);
        }
        printf("send bytes %u\n", write_cnt);
    }

    return 0;
}

```

客户端程序：

```c
#include <strings.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

void error(char *s, int status)
{
    printf("%s\n", s);
    exit(status);
}

void handle_signal(int signal)
{
    printf("receive signal %d\n", signal);
}


int main(int argc, char const *argv[])
{
    signal(SIGPIPE, handle_signal);

    int port = 9100;
    char *addr = "127.0.0.1";

    struct sockaddr_in server_addr;
    bzero(&server_addr, sizeof(server_addr));
    server_addr.sin_port = htons(port);
    server_addr.sin_family = AF_INET;
    inet_pton(AF_INET, addr, &server_addr.sin_addr);

    int socket_fd = socket(AF_INET, SOCK_STREAM, 0);

    int res = connect(socket_fd, (struct sockaddr *)&server_addr, sizeof(server_addr));
    if (res < 0) {
        error("connect error\n", res);
    }

    // 向服务端发送数据，开始测试。
    char buffer[1024];

    while (fgets(buffer, 1024, stdin)) {
        // 第一次send会发送成功。如果服务器关闭连接了，内核会返回RST包
        int len = strlen(buffer);
        res = send(socket_fd, buffer, len, 0);
        if (res < 0) {
            error("first send error", res);
        }

        // 在返回rst包后，第二次send会失败，并触发一个PIPE信号。
        len = strlen(buffer);
        res = send(socket_fd, buffer, len, 0);
        if (res < 0) {
            error("second send error", res);
        }
    }
    return 0;
}
```

依次开启服务端和客户端，然后关闭服务器，这时候客户端发送数据，就可以看到要测试的效果。