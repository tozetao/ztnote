```c
#include <sys/epoll.h>

int epoll_wait(int epfd, struct epoll_event *events, int maxevents, int timeout);
```

在一段超时时间内等待一组文件描述符上的事件，成功时返回就绪的文件描述符个数，失败返回-1并设置errno。

epoll_wait函数如果监测到事件，就将所有就绪的事件从内核事件表（由epfd参数指定）中复制到它的第二个参数events指向的数组中。

events数组只用于epoll_wait检测到的就绪事件，而不像select和poll的数组参数，既用于传入用户注册的事件，又用于输出内核检测到的就绪事件。这极大的提供了索引就绪文件描述符的效率。



```c
// epoll_event结构
struct epoll_event
{
    __uint32_t events;	
    epoll_data_t data;
}
```

epoll_event结构体表示文件描述符的事件类型。

events成员是事件类型，epoll支持的事件类型与poll基本相同。表示epoll事件类型的宏是在poll对应的宏前加上字符E，例如EPOLLIN。epoll有俩个额外的事件类型：EPOLLLT表示LT模式，EPOLLONESHOT表示ET模式。

data成员是一个epoll_data_t的联合体，用于存储数据。



```c
// 一个联合体
typedef union epoll_data
{
	void *ptr;
	int fd;
	uint32_t u32;
	uint64_t u64;
}epoll_data_t;
```

epoll_data_t是一个联合体，fd成员最常用，表示事件所属的文件描述符。ptr成员用于指定与fd相关的数据，由于联合体只能使用一个成员，如果要将文件描述符与相关数据关联起来，一般会放弃fd成员，而在ptr指向的数据中包含fd。



example：

```c
int ret = poll(fds, MAX_EVENT_NUMBER, -1);
// 遍历所有已注册文件描述符，并找到其中就绪的
for(int i = 0; i < MAX_EVENT_NUMBER; i++) {
    // 判断文件描述符是否就绪
    if (fds[i].revents & POLLIN) {
        int sockfd = fds[i].fd;
    }
}

int ret = epoll_wait(epollfd, events, MAX_EVENT_NUMBER, -1);
// 仅便利就绪的ret个文件描述符
for(int i = 0; i < ret; i++) {
    int sockfd = events[i].data.fd;
}
```



- EPOLLOUT

  数据可写时触发，包括普通数据和优先数据。

- EPOLLIN

  数据可读，包括普通数据和优先数据。



LT与ET对于可读和可写触发事件的解读是不一样的。



LT模式

Level Trigger，电平触发模式。是epoll默认的工作模式。

对于EPOLLIN事件，LT模式下认为只要socket输入缓冲区有可读的数据，epoll_wait将会触发EPOLLIN事件；对于EPOLLOUT，LT模式下认为只有缓冲区有可写入的空间则会触发EPOLLOUT事件。



ET模式

- 对于EPOLLIN事件

  在ET模式下，EPOLLIN事件只有对端有数据写入时才会触发，在触发事件后如果输入缓冲区的数据未被读取完毕，即使输入缓冲区有数据也不会触发事件，只有在下次对端写入新的数据时才会再次触发。

  因此读取数据时会一次性的把缓冲区的数据读取完毕，一般会使用循环读取socket缓冲区直到没有数据，如果socket是阻塞的会导致最后一次read阻塞，因此在ET模式下会将socket设置未非阻塞。

- 对于EPOLLOUT事件

  只有在从不可写变为可写的情况下才会触发一次。因此必须将文件描述符缓冲区一直写满，让errno返回EAGAIN为止，或者发送完所有数据为止。

  满的缓冲区刚空出空间的时候触发事件?



EPOLLONESHOT事件

对于注册EPOLLONESHOT事件的文件描述符，内核最多触发其上注册的一个可读、可写或者异常事件，且只触发一次。除非使用epoll_ctl函数重置该文件描述符上注册的EPOLLONESHOT事件。



