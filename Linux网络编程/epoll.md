### epoll_create

```c
int epoll_create(int size);
int epoll_create1(int flags);
```

创建一个epoll实例，从Linux2.6.8开始，参数size被自动忽略，但是该值仍需一个大于0的整数。

这个实例用于调用epoll_ctl和epoll_wait，如果该实例不在需要使用，比如服务器关闭，需要调用close()方法来释放epoll实例，这样系统内核可以回收epoll实例分配使用的内核资源。

关于参数size，在开始时是用来告知内核要监听的文件描述符大小，以便初始化系统内核数据结构。在新的实现中已经能够动态分配需要的内核数据结构了，因此size设置为大于0的数即可。



### epoll_ctl

```c
int epoll_ctl(int opfd, int op, int fd, struct epoll_event *event);
```

该函数向epoll实例增加或删除文件描述符的事件。

#### 参数说明

epfd参数表示epoll_create函数创建的实例描述符，可以理解为epoll句柄。



op参数表示新增还是删除一个监控事件，它有三个选项：

- EPOLL_CTL_ADD：用epoll实例增加文件描述符对应的事件。
- EPOLL_CTL_DEL：用epoll实例删除文件描述符对应的事件。
- EPOLL_CTL_MOD：修改文件描述符对应的事件



fd是要注册事件的文件描述符。



event参数表示要注册的事件类型，并且可以在这个结构体里设置用户需要的数据，最常见的是设置epoll_data联合体的fd成员，表示触发事件的描述符。

具体结构如下：

```c
struct epoll_event {
    uint32_t events;		// epoll events
    epoll_data_t data;		// 用户数据变量
};

typedef union epoll_data {
    void *ptr;
    int fd;
    uint32_t u32;
    uint64_t u64;
} epoll_data_t;
```



#### 事件类型

epoll与poll一样适应了基于mask的事件类型，具体有：

- EPOLLIN：表示对应的文件描述符可以读。
- EPOLLOUT：表示对应的文件描述符可以写。
- EPOLLRDHUP：表示套接字的一端已经关闭，或者半关闭。
- EPOLLHUP：表示对应的文件描述符挂起
- EPOLLET：设置为edge-triggered（边缘触发），默认为level-triggered（水平触发）。

#### 返回值

若成功返回0，失败返回-1.







### epoll_wait

```c
int epoll_wait(int epfd, struct epoll_event *events, int maxevents, int timeout);
```

在一段超时时间内等待一组文件描述符上的事件。

如果监测某个事件发生，就将所有就绪的事件从内核事件表（由epfd参数指定）中复制到它的第二个参数events指向的数组中。

events数组只存储epoll_wait检测到的就绪事件，而不像select和poll的数组参数，既用于传入用户注册的事件，又用于输出内核检测到的就绪事件。这极大的提供了查找就绪文件描述符的效率。



maxevents是一个大于0的整数，表示epoll_wait可以返回的最大事件值。

timeout是阻塞调用的超时值，-1表示不超时，0则立即返回，即便没有任何I/O事件发生。



返回值：

成功时返回就绪的文件描述符个数，失败返回-1并设置errno。





### LT模式

level trigger是条件触发模式，这是epol认的默认模式。

对于EPOLLIN事件，LT模式下认为只要socket输入缓冲区有可读的数据，epoll_wait将会触发EPOLLIN事件；

对于EPOLLOUT，LT模式下认为只有缓冲区有可写入的空间则会触发EPOLLOUT事件。





### ET模式

edge-triggered模式是边缘触发。



> 对于EPOLLIN事件

只有对端有数据写入时才会触发。在触发事件后如果输入缓冲区的数据未被读取完毕，即使输入缓冲区有数据也不会触发事件，只有在下次对端写入新的数据时才会再次触发。

在et模式读取数据时都会一次性的把缓冲区的数据读取完毕，一般会使用循环读取socket缓冲区，直到没有数据可读。如果socket是阻塞模式会导致最后一次read阻塞，因此都会将socket设置为非阻塞。



> 对于EPOLLOUT事件

只有在从不可写变为可写的情况下才会触发一次，所以必须将文件描述符缓冲区一直写满，让errno返回EAGAIN为止，或者发送完所有数据为止。



> EPOLLONESHOT事件

对于注册EPOLLONESHOT事件的文件描述符，内核最多触发其上注册的一个可读、可写或者异常事件，且只触发一次。除非使用epoll_ctl函数重置该文件描述符上注册的EPOLLONESHOT事件。











注意：在telnet按回车键发送数据，最终会多出俩个字符，一个是回车字符，另外一个是换行字符。







相关参考：

https://www.zhihu.com/question/22576054/answer/89395376

https://www.zhihu.com/question/20502870/answer/89738959