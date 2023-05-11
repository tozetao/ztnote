#### poll

```c
int poll(struct pollfd *fdarray, unsigned long nfds, int timeout);
```

poll与select类似，允许进程指定内核要监视的描述符，并在IO事件就绪的情况下通知进程。

- fdarray

  该参数是一个指向pollfd结构体数组的第一个元素的指针。该数组每个元素都是一个pollfd结构体，用于指定监视文件描述符（fd）条件。

- nfds

  描述fdarray数组的大小，就是向poll申请的事件检测的个数。

- timeout

  秒数了poll的行为。

  如果是一个<0的数，表示在有事件发生之前永远等待；如果是0，表示不阻塞进程，立刻返回检测结果；如果是一个>0的数，表示poll调用方会在等待指定的毫秒数返回结果。

返回值：如果有就绪的描述符则返回就绪的数目，若超时返回0，若出错返回-1.

poll与select的区别：select的待检测描述符个数受限于fd_set的实现，而poll的待检测描述符个数并没有做限制，由pollfd结构的数组大小来做控制。



fdarray参数pollfd数组，pollfd结构体如下：

```c
struct pollfd {
    int fd;	//要监视的描述符
    short events;	//描述符要检测的IO事件
    short revents;	//描述符上触发的IO事件
};
```

pollfd结构体有3个成员，要监视的事件由events成员指定，它可以表示多个事件，由二进制掩码位操作来完成。比如：

```c
#define    POLLIN    0x0001		// any readable data available
#define    POLLPRI   0x0002
#define    POLLOUT   0x0004		// file descriptor is writeable
```

与select不同的是，pollfd的revents成员会返回描述符已触发的事件，这样就不需要每次检测完毕后重置待检测的描述符集合和感兴趣的事件了。





#### 事件类型

event事件类型可以分为俩大类。

第一类是可读事件，有以下几种：

```c
#define    POLLIN     0x0001
#define    POLLPRI    0x0002	// OOB/Urgent reabable data
#define    POLLRDNORM 0x0040	// non-OOB/URG data available
#define    POLLRDBAND 0x0080	// OOB/Urgent readable data
```

一般关注POLLIN即可。



第二类是可写事件，有以下几种：

```c
#define POLLOUT    0x0004	// file descriptor is writeable
#define POLLWRNORM POLLOUT	// no write type differentiation
#define POLLWRBAND 0x0100	// OOB/Urgent data can be written
```

一般在程序中统一使用POLLOUT。



还有一类事件没办法通过poll向系统内核提交检测请求，只能通过revents来检测，这种事件是各种错误事件。

```c
#define POLLERR    0x0008	// 一些错误发送
#define POLLHUP    0x0010	// 描述符挂起
#define POLLNVAL   0x0020	// 请求的事件无效
```

