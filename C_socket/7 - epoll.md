### epoll

eopll是linux2.7以上版本操作系统提供的I/O复用函数，它主要用于解决select()性能不足的问题。

select函数的缺点：

- 需要遍历所有文件描述符，找到监视对象fd_set变量中变化的文件描述符
- 每次调用select函数都需要向函数传递新的监视对象fd_set变量

每次传递监视对象fd_set变量是影响性能最大的原因，它表示每次都向操作系统传递监视对象信息，因为文件描述符是属于操作系统管理的，select()函数需要操作系统帮忙才能完成。

如果仅向操作系统传递1次监视对象，同时只传递监视范围的变化或者内容发生的变化，那么性能会大大提高，Linux提供了epoll函数，windows则提供IOCP支持，不同的操作系统是有区别的。



### API

```c
int epoll_create(int size);
```

创建保存epoll文件描述符的空间，成功返回文件描述符，失败返回-1。

调用eopll_create()函数创建的文件描述符保存空间称为epoll例程，size参数是建议系统设置epoll例程的保存空间大小。







```c
epoll_ctl
```

向空间注册并注销文件描述符



```c
epoll_wait
```

等待文件描述符发生变化



epoll方式由操作系统负责保存监视对象文件描述符，向操作系统请求创建保存文件描述符的空间使用epoll_create()函数，注册和删除文件描述符有epoll_ctl()函数完成，等待描述符发生变化则使用epoll_wait()函数。



```c
struct epoll_event
{
    __uint32_t events;
    epoll_data_t data;
}

typeof union epoll_data
{
	void * ptr;
	int fd;
	__uint32_t u32;
	__uint64_t u64;
}epoll_data_t;
```

该结构体存储发生变化的文件描述符

