

reactor模型的核心有俩点：

第一是reactor线程，这是一个无限循环的事件分发线程。这个事件分发线程的背后是poll、epoll等I/O分发技术的使用。

第二是将所有I/O操作抽象成事件，每个事件必须有对应的回调函数来处理。

acceptor上有连接建立成功、已连接套接字上发送缓冲区空出可以写、通信管道pipe上有数据可以读，这些都是事件，通过事件分发这些事件可以一一被检测并调用对应的回调函数来加以处理。





### acceptor

acceptor是监听对象，对listen socket的封装。

```c
struct acceptor *acceptor_init(int port);
```

该函数是对listen socket的封装，创建好的listen socket可以通过acceptor结构体获得。





### event_loop

event_loop是reactor对象，event_loop和线程相关联，每个event_loop在线程里执行的是一个无限循环，以便完成事件的分发。



```c
int event_loop_do_channel_event(struct event_loop *eventLoop, int fd, struct channel *chanel1, int type);
```

该函数是根据type来决定如何处理channel，type可以是添加、更新和删除。channel最后会被加入到待处理列表中。

这里的type的作用暂时未知。



```c
void event_loop_channel_buffer_nolock(struct event_loop *eventLoop, int fd, struct channel *channel1, int type);
```

该函数会将channel封装为一个channel_element结构，并加入到channel_element链表中。





```c
struct channel_element {
    int type;	// 1: add, 2: delete
    struct channel *channel;
    struct channel_element *next;
};
```

channel_element是链表结构。

event_loop结构体通过head、tail俩个指针指向了链表的头元素和尾元素以此来实现链表。





```c
void event_loop_wakeup(struct evetn_loop *eventLoop) {
    char one = 'a';
    size_t n = write(eventLoop->socketPair[0], &one, sizeof one);
    if (n != sizeof one) {
        LOG("wakeup event loop thread failed");
    }
}

```

向socket写入一个字符，暂时不知道作用。







```c

int event_loop_handle_pending_channel(struct event_loop *eventLoop)
{
    //get the lock
    pthread_mutex_lock(&eventLoop->mutex);
    eventLoop->is_handle_pending = 1;

    // 遍历pending列表中的元素，执行处理
    struct channel_element *channelElement = eventLoop->pending_head;
    while (channelElement != NULL) {
        //save into event_map
        struct channel *channel = channelElement->channel;
        int fd = channel->fd;
        if (channelElement->type == 1) {
            event_loop_handle_pending_add(eventLoop, fd, channel);
        } else if (channelElement->type == 2) {
            event_loop_handle_pending_remove(eventLoop, fd, channel);
        } else if (channelElement->type == 3) {
            event_loop_handle_pending_update(eventLoop, fd, channel);
        }
        channelElement = channelElement->next;
    }

    eventLoop->pending_head = eventLoop->pending_tail = NULL;
    eventLoop->is_handle_pending = 0;

    //release the lock
    pthread_mutex_unlock(&eventLoop->mutex);

    return 0;
}
```

开始处理pending列表中的channel。

















### event_dispatcher

事件分发器结构：

```c
struct event_dispatcher {
    /**  对应实现 */
    const char *name;

    /**  初始化函数 */
    void *(*init)(struct event_loop * eventLoop);

    /** 通知dispatcher新增一个channel事件*/
    int (*add)(struct event_loop * eventLoop, struct channel * channel);

    /** 通知dispatcher删除一个channel事件*/
    int (*del)(struct event_loop * eventLoop, struct channel * channel);

    /** 通知dispatcher更新channel对应的事件*/
    int (*update)(struct event_loop * eventLoop, struct channel * channel);

    /** 实现事件分发，然后调用event_loop的event_activate方法执行callback*/
    int (*dispatch)(struct event_loop * eventLoop, struct timeval *);

    /** 清除数据 */
    void (*clear)(struct event_loop * eventLoop);
};
```





```c
const struct event_dispatcher_data {
    int event_count;
    int nfds;
    int realloc_copy;
    struct pollfd *event_set;
    struct pollfd *event_set_copy;
}
```

保存分发器对应的数据，即poll或epoll在使用过程中需要的数据。











event_dispatcher_data



### channelMap



### channel

channel通道的意思，它是对文件描述符的封装。指定了一个文件描述符触发的事件要由哪个对象来处理，调用哪些回调方法进行处理。

```c
struct channel {
    int fd;
    int events;		// 表示event类型
    
    void *data;		// 可以是回调数据、tcp_server、event_loop、tcp_connection
    
    callback...		// 回调函数略
}
```





进程之间的通信

socketpair()





### TCPServer

创建的时候可以指定线程，如果线程数为0就只有一个线程，既负责acceptor的连接处理，也负责已连接socket的I/O处理。



server中的线程池是怎么实现的？



server start做了什么？



event_loop_run()做了什么？












reactor涉及的系统知识有：
- 锁与条件变量
- 进程之间的socket通信
