

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



#### init

init函数会初始化event_loop对象，同时初始化好event dispatcher。



#### channel事件处理

```c
int event_loop_do_channel_event(struct event_loop *eventLoop, int fd, struct channel *chanel1, int type);
```

该函数用于处理channel事件。参数type指channel事件，分别为1添加、2删除、3更新。

处理流程为：

- 将channel对象加入event_loop对象的channel map链表中。
- 





```c
void event_loop_channel_buffer_nolock(struct event_loop *eventLoop, int fd, struct channel *channel1, int type);
```

该函数会将channel封装为一个channel_element结构，并加入到eventLoop的链表中。





```c
struct channel_element {
    int type;	// 1: add, 2: delete
    struct channel *channel;
    struct channel_element *next;
};
```

channel_element是链表结构。

event_loop结构体通过head、tail俩个指针指向了链表的头元素和尾元素以此来实现一个单向链表。





```c
void event_loop_wakeup(struct evetn_loop *eventLoop) {
    char one = 'a';
    ssize_t n = write(eventLoop->socketPair[0], &one, sizeof one);
    if (n != sizeof one) {
        LOG("wakeup event loop thread failed");
    }
}

```

该函数在非主线程中执行，向socket写入一个字符，暂时不知道作用。







```c

int event_loop_handle_pending_channel(struct event_loop *eventLoop)
{
    //get the lock
    pthread_mutex_lock(&eventLoop->mutex);
    eventLoop->is_handle_pending = 1;

    // 变量链表中所有channel元素，根据channel的事件类型执行对应处理。
    // 注：在整个处理过程中，is_handle_pending = 1
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

当前线程是主线程的时候开始执行，该函数用于处理event_loop中channel element链表的所有元素。不同类型的channel会调用不同的函数来进行处理。



add channel 

加入channel map中，加入dispatcher中。



remove channel

从channel map中删除，从dispatcher中删除。



update channel







```c
int channel_event_active(struct event_loop *eventLoop, int fd, int revents);
```

channel事件被触发时的处理。

- fd：文件描述符，一般是socket
- revents：触发的事件，它是一个掩码，值可能是EVETN_READ、EVENT_WRITE的组成。

函数会从channel map中取出channel并执行channel所设置的回调函数来处理socket。





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
const struct poll_dispatcher_data {
    int event_count;
    int nfds;
    int realloc_copy;
    struct pollfd *event_set;
    struct pollfd *event_set_copy;
}
```

保存分发器对应的数据，即poll或epoll在使用过程中需要的数据。





#### dispatch

poll dispatcher的实现。



```c
poll_dispatch(struct event_loop *eventLoop, struct channel *channel1);
```

函数实现逻辑：

- 执行poll系统函数，监听要处理的连接。
- 



首先使用poll函数监听有哪些文件描述符准备就绪，设置最多阻塞1秒时间。



```c
poll_add(struct event_loop *eventLoop, struct channel *channel1);
```

poll_add的实现很简单，就是将channel对应的socket加入到pollset集合中，由系统去监听它。





### channelMap

一个map，键是文件描述符，值是channel结构体。

```c
struct channel_map {
    void **entries;
    
    // map容量的大小
    int nentries;
}
```



```c
void map_init(struct channel_map *map)
{
    map->nentries = 0;
    map->entries = NULL;
}
```

channel map的初始化。



```c
int map_make_space(struct channel_map *map, int slot, int msize)
{
    if (map->nentries <= slot) {
        // map容量的大小
        int nentries = map->nentries ? map->nentries : 32;
        void **tmp;
        
        // 如果map的size小于slot，那么扩充map的size
        while (nentries <= slot)
            nentries <<= 1;

        // 扩充map的内存大小
        tmp = (void **) realloc(map->entries, nentries * msize);
        if (tmp == NULL)
            return (-1);

        // 将tmp[map->nentries]位置后面的n个字节填充为0.
        memset(&tmp[map->nentries], 0,
               (nentries - map->nentries) * msize);

        // 更新map容量大小，指针
        map->nentries = nentries;
        map->entries = tmp;
    }

    return (0);
}
```

该用户用于扩展map的空间大小。

- slot

  期望新建map的大小。map的大小并不是slot参数指定的值，是一个大于等于slot的值。

- msize

  map中元素的大小，单位字节。

函数执行成功返回0，失败返回-1.



```c
void map_clear(struct channel_map *map)
{
    if (map->entries != NULL) {
        for(i = 0; i < map->entries; ++i) {
            if (map->entries[i] != NULL) {
                free(map->entries[i]);
            }
        }
        free(map->entries);
        map->entries = NULL;
    }
    map->nentries = 0;
}
```









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









```c
int channel_write_event_enable(struct channel *channel) {
    struct event_loop *eventLoop = (struct event_loop *) channel->data;
    channel->events = channel->events | EVENT_WRITE;
    event_loop_update_channel_event(eventLoop, channel->fd, channel);
}
```

启用channel的write事件。











### TCPServer

创建的时候可以指定线程，如果线程数为0就只有一个线程，既负责acceptor的连接处理，也负责已连接socket的I/O处理。





### thread pool



```c
struct thread_pool {
    // 创建thread_pool的主线程
    struct event_loop *mainLoop;
    // 是否已启动
    int started;
    // 线程数量
    int thread_number;
    
    // 数组指针，指向创建的event_loop_thread数组
    struct event_loop_thread *eventLoopThreads;
    
    // 在数组中的位置，决定选择哪个event_loop_thread服务。
    int position;
};
```



```c
void thread_pool_start(struct thread_pool *threadPool)
{
    assertInSameThread(threadPool->mainLoop);
    // ...
    
    threadPool->eventLoopThreads = malloc(threadPool->thread_number * sizeof(struct event_loop_thread));
}
```

启动线程池，必须由主线程来启动。





```c
struct event_loop_thread {
    struct event_loop *eventLoop;
    
    pthread_t thread_tid;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    
    char *thread_name;
    
    // 已处理的连接数
    long thread_count;
};
```

子线程结构体。



```c
int event_loop_thread_init(struct event_loop_thread *eventLoopThread, int i)
{
    
}
```

初始化一个子线程。主要初始化锁、条件变量和线程名字。



```c
struct event_loop *event_loop_thread_start(struct event_loop_thread *eventLoopThread)
{
    
}
```










问题：

poll的使用

socketpair的使用

listen socket只需要处理读事件，连接socket的可读、可写事件如何处理？

线程池的实现。

wakeup有什么作用。