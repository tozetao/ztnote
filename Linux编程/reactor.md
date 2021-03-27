

acceptor
    针对listen socket的封装



event_loop

event_dispatcher
    该结构体是event_loop的一个属性。
    在代码编译阶段会根据系统平台来决定要选择哪种类型的event_dispatcher。

event_dispatcher_data
    由event_dispatcher初始化的结构体。



channel
    通道的抽象。
    fd：文件描述符，可以是socket的文件描述符，或者通道文件描述符
    events：事件类型，它是一个mask
    data：结构体类型，可能是event_loop、tcp_server或者tcp_connection


channel_element
    这是一个链表结构体，这个链表主要来存储channel元素。
    next：指向下一个channel_element
    type：channel的类型
    channel：存储真实的channel



event_loop_add_channel_event
--------------------------------------
1. 将channel加入到channel_element链表中
2. 遍历链表处理所有元素，将新加入的channel在map中建立关系，同时加入到dispatcher中。
3. 清空链表

poll_dispatcher的加入很简单，只是将fd加入到数组中，再设定对应的监听条件。


channel_event_activate
--------------------------------------
1. 根据fd从map中取出channel
2. 根据event类型，执行channel的回调函数


reactor涉及的系统知识有：
- 锁与条件变量
- 进程之间的socket通信
