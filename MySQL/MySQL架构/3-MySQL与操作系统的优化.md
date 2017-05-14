## MySQL适合的操作系统
- Windows
- FreeBSD
- Solaris
- Linux

## Centos系统参数优化
### 1. 内核相关参数
参数配置位于：/etc/sysctl.conf

对于一个tcp连接来说，服务器要与客户端需要进行3此握手来建立网络连接，当连接成功后能通过netstat命令看到由准备变成连接，下面是关于tcp连接的系统参数。

- net.core.somaxconn：端口队列大小，建议值2048或者更大(65535)
- net.core.netdev_max_backlog：65535
- net.ipv4.tcp_max_syn_backlog：65535


- net.ipv4.tcp_fin_timeout：10，控制tcp连接处理等待状态的时间，加快tcp连接的回收速度
- net.ipv4.tcp_tw_reuse：1
- net.ipv4.tcp_tw_recycle：1
这3个参数决定的tcp连接的回收速度


net.core.wmem_default=87380
net.core.wmem_max=16777216
net.core.rmem_default=87380
net.core.rmem_max=16777216
这4个参数决定tcp连接接受和发送缓冲区大小的默认值和最大值。



net.ipv4.tcp_keepalive_time=120
net.ipv4.tcp_keepalive_intvl=30
net.ipv4.tcp_keepalive_probes=3
减少失效连接占用系统的资源数量，加快资源回收效率

kernel.shmmax=4294967295
linux内核参数中最重要的参数之一，用于定义单个共享内存段的最大值。

注1：这个参数应该足够大，以便能在一个共享内存段下容纳下整个innodb缓冲池的大小，如果值比较小，那么会导致创建多个共享内存段，在实例启动的时候会导致轻微的系统下降。

注2：对于64位linux操作系统，最大值是物理内存只-1byte，建议值为大于物理内存值的一般，一般取值大于innodb缓冲池的大小即可。

kernel.shmmax=4294967295

vm.swappiness=0
告诉linux内核，除非虚拟内存完全满了，否则不要使用交换区

### 2. 增加资源限制
/etc/security/limit.conf
该文件是Linx PAM，也就是插入式认证模块的配置文件，这里面比较重要的是打开文件数的限制。

\* soft nofile 65535
\* hard nofile 65535
*表示对所有用户有效，soft是当前系统生效的设置，hard标明系统所能设置的最大值，nofile表示所限制的资源是打开文件的最大数目，65535就是限制的数量。

结论：保证能够打开足够多的文件句柄。
注意：这个文件的修改需要重启系统才能生效。

### 3. 磁盘调度策略
cat /sys/block/devname/queue/scheduler
noop anticipatory deadline [cfq]

回头再来做笔记，cfq对于桌面应用是足够的，但是对于MySQL不是很好，所以需要修改。






推荐书籍：Linux性能优化大师