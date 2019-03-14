### http_stub_status_modules

该模块用于查看Nginx服务器的请求状态。该模块是内建模块，编译的时候附带上即可。

```conf
# ./configure --help |grep http_stub_status_modules
location /status {
	stub_status;
	allow ip_addr;
	access_log   off;
}
```

模块状态说明：

- Active Connection：1		

  当前同一时间访问的请求

- server accepts handled requests: n

  服务器已接受的请求数

- Reading: n Writing:n Waiting: n

  分别是正在读取数据的请求，写入数据的请求，等待处理的请求



### error

- apr_socket_recv: Connection reset by peer

  这是测试客户端系统不允许打开过多的连接，修改系统俩个配置参数就可以了，一个是防止洪水攻击的参数，一个是允许系统打开的最大连接数。

- many files to open

  linux系统不允许打开过多的文件，建立一个socket连接就是打开一个文件





### Linux内核参数



- tcp_max_syn_backlog

  系统所能接受SYN报文段的最大客户端连接数量，即半连接的上限

- somaxconn

  系统所能accept的最大客端数量，即完成连接的上限

如果程序中的backlog数大于somaxconn，则会呗截断成backlog。



疑问：backlog与上面俩个参数有什么关系，是backlog = tcp_max_syn_backlog + somaxconn？还是





### 3. Nginx优化思路



并发数越高意味着服务器能够同时接受socket数越大，socket连接创建的越多，意味着占用系统的内存资源就越多。

注意：因为socket是会创建系统缓冲区的。



流量越大，意味着网卡也需要能够流通的流量很大。



影响硬件俩个方面的因素：内存和网卡。





概念：在linux系统中，一个进程默认允许打开1024个文件

当一个请求发生时，大体是发生俩件事情：
- 客户端发出请求，Nginx服务器建立socket连接
- Nginx服务器打开文件

这俩个步骤涉及到俩个硬件的限制，一个是你的socket连接能否建立这么多？你的内存是否足够大，因为socket连接都需要内存维护着连接信息的，同时操作系统是否允许你打开这么多的文件，因为一个进程操作系统默认允许你打开1024个文件。

总结：操作系统限制了文件的打开数量，限制了socket连接数，当然你的网卡万兆以太网也需要能跑通这么多的流量，内存也要够大。


解决：只要建立的socket连接多，系统允许打开的文件多，并发数就上来了。


观察linux系统连接信息和nginx错误日志
```
dmesg|tail
# possible SYN flooding on port 80. Sending cookies.
# 1. 使用命令dmesg查看系统连接信息，发送提示说可能收到洪水攻击，进行了抵御

tail logs/error_log
# 查看nginx错误日志记录，可以发现出现Too many open files，这里是nginx打开文件数量被限制了
```
从上面的日志可以看出，linux系统和nginx服务器都限制了socket连接数和文件打开数量，调优从这里入手。
优化：

```


# 系统方面的优化
ulimit -n 50000
# 0. 设置系统本身允许打开的文件个数

more /proc/sys/net/core/somaxconn
echo 50000 > /proc/sys/net/core/somaxconn 
# 1. 设置允许打开的最大tcp连接数，somaxcon

cat /proc/sys/net/ipv4/tcp_tw_recycle
echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
# 该参数决定是否加速TIME_WAIT的sockets的回收，默认为0。

cat /proc/sys/net/ipv4/tcp_tw_reuse
echo 1> /proc/sys/net/ipv4/tcp_tw_reuse
# 该参数决定是否可将TIME_WAIT状态的sockets用于新的TCP连接，默认为0。

more /proc/sys/net/ipv4/tcp_syscookies
echo 0 > /proc/sys/net/ipv4/tcp_syncookies
# 此参数是为了防止洪水攻击的，但对于大并发系统，要禁用此设置


注：/proc目录下都是系统运行状态的值，在系统运行时得到的
```

shell文件
``` shell
# tcpopt.sh
echo 50000 > /proc/sys/net/core/somaxconn
echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
echo 1> /proc/sys/net/ipv4/tcp_tw_reuse
echo 0 > /proc/sys/net/ipv4/tcp_syncookies
```

上述的配置完毕后再进行测试，有的时候打不到你想要的测试结果，不一定是服务器的问题，先查看ab命令返回的结果，如果没有错误，可能是你自身的测试环境无法发出那么多的请求。
所以调整测试环境服务器或增加测试环境服务器。

Connections：keep-alive的一些说明，在http1.1协议中，该请求头主要用于跟服务器建立tcp长连接（如果服务器有开启），建立连接时间由服务器决定，这能很好的避免不断建立tcp连接，避免浪费服务器资源

但是在高并发的网站中，tcp是很珍贵的资源，如果让客户端一直保持连接的话，就会占用服务器的资源，因此一般情况下在并发网站是关闭的，如果你真想要用，2秒就够了。

例如：
​	keepalive_timeout 65
​	nginx的配置，指http连接后占用tcp连接资源。

实验有个不明白的地方，使用多台测试机一起对web服务器发出请求，结果nginx服务器的并发数目观察是有问题的，反而使用单独的一台测试机器测试反而没有问题。















### 4. Linux系统内核参数
/etc/sysctl.conf文件 
```
net.ipv4.tcp_syncookies = 0  
#此参数是为了防止洪水攻击的，但对于大并发系统，要禁用此设置
 
net.ipv4.tcp_max_syn_backlog
tcp_max_syn_backlog
# 参数决定了SYN_RECV状态队列的数量，一般默认值为512或者1024，即超过这个数量，系统将不再接受新的TCP连接请求，一定程度上可以防止系统资源耗尽。可根据情况增加该值以接受更多的连接请求。
 
net.ipv4.tcp_tw_recycle
# 参数决定是否加速TIME_WAIT的sockets的回收，默认为0。
 
net.ipv4.tcp_tw_reuse
# 参数决定是否可将TIME_WAIT状态的sockets用于新的TCP连接，默认为0。
 
net.ipv4.tcp_max_tw_buckets
# 参数决定TIME_WAIT状态的sockets总数量，可根据连接数和系统资源需要进行设置。 
```







net.core.somaxconn = 1048576 # 默认为128

net.core.netdev_max_backlog = 1048576 # 默认为1000

net.ipv4.tcp_max_syn_backlog = 1048576 # 默认为1024





nginx

listen       80 backlog=8192; # 默认为511





php-fpm

listen.backlog = 8192 # 默认为-1（由系统决定）