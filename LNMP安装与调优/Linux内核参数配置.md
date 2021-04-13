### Linux内核参数

/ect/sysctl.conf文件，该文件是一个允许改变正在运行时中的Linux系统内核参数的接口，它包含一些TCP/IP堆栈和虚拟内存的高级选项，修改参数将永久生效。

与网络相关的内核参数：

```ini
fs.file-max = 999999

net.ipv4.tcp_max_syn_backlog = 1024 # 默认为1024
net.ipv4.tcp_tw_reuse = 1
net.ipv4.tcp_syncookies = 1

net.ipv4.tcp_keepalive_time = 600
net.ipv4.tcp_fin_timeout = 30
net.ipv4.tcp_max_tw_buckets = 5000
net.ipv4.ip_local_port_range =1024    61000
net.ipv4.tcp_rmem =4096 32768 262142
net.ipv4.tcp_wmem =4096 32768 262142

net.core.rmem_default = 262144
net.core.wmem_default = 262144
net.core.rmem_max = 2097152
net.core.rmem_max = 2097152

net.core.netdev_max_backlog = 8096 # 默认为1000
net.core.somaxconn = 1048576 # 默认为128
```

- tcp_max_syn_backlog

  决定了SYN_RECV状态的socket队列大小。

  一般默认值为512或者1024，即超过这个数量，系统将不再接受新的TCP连接请求，一定程度上可以防止系统资源耗尽。可根据情况增加该值以接受更多的连接请求。

- tcp_tw_reuse

  参数决定是否加速TIME_WAIT的sockets的回收，默认为0。

- tcp_syncookies

  此参数是为了防止洪水攻击的，但对于大并发系统，要禁用此设置

- tcp_max_tw_buckets

  这个参数表示操作系统允许TIME_WAIT套接字数量的最大值，如果超过这个数字，TIME_WAIT套接字将立刻被清除并打印警告信息。默认是i180000,过多TIME_WAIT套接字会使Web服务器变慢。



- somaxconn

  系统所能accept的最大客端数量，即完成连接的上限。如果程序中的backlog数大于somaxconn，则会被截断成backlog。

- netdev_max_backlog

  当网卡接收数据包的速度大于内核处理的速度时，会有一个队列保存这些数据包。这个参数表示该队列的最大值。

- ulimit -n

  可显示系统允许打开的最大文件数



/proc/sys目录，该目录存放着系统运行时的大多数内核参数，并且可以在系统运行时修改，在机器重启后会失效。

example：直接修改系统运行时配置

```shell
echo 50000 > /proc/sys/net/core/somaxconn 
echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
echo 1> /proc/sys/net/ipv4/tcp_tw_reuse
echo 0 > /proc/sys/net/ipv4/tcp_syncookies
ulimit -n 10000
```



### backlog

nginx配置backlog

```ini
listen 80 backlog=4086;
#在nginx中，backlog默认是511
```

php-fpm配置backlog

```ini
listen.backlog = 8192
#默认为-1（由系统决定）
```

问题：listen()系统调用的backlog是指半连接与完全连接倆者相加的总数还是指其中某个的总数?



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



### 硬件要求

高并发意味着服务器能够同时接受更多的连接，连接数（socket）创建的越多，意味着瞬间接收和发送的数据量就越多。

所以高并发影响的硬件是内存和网卡，socket的创建是需要缓冲区的，所以连接数越多越占用更多的内存。而流量越大，意味着网卡要能够承载的流量就越大。





### nginx error

- apr_socket_recv: Connection reset by peer

  这是测试客户端系统不允许打开过多的连接，修改系统俩个配置参数就可以了，一个是防止洪水攻击的参数，一个是允许系统打开的最大连接数。

- many files to open

  linux系统不允许打开过多的文件，建立一个socket连接就是打开一个文件



- possible SYN flooding on port 80. Sending cookies.

  使用命令dmesg|tail查看系统连接信息，发送提示说可能收到洪水攻击，进行了抵御



- upstream prematurely closed connection while reading response header from upstream

- connect() to unix:/dev/shm/php-fcgi.sock failed (11: Resource temporarily unavailable) while connecting to upstream
  连接到上游资源时暂时不可用，也就是php-fpm的backlog队列太小，过多的请求被抛弃了。





