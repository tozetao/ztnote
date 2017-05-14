## 网站的优化思路
对于高性能网站，请求量大，该如何支撑？
主要从俩个方面，一个是请求连接数的优化，这是属于程序方面做的事情，另外一个是架构方面的优化。

### 1. 请求连接的优化
按照以下几点优化：
1. 减少客户端请求，例如：合并css、合并背景图片
2. 减少对mysql的请求，例如使用页面静态化、服务器缓存技术
2. 针对浏览器缓存，例如：nginx的expires，主要是对于一些图片不经常变动的文件
3. 利用cdn来响应请求

### 2. 架构优化
当一台服务器的性能做到极致的时候，这时候就要考虑以多台服务器做架构来分担一台服务器的压力了。

基本上是才用负载均衡技术，将用户的请求平均分摊给每台服务器，如果请求数量分配均匀，每台服务器的性能都充分利用，那么网站的性能就提升了。

## Nginx 1w并发优化
原视频实验主机配置
- RAM：8G
- HD：500G
- CPU：i5

在拿到一台web服务器后，先做压力测试，判断该服务器缺陷，在进行优化。
主要看俩个层面，一是看系统和nginx服务器上的socket连接数的大小限制，二是在系统和nginx中文件同时打开数量的大小限制。

### 1. ab.exe工具使用说明
ab.exe是Apache自带的压力测试工具。

```
参数说明：
	-n，总请求数
	-c，并发请求数

例如：
	ab -c 100 -n 1000 ip_addr
	# 发送1000次请求，同一时间发出100个请求
```

响应结果说明：
```
Server Software: nginx
Server Hostname: 192.168.0.125
Server Port: 80

Document Path:	/index.html
Document Length: 612 bytes

Concurrency Level:	1000
# 并发等级
Time taken for tests:	13.222 seconds
# 测试时长

Complete requests:	50000
# 请求总数
Failed requests:	3721
# 失败请求数

Write errors:	0
Non-2xx responses:	3818
Total transferred:	39509952 bytes
# 网络总传输量
Html transferred:	29145768 bytes
# html总传输量

Requests per second:	3781.68 (#/sec) (mean)
# 吞吐量，每秒请求数
Time per request:	264.433 [ms] (mean)
# 服务器收到请求，响应页面要花费的时间
Time per request:	0.261 [ms] (mean,across all concurrent requests)
# 并发的每个请求平均消耗时间

Transfer rate：		2918.94 [Kbytes/sec] received
# 平均每秒网络上的流量，可以帮助排除是否存在网络流量过大导致响应时间延长的问题

# 这部分是网络上消耗的时间分解：略
Connection ....

# 整个场景所有请求的响应情况，在场景中每个请求都有一个响应时间
Percentage of the requests served within a certain time (ms) 
  50%    571 
  66%    627 
  75%    646 
  80%    652 
  90%    666 
  95%    677 
  98%    681 
  99%    682 
100%    684 (longest request)
# 左边是请求的百分占比数，右边是请求的响应时间（毫秒单位）
# 50%的请求小鱼571毫秒
# 
```

压力测试建议在局域网中进行，因为外网的话可能会有网络延迟以及传输限制问题。
局域网中压力测试的结果比较准确。

### 2. Nginx压力测试
我们需要俩个工具，一个是apache自带的ab.exe命令，用于做压力测试，另外一个是nginx内建模块http_stub_status_modules，用于查看Nginx服务器的请求状态

http_stub_status_modules说明：
```
./configure --help |grep http_stub_status_modules
# 安装nginx的统计模块，便于观察nginx的状态
# 该模块是nginx内建的模块，编译的时候我们带上就可以了。

# 模块配置
location /status {
	stub_status;
	allow ip_addr;
	access_log   off;
}

# 模块状态说明
Active Connection：1		
# 当前同一时间访问的请求

server accepts handled requests: 0		
# 

Reading:0 Writing:0 Waiting:0
# 分别是正在读取数据的请求，写入数据的请求，等待处理的请求

```


测试的时候预估你机器的最大并发数大小或者自定义并发数大小来进行调整测试，比如说我们要做到并发数1w那么并发数的测试可以由小到大来进行。

测试机器是一台linux，以下是测试场景：
```
./ab -c 1000 -n 50000 http://192.168.0.125
# 先测试100并发效果

./ab -c 2000 -n 50000 http://192.168.0.125
# 再测试2000并发效果，发生错误

Benchmarking 192.168.0.125
socket: Too many open files
# 指测试机器的socket连接数过多，超出系统限制。
# 说明：在linux中网卡是作为文件看待的，每建立一次socket就要打开一次文件，
# linxu系统有限制同时打开的文件数量，所以报错。

ulimit -n
# 查看机器的最大连接数
# 由于测试机器的连接数限制，所以我们先设置测试机器的最大连接数

ulimit -n 20000  
# 设置最大连接数，该命令机器重启会失效
```
测试数据就不写了，主要看俩个方面。
一是ab命令返回的响应结果，查看失败请求数量、请求响应时间的总体情况，如果达标，再查看Nginx的并发请求数量是否达标，也可以查看Nginx的错误日志来排错。

./ab -c 2000 -n 100000 http:xxx.com/index.html
在我的虚拟机进行测试ab响应数据基本挂掉，10w个连接差不多有9w个挂掉，同时nginx状态最多接受俩三百请求，这是在不优化的情况下。

注1：apr_socket_recv: Connection reset by peer
这是测试客户端系统不允许打开过多的连接，修改系统俩个配置参数就可以了，一个是防止洪水攻击的参数，一个是允许系统打开的最大连接数。

主2：many files to open
linux系统不允许打开过多的文件，建立一个socket连接就是打开一个文件

### 3. Nginx优化思路
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


# nginx方面的优化
worker_connections 10000
# 1. 设置Nginx允许建立的socket数量

worker_rlimit_nofile 10000
# 2. 设置子进程允许打开的最大文件数


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
	keepalive_timeout 65
	nginx的配置，指http连接后占用tcp连接资源。


实验有个不明白的地方，使用多台测试机一起对web服务器发出请求，结果nginx服务器的并发数目观察是有问题的，反而使用单独的一台测试机器测试反而没有问题。

### 4. Linux系统内核参数
/etc/sysctl.conf文件 
```
net.ipv4.tcp_syncookies = 0  
#此参数是为了防止洪水攻击的，但对于大并发系统，要禁用此设置
 
net.ipv4.tcp_max_syn_backlog
# 参数决定了SYN_RECV状态队列的数量，一般默认值为512或者1024，即超过这个数量，系统将不再接受新的TCP连接请求，一定程度上可以防止系统资源耗尽。可根据情况增加该值以接受更多的连接请求。
 
net.ipv4.tcp_tw_recycle
# 参数决定是否加速TIME_WAIT的sockets的回收，默认为0。
 
net.ipv4.tcp_tw_reuse
# 参数决定是否可将TIME_WAIT状态的sockets用于新的TCP连接，默认为0。
 
net.ipv4.tcp_max_tw_buckets
# 参数决定TIME_WAIT状态的sockets总数量，可根据连接数和系统资源需要进行设置。 
```