## redis

redis使用单线程架构和I/O多路复用技术 来实现高性能的内存数据库服务。



单线程

单线程处理命令，多个命令在处理时是放在队列中的，因此命令是逐个执行的。由于是纯内存的访问，单线程处理命令也不会影响到访问性能。



非阻塞I/O

使用epoll作为I/O多路复用技术的实现。



redis只有库和键的概念，键名的设计要规范，推荐使用：业务名-对象名-id



redis是一个高级的key/value存储系统，与Memcached相比它能对数据持久化，所以redis才叫做存储系统，而不是缓存系统。



### 安装
- 下载源码解压
- 编译，make（redis帮我们配置了源码，所以不用configure）
- 安装，make PREFIX=/usr/local/redis install，这里安装并指定路径

注1：make需要根据系统位数编译，如果是32位系统，需要make 32bit
注2：PREFIX必须大写



### 启动
- 配置，这里从源码文件夹中拷贝一份配置文件放到安装目录中。
- 指定配置文件并启动，./bin/redis-server ./redis.conf

说明：通过配置文件的daemonize选项，该选项表示是否后台运行。



### 目录

src目录的可执行文件有：

- redis-benchmark，性能测试工具
- redis-check-aof，日志文件检测工具（比如断电造成日志损毁，可以检测并修复）
- redis-check-dump，快照文件检测工具，效果如上
- redis-cli，客户端工具
- redis-server，服务端，redis服务进程



















## 命令

redis的命令大体可以分为4类：

- 增加
- 删除
- 批量操作：批量增加、删除等
- 查询

### keys

查找数据库中所有键。

- 语法

  > keys pattern

  pattern是匹配模式，可以使用正则进行匹配

- 时间复杂度

  O(N)，N是数据库key的数量



### EXPIRE

设置给定key的生存时间，当key过期时，将会被自动删除。

- 语法

  > EXPIRE key seconds

在redis中，带有生存时间的key被称为volatile。

如果一个命令只是修改带生存时间的key的值，而不是用新的key来代替它，那么该key的生存时间不变。例如对一个string类型的key执行自增自减，对一个列表进行lpush操作，这些操作都不会改变key的生存时间



### TTL

返回一个key的剩余的生存时间。

如果key不存在返回-2，如果key存在但是没有设置生存时间返回-1，否则以秒为单位返回key的生存时间。

- 语法

  > ttl key



### SCAN

扫描





### DBSIZE

返回当前数据库key的数量

- 时间复杂度

  O(1)



### EXISTS

检查给定的key是否存在

- 语法

  > exists key

- 时间复杂度

  O(1)



### sort

根据给定的选项，对输入的列表、集合和有序集合进行排序，然后返回或者存储排序的结果。

- 语法

  > sort  source-key  \[by pattern]  \[limit  offset  count]  \[GET  pattern]  \[GET  pattern...]  \[ASC|DESC]  \[ALPHA]  \[STORE  dest-key]  

