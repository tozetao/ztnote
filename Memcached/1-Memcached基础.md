## Memcached基础

### 1. 安装
memcached依赖于libevent库
```
# 编译安装libevent

# 编译安装memcache，注意libevent库的依赖
```

注1：wget命令在解析https链接可能会出现证书问题，使用--no-check-certificate参数即可解决，如果是无法写入可能是文件名字过长，通过-O参数来命名文件。

注2：在编译的时候如果出现无限循环编译问题，那可能是因为虚拟机的时钟问题，本地时间不正确出现的事件。

### 2. Memcached运行
```
/usr/local/memcached/bin/memcached -h

启动选项：
-p：memcached监听的端口
-m：要分配的内存大小
-d：以守护进程启动
-l：监听服务器的地址，可以有多个
-c：最大运行的并发连接数
-P：设置保存Memcached的pid文件
-v：输出警告和错误信息
-vv：打印客户端的请求和返回信息

```
memcached客户端与服务器端的通信是基于文本的协议，而不是二进制协议（http协议也是这样），所以通过telent连接memcached来操作即可。
```
语法：
	telnet host port

telnet 127.0.0.1 11211

ctrl+]
# 用于打开回显，在该模式下就可以用命令操作memcached
```

### 3. 命令详解
hash，1要求在某个范围内随机存储，2要求key的重复度低。

add
```
语法：add key flag expire length
说明：用于往内存增加一条记录，只能添加不存在的key。

参数：
key
	键名

flag的意义
	标识，要求是一个正整数。
	Memcached基于文本协议，传输的东西都是以字符串来存储。
	如果想要存储一个对象或者数组，存储的时候需要序列化，取出的时候需要反序列化，
	反序列化可能有多种类型，例如对象，数组、json。
	这时候flag的意义就体现出来了，你可以规定要反序列化的类型。
	
exipre
	缓存有效期，有3种格式，
	1是设置秒数，从设置开始计数，N秒后失效。
	2是时间戳，在指定的时间戳失效，例如团购网站，缓存的某个团在中午12点失效。
	3是设为0，不自动失效，0并非永久有效，编译memcached时可以指定一个最长常量，
	默认是30天，或者可能等待不到30天就会被新数据踢出。

例子：
	add name 0 10 5
	# 10秒后失效

	add name 0 1379209950 5
	# 在时间戳是1379209950失效
```

replace
```
语法：replace key flag expire length
说明：替换指定的key，只能替换存在的key，不存在的key无法替换。

参数说明：
	同add命令
```

delete
```
语法：delete key [seconds]
说明：删除指定的key

参数：
	加秒数之后，是指被删除的key，n秒内不能再被使用，
	目的是让网站上的页面缓存也代谢完毕。
```

get
```
语法： get key

说明：
	查询，返回key的值
```

set
```
语法：set key flag expire length
说明：
	用于设置和修改key的值，set相当于add和replace的集合，
	如果添加的这个key不存在，则直接添加；如果添加的key存在，则对key进行替换。
```

incr
```
语法：incr key increment_value
说明：增加一个key值的大小，memcached是将值作为32位无符号来做运算的。
```
decr
```
语法：decr key decrement_value
说明：减少一个值的大小，同上。
```
incr、decr的应用场景，例如秒杀功能
```
# 一个人下单，要牵扯数据库读取、写入订单、更改库存以及事务等要求，
# 这样对于传统数据库来说，一瞬间的I/O压力是巨大的。

# 解决的方案是在秒杀的时候，在内存中获取一个标识派发给用户，拥有标识的用户才有后续操作。
# 利用memcached的decr功能，在内存中存储count库存数，请求过来的时候则递减并判断库存量，如果库存非0非给用户一个标示，表示用户抢到订单，然后再完成后续的处理
# 通过这种方式来减少对数据库的压力
```

stats
```
说明：统计命令，把memcached当前的运行信息统计出来

输出选项说明：

curr_items，当前存在的key的个数
total_items，memcached历史上存储所有key的个数

cmd_get，请求的次数
get_hits，命中的次数
get_miss，miss的次数
# 上述3个选项能算出缓存的命中率，40%-50%不错，70%-80%不错

cmd_set，

```
flush_all
```
说明：清空memcached所有的缓存对象
```