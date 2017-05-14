## 主从复制

### 1. 典型问题
有这么一个业务场景，经过测试，读写比的比例为1:20，请根据读写比合理设置优化方案。

读写比，即写入数据/读取数据的比例，写入指insert/update/delete操作，读指select操作，一般我们会使用读写分离技术来进行优化，根据比例会有若干台读服务器，少量写服务器。

### 2. 读写分离实现
服务器端读写分离的具体技术实现大体有2种，分别是：
- 数据库集群技术
- 数据库复制（replication）

数据库集群技术，集群由SQL节点（sql node），数据节点（data node），管理节点（ndb managerment）组成，SQL语句发送"SQL节点"，SQL节点发往数据节点，再由管理节点完成数据节点之间的实现。
集群技术比较复制，至少要有3个节点4台服务器才能完成。

数据库复制，主服务器凡事运行语句，都会产生一个二进制日志文件binlog，从服务器不断读取主服务器的binlog，并将读取到的binlog转换为自身可执行的relaylog，再执行relaylog。

数据库集群技术比较复杂，一般是通过数据库复制来实现读写分离。

### 3. MySQL数据库复制实现（replication）
数据库复制实现读写分离的话，会将服务器分为主服务器和从服务器，即master和slave。
主服务器针对写入操作，一般是1台，当然也可以有多台，
从服务器针对读取操作，一般是多台。

实现的原理是让从服务器跟踪主服务器的磁盘状态或者执行的SQL语句，要么从服务器的磁盘跟着主服务器磁盘变化，或者主服务器执行SQL语句，从服务器也跟着执行，这样就保证了主从服务器数据的一致性。

实现过程：
1. 主服务器打开二进制日志功能，确保主服务器有数据变化就能产生二进制日志（binlog）。
2. 从服务器要开启二进制日志（binlog）同时开启relay日志功能，确保可以从主服务器读取二进制日志（binlog）并产生relaylog，再执行relaylog。

注1：一般主服务器会创建一个授权复制账号，从服务器则利用复制账号来监听主服务器的日志变化。
注2：mysql的配置文件一般在/etc/my.cnf

**编辑主服务器配置文件**
```sql

log-bin=mysql-bin
# 开启master服务器的二进制日志，00001是编号，用于区分主服务器

server-id=102
# 服务器ID，用于区分从属服务器
# 因为从服务器也可以有自己的从服务器，服务器之间的关系类似一棵树，所以需要ID区分
# 建议配置成局域网后三位IP地址

binlog-format=mixed
# 混合监听，由MySQL决定
# 监听变化有俩种：# statment、row、mixed
# statment是监听SQL语句变化、row是监听磁盘变化（数据变化）
# 俩种方式各有各的好处
```
**编辑从服务器配置文件**
```sql
relay-log=mysql-relay
server-id=103
```

**创建授权账号**
```sql
/usr/libexec/mysqld --skip-grant-tables
# 跳过权限检测，mysql启动不需要用户名

use mysql;
select host,user,password from user;
# user表存储了允许登录的IP、用户和密码

flush privileges;
# 刷新权限

grant replication client, replication slave on *.* 
to 'repl'@'192.168.%.%'
identified by 'repl'; 
# *.*，意思是作用于全部数据库下的所有表
# to是分别给什么账号，@是允许登录的IP地址，%是泛指，表示某个区域段的IP地址
```

**指定从服务器连接主服务器的授权账号**
```sql

ls /var/lib/mysql/
# 存储mysql日志文件信息
# mysql每次启动都会产生一个新的二进制日志文件

more mysql-bin.index
# 显示当前使用的日志文件信息

show master status;
# 显示主机状态
# file字段：当前使用的日志文件，即slave服务器要监听的文件
# position字段：指定从服务器监听的文件指针地址

changer master to
master_host='192.168.1.199'
master_user='repl'
master_password='repl'
master_log_file='mysql-bin.00003'
master_log_pos=278
# 指定从服务器的主机地址、连接的授权账号和日志地址
# master_log_file和master_log_pos俩个字段的值要参考master status。

show slave status;
# 查看从服务器的状态

reset slave;
# 重置从服务器

start slave;
# 启动从服务器，启动后会一直监听主服务器的状态

stop slave;
# 停止从服务器的监听

# 最后再进行主从相关配置即可
```
**最后重启下mysql服务器进程即可**

### 4. 主服务器日志格式说明
主服务器的日志格式有statement、row、mixed3中，mixed是前俩种的混合。
statement是指从服务器监听主服务器上的语句变化，
row模式是指从服务器监听主服务器上的磁盘变化。

mixed能根据语句的不同而选择合适的日志格式。
说明：
```sql
insert into table_name values('lisi');

# 举例，上述的插入语句，MySQL需要分析SQL语句语法，在将数据插入到磁盘中
# 碰到这种情况，用row模式好，因为从服务器直接复制主服务器的磁盘数据变化，不需要有分析SQL语句的过程

# 使用row格式是直接复制磁盘上1行数据变化

update table_name set name='xxx' where id = 1;
# 该update语句也是只影响一行数据，用row比较合适

update table_name set salary=salary+100;
# 该update语句是针对磁盘上每一行数据的，因此磁盘上的很多条数据都发生变化
# 这种情况下row模式复制起来很麻烦，所以这种情况适合使用statement模式

```

## 主主复制
在上面的配置中，俩台服务器地位有差别，一主一从，从服务器是起到一个备份的作用，同时也能分担主服务器的查询压力。在主从模式中，从服务器无法接受业务状态的状态，它只是根据主服务器的变化而变化。

主主复制模式的话，俩台服务器的地位是一样的，都可以接受业务逻辑处理后的数据变化，同时俩台服务器的数据保持一致，主主复制相当于MySQL分片+复制。

### 1. 部署思路
1. 2台服务器都设置2进制日志和relay日志
2. 相互创建replication账号
3. 相互设置为自己的master

```sql
# 108 mysql服务器配置
server-id=108
log-bin=mysql-bin
binlog-format=mixed
relay-log=mysql-relay

# 111 mysql服务器配置
server-id=111
log-bin=mysql-bin
binlog-format=mixed
relay-log=mysql-relay


# 相互创建授权账号
grant replication client,replication slave on *.*
to 'repl'@'192.168.%.%'
identified by 'repl'

# 相互指定对方为主服务器
change master to
master_host='192.168.1.111',
master_user='repl',
master_password='repl',
master_log_file='mysql-bin.000003',
master_log_pos=106;

change master to
master_host='192.168.1.108',
master_user='repl',
master_password='repl',
master_log_file='mysql-bin.000004',
master_log_pos=106
```
关于请求的分流，你可以根据用户id来配置，奇数访问的服务器和偶数访问的服务器是不同的。

### 2. 主键冲突问题
俩台服务器地位相等，假如同时有多个请求到达2台服务器，在这瞬间就有可能产生多个相同的主键ID。

解决：调整服务器的主键增长率，a服务器以1,3,5递增，b服务器以2,4,6自增

```sql
# a服务器
set global auto_increment_increment=2;	# 增长步伐
set global auto_increment_offset=1;		# 起始值
set session auto_increment_increment=2;
set session auto_increment_offset=1;

# b服务器
set global auto_increment_increment=2;	# 增长步伐
set global auto_increment_offset=2;		# 起始值
set session auto_increment_increment=2;
set session auto_increment_offset=2;

# 注：当前命令要写到配置文件中，放置服务器重启失效
# global是全局的意思，session是当前连接回话
```

如果有2台以上的主服务器，解决主键冲突的方法有：
- 要么从业务逻辑上来处理，以redis为例，可以构建一个全局的id，每次新增数据库就用该id进行插入，然后再递增
- 或者每天生成几万个id，存储在队列中供使用。

### 3. 被动主主复制
被动模式下的主主模式下，俩台服务器的地位是一样的，但是其中一台服务器为只读，并且业务中也只写某一台服务器。

这种模式主要体现一个好处，如果供写入的服务器出了故障，能迅速切换到另外一台主服务器，或者出于检修的目的将写入功能暂时切换到另外一台服务器也比较方便。

配置：
```sql
read-only=on/1
# 同主动的主主复制模式一样，只不过额外设置只读的服务器。

show variables like '%read%';
# 查看配置


```

被动模式下的主主复制一般的每台主服务器下都会有多台从服务器，这样不仅能分担读写压力，同时对数据的备份有做了一层保障。

### 4. mysql-proxy
如果做了读写分离，要路由SQL语句有俩种办法：
- 在php做逻辑判断，写入的SQL语句则请求主服务器，查询SQL语句则请求从服务器
- 使用集群中间件，例如官方的mysql_proxy、淘宝的amoeba

mysql-proxy能帮助我们完成负载均衡与读写分离，
简单的说通过该中间件，我们不需要在程序中做业务判断，该中间件自动帮程序去调用数据库，这样方便扩展，在增加数据库服务器的时候不需要去改动程序。

**mysql-proxy配置：**
```
mysql-proxy --help-all
# 查看全部帮助

mysql-proxy -P 4040   
--proxy-backed-addresses=192.168.1.108:3306
--proxy-backed-addresses=192.168.1.111:3306
# 配置mysql-proxy监听端口
# 配置mysql-proxy管理的服务器
# mysql-proxy -P 192.168.1.100:4040

mysql -h192.168.1.100 -P 4040 -uroot -proot
# 使用mysql去连接mysql-proxy中间件来建立MySQL链接

ls share/doc/mysql-proxy/rw-splitting.lua
# 该脚本是用于读写分隔

./bin/mysql-proxy 
--proxy-backed-addresses=192.168.1.108:3306
--proxy-read-only-backend-addresses=192.168.1.111
--proxy-lua-script=/usr/local/mysql-proxy/share/doc/mysql-proxy/rw-splitting.lua

./bin/mysql-proxy
-b 192.168.1.108 
-r 192.168.1.1111
-s /usr/local/mysql-proxy/share/doc/mysql-proxy/rw-splitting.lua
# 简写
```

**负载均衡测试**
mysql-proxy的负载均衡是以mysql连接为单位的均衡，并不是指SQL语句方面的均衡。

主要是考虑如果操作事务，有多条SQL语句在执行，做SQL语句方面的均衡这样会导致数据的不一致各种问题。


**读写分离测试：**
读写分离，主服务器负责写入数据也负责查询数据，从服务器只服务器查询数据，
并不是说主服务器不查询数据的，在压力大的时候会将查询请求分担给从服务器，减少主服务器的负担。