## 获取有性能问题的SQL
一般有3种方式来获取有问题的SQL语句
- 通过用户反馈获取存在性能的SQL
- 通过慢查询日志获取性能问题的SQL
- 实施获取存在性能问题的SQL

## 慢查询
MySQL慢查询是一种占用资源低的获取性能有问题SQL语句的解决方案，占用的资源主要是磁盘I/O和存储日志所需要的空间。

slow_query_log
启动或停止记录慢查询日志，该参数是可以动态配置的。

slow_query_log_file
指定慢查询日志的存储路径及文件，默认情况下是保存在MySQL的数据目录中，建议日志和数据分来存储。

long_query_time
指定记录慢查询日志SQL执行时间的伐值，这个参数可以精确到微秒，但是参数的单位是秒，参数的默认值是10秒，通过改为0.0001秒比较合适，也就是1毫秒可能比较合适。

long_queries_not_using_indexes
是否记录记录未使用索引的SQL，即使该SQL执行的速度没有超过long_query_time指定的时间，该语句也会被记录。

1秒 = 1000毫秒；1毫秒 = 1000微秒

慢查询会记录所有符合条件的SQL语句，包括查询语句、数据修改语句以及已经回滚的SQL语句。

## 慢查询日志分析工具

### 1. mysqldumpslow
MySQL官方自带的慢查询日志分析工具，可以汇总除查询条件外其他完全相同的SQL，并将分析结果按照参数中所指定的顺序输出。

```sql
mysqldumpslow -s r -t 10 show-mysql.log

-s order(c,t,l,r,at,al,ar)

# -s是指定输出结果集的排序方式，order中是排序方式的选项。
# c是总次数，t是总时间，l是锁的时间，r是总行数
# 以a开头的选项是以平均数量来排序，

# -t top
# -t是指定取前几条作为结束输出
```

### 2. pt-query-digest
该工具能够生成慢查询分析日志，分析过程是对查询语句的条件进行参数化，然后对参数化后的查询进行分组统计，统计出各查询的执行时间、次数、占比等，并且包括查询语句的查询计划，是一个非常方便的日志分析工具。

使用案例：
```sql
pt-query-digest --explain h=127.0.0.1,u=root,p=p@ssW0rd slow-mysql.log
```

## 实施获取慢查询SQL
利用information_schema数据库下的processlist表来实现实时获取有问题的SQL。
```sql
select id,user,host,db, command, time,state,info from information_schema.PROCESSLIST where time>=60
# 该语句用于查询当前服务器执行时间大于等于60秒的SQL

# 通过该语句我们可以写一个定时执行脚本，就能知道哪些SQL执行速度是比较慢的
```

## SQL的解析预处理
### 1. SQL的执行步骤
1. 客户端发送SQL请求给服务器
2. 服务区检查是否可以在查询缓存中命中该SQL，如果命中则直接返回查询结果
3. 在没有命中的情况下，服务器端会进行SQL解析，预处理，再由优化器生成对应的执行计划
4. 根据执行计划，调用存储引擎API来查询计划
5. 将结果返回客户端

### 2. 查询缓存
在解析一个查询语句之前，如果查询缓存是打开的，MySQL会优先检查这个查询是否命中查询缓存中的数据，查询缓存是通过一个对大小写敏感的哈希查找实现的。

Hash查找只能进行全值匹配，如果查询语句与查询缓存中哪怕有一个字节的不同，也无法命中缓存。在命中查询缓存的情况下，会检查用户权限（也是在缓存中进行），权限验证通过下会直接返回缓存中的数据，这种情况下查询语句不会被解析，也不会生成执行计划。

从上面可以看出从查询缓存中返回缓存结果是不容易的，
- 查询SQL和缓存SQL要完全一致，即使只有一个字节不同也无法命名
- 即使缓存命中，在缓存中缓存数据所涉及的表一旦发生更新，都要对缓存结果进行刷新，也就是说同一个表中不涉及查询的字段被更新了，那么缓存也是无法命中的
- 每次在缓冲中检查SQL是否命中时，都会对缓存进行加锁

总结：对于一个读写频繁的系统使用查询缓存很可能会降低查询处理效率，在这种情况下建议不使用查询缓存。

查询缓存参数：
- query_cache_type：设置查询缓存是否可用，on、off、demand，demand表示只有使用SQL_CACHE和SQL_NO_CACHE来控制是否需要缓存
- query_cache_size：设置查询缓存的内存大小，单位必须是1024的整数倍
- query_cache_limit：设置单个查询缓存可用存储的最大值，超过这个值则不会被缓存
- query_cache_wlock_invalidate：设置数据表被锁后是否返回缓冲中的数据，建议off
- query_cache_min_res_unit：设置查询缓存分配的内存块最小单位

在一个读写频繁的系统中建议关闭，query_cache_type设置为off，query_cache_size设置为0。

### 3. 查询计划
在查询缓冲未命中的情况下，MySQL会将SQL语句转换成执行计划，MySQL再依据该执行计划与存储过程交互，该阶段包括多个子过程，包括解析SQL，预处理以及优化SQL执行计划。

语法解析阶段是通过关键字对MySQL语法进行解析，并生成一颗对应的"解析树"，预处理阶段是根据MySQL规则进一步检查解析树是否合法，例如检查查询中所涉及的表和数据列是否存在或名字和别名是否存在歧义等等。

在语法检查通过后，查询优化器就可以生成查询计划了。

生成错误的执行计划的原因：
- 统计信息不准确，例如innodb引擎的表中数据的总行数，由于其架构无法进行正确统计。
- 执行计划中的成本估算不等同于实际的执行计划的成本
- MySQL从不考虑其他并发的查询，这可能会影响当前查询的速度
- ...


### 4. MySQL优化器可优化的SQL类型
- 重新定义表的关联顺序，优化器会根据统计信息来决定表的关联顺序
- 将外连接转化成内连接
- 使用等价变换规格，优化一些查询条件，例如5=5 and a>5会优化成a>5
- 优化count()、min()和max()


select tables optimized away
优化器已经从执行计划中移除了该表，并以一个常数代替。

### 5. 查询计划各个阶段的消耗时间
### profile
```sql
set profiling = 1;
# 启动profile，这是一个session级的配置，启动后会记录会话连接中所有SQL语句都会记录消耗时间和其他查询状态。

show profiles;
# 该命令可以查看每一个查询锁耗费的总时间信息

show profile for query N;
# 查询的每个阶段所耗费的时间，N是查询语句的id，即上个命令的query_id

show profile cpu for query N
# 包含CPU执行信息

show warnings;
# 显示警告信息
```

### performance_schema
5.5版本引入的新的分析引擎，由于该命令开销较大，所以在配置中是默认关闭的(5.6以上的版本性能开销较小)。

performance_schema是全局生效的，会监听所有连接执行的SQL语句。
```sql
use performance_schema
# 切换数据库

update setup_instruments set enabled='YES', timed='yes' where name like 'stage%';
update setup_consumers set enabled='yes' where name like 'events%';
# 启用performance_schema
# 
```

查看监听的SQL语句后面看。。。。


### 6. 特定SQL语句的优化
很重要，回头再看。
