我现在要用mysql和golang来开发一个接口，

video_like表:
id:
uid
video_id
status

id是自增组件，uid是用户id，video_id是视频id，status是状态，1表示已点赞，2表示取消点赞。
uid和video_id组成唯一索引。

现在我要设计一个视频点赞接口，要先查询用户是否有为某个视频点赞。如果未点赞，那么以用户id、视频id、status=1插入一条记录。
如果该用户对该视频的点赞记录已经存在了，当用户取消点赞后再次点赞时，要把点赞记录的status改为1。


用golang的gorm框架来实现这个接口，要考虑并发问题。



stu
id primary
name
age


1    php     10
3    java     25
6    c#       30
10  html    33
19  erlang 22


间隙锁
select * from stu where id between 1 and 3 for update;
[1,3]id=1和id=3之间的记录都会被加上间隙锁，对于间隙中已存在的记录的更新会阻塞，而插入id处于间隙中的记录会阻塞。

update t set age = 1 where id = 5;
由于id=5的记录不存在，会在id3-6之间之间加间隙锁。
测试可以发现无法插入id在3-6之间的记录，但是更新id在3-6之内的不存在的记录不会阻塞，返回的行数为0.
注：（3，6）开区间，不包含3和6。



update video_like set status = 1 where uid = 1 and video_id = 1;

-- 加间隙锁[1-5]
select * from t where id between 1 and 5 for update;
效果：
针对id1到5之间的更新（insert、update、delete）都会阻塞。





幻读：在可重复读隔离级别下，同个事务俩次同个范围的查询，后一次的查询中包含前一次查询中不存在的记录。

说明：

- 在可重复读隔离级别下，幻读在当前读才会出现，快照读是不会出现幻读的。
- 幻读仅针对新插入的行。

```
for update、for share都是当前读
```



什么是幻读？为什么会有幻读？

首先幻读是在可重复读的当前读才会出现的，它只针对新插入的行。为什么这么说？

要了解什么是幻读就要了解幻读是怎么来的。

MySQL的select语句默认是快照读，当select加了for update、for share时就成了当前读。在可重复读下，当前读的语义表示给查询到的记录加锁。

```sql
create table t(
	id int primary key auto_increment,
	c int,
	d int,
	key(c)
)engine=innodb charset=utf8mb4;;

select * from t where d = 5 for update;
```

比如上面这条查询语句表示给d=5的所有记录加锁，在当前事务中除了事务自身的修改，在本次事务执行过程中（未提交事务）每次select ... for update查询出来的结果都是一致的，而幻读就是就是在这多次的范围查询中查出不一样的结果。

```
-- session a
begin;
select * from t where d = 5 for update;		-- t1

select * from t where d = 5 for update;		-- t3

-- session b
insert into t value(1, 1, 5);	-- t2
```

上面的俩个事务，如果在t2时刻插入一条记录，t3时刻的查询就会多出一条新的记录。





注：for update是为查询语句加悲观锁，for share是为查询语句加乐观锁。



```
for update: 悲观锁
for share: 乐观锁
```

update语句是否会加入间隙锁？





幻读：在不引入间隙锁时，可重复读下会出现的可预见的问题。

考虑一个问题，只有行锁而没有间隙锁会导致什么问题？



给条件匹配的行加上锁，或者是把所有扫描到的行加上锁，也解决不了幻读。



带来的问题：

```
-- 破环语义
-- 考虑只给d=5的记录加锁
select * from t where d = 5 for update;

-- 数据一致性问题
不仅仅是数据库内部中数据的一致性，数据和日志在逻辑上的一致性也是很重要的。
```



我的问题：

update一条不存在的记录，中间的空隙是否会加间隙锁。

已经知道的会加间隙锁的地方：

```
select for update
where条件的字段没有索引，走全表扫描
有索引，where条件没有命中记录
```



间隙锁

next-key lock：间隙锁和行锁合称为next-key lock。next-key lock是左开有闭区间。比如（-∞,0]、（0,5]。



什么时候加间隙锁

什么时候有next-key lock





我在文章一开始就说过，如果没有特别说明，今天和你分析的问题都是在可重复读隔离级别下的，间隙锁是在可重复读隔离级别下才会生效的。所以，你如果把隔离级别设置为读提交的话，就没有间隙锁了。但同时，你要解决可能出现的数据和日志不一致问题，需要把 binlog 格式设置为 row。这，也是现在不少公司使用的配置组合。





```
-- 查看加锁情况
select object schema,object name,index name,lock type,lock mode,lock data from performance schema.data locks;

参考：
https://cloud.tencent.com/developer/article/2450077
https://www.cnblogs.com/wzh2010/p/18030866
https://www.cnblogs.com/wzh2010/p/17855987.html
```















