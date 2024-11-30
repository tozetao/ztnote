测试：

锁住一条不存在的记录。不存在则插入，存在则更新。

查询（不同的地方）一条不存在的记录，不存在则插入，存在则更新。











幻读：在可重复读隔离级别下，同个事务俩次同个范围的查询，后一次的查询中包含前一次查询中不存在的记录。

说明：

- 在可重复读隔离级别下，幻读在当前读才会出现，快照读是不会出现幻读的。
- 幻读仅针对新插入的行。

```
for update、for share都是当前读
```



在读提交隔离级别下，普通查询能够读取到其他已经提交事务的修改。在可重复读隔离级别下，多次相同范围的普通查询结果是一致的。

- 普通的查询是快照读，快照读遵循隔离级别定义的实现。

- 当前读，语义上同update一样，都是能够看到最新的数据。







#### 什么是幻读？

要了解什么是幻读就要了解幻读是怎么来的。

MySQL的select语句默认是快照读，当select加了for update、for share时就成了当前读。从语义上来说，当前读表示给查询到的记录加锁，且能够看到加锁数据的最新状态，幻读就是从当前读导致的。比如下面的SQL语句:

> 注：for update是为查询语句加悲观锁，for share是为查询语句加乐观锁。

```sql
create table t(
	id int primary key auto_increment,
	c int,
	d int,
	key(c)
)engine=innodb charset=utf8mb4;

insert into t values(0,0,0), (5,5,5), (10,10,10), (15,15,15), (20,20,20), (25,25,25);

select * from t where d = 5 for update;
```

这条查询语句表示给d=5的所有记录加锁，在本次事务执行过程中（未提交事务）每次select ... for update查询出来的结果都是一致的（除了事务自身的修改），而幻读就是就是在这多次的范围查询中会查出新插入的记录。

```
-- session a
begin;
select * from t where d = 5 for update;		--t1
select * from t where d = 5 for update;		--t3
select * from t where d = 5 for update;		--t5
commit; --t6

-- session b
update t set d = 5 where id = 0; --t2

-- session c
insert into t value(1, 1, 5);	-- t4
```

上面的3个事务的SQL语句假设按照t1-t6依次执行，t3时刻会查询出id=0、id=5俩条记录，因为当前读能够读取到t2的更新。t5时刻能够读取到t4插入的语句，这种现象就是幻读。因为才会说只有在当前读下才会发生幻读，且幻读只针对插入的记录。



#### 幻读带来的问题

**语义上的破坏**

```sql
-- session a
begin;
select * from t where d = 5 for update;		-- t1
update t set d = 100 where d = 5;			-- t1

select * from t where d = 5 for update;		-- t3
select * from t where d = 5 for update;		-- t5
commit; --t6

-- session b
update t set d = 5 where id = 0; -- t2
update t set c = 5 where id = 0; -- t2

-- session c
insert into t value(1, 1, 5);	 -- t4
update t set c = 5 where id = 1; -- t4
```

在t1节点select ... for update表示锁住d=5的所有记录，但是t2节点将id=0的d更新为5，并再次对id=0、d=5进行了更新，这就破坏了t1节点的语义，它并没有锁住所有d=5的记录。



**数据不一致**

这里的一致性除了数据库内数据的一致，还要保证数据和日志在逻辑上的一致。考虑下面的场景：

```sql
-- session a
begin;
select * from t where d = 5 for update;		-- t1
update t set d = 100 where d = 5;			-- t1
commit; -- t6

-- session b
update t set d = 5 where id = 0; -- t2

-- session c
insert into t value(1, 1, 5);	-- t4
```

t1时刻的select ... for update表示要把d=5的所有记录锁住，让下面的update语句来进行更新，session a应该只更新到id=5的这行记录，但是这3个的事务对应的bin log日志的逻辑不对：

```sql
update t set d = 5 where id = 0;	-- 0 0 5
insert into t value(1, 1, 5);		-- 1 1 5

update t set d = 100 where d = 5;	-- 上面俩条记录的d字段都被更新为100了，因为sesion a是在t6时刻提交
```









#### 间隙锁

对于session a的情况，给条件匹配的行加上锁，或者是把所有扫描到的行加上锁，也解决不了幻读，Innodb为了解决幻读带来的问题，引入了间隙锁。



间隙锁：锁的是俩个值之间的空隙。

间隙锁与间隙锁之间不冲突，当往间隙锁锁住的间隙之间插入记录时才会冲突。



next-key lock：间隙锁和行锁合称为next-key lock。next-key lock是左开有闭区间。比如（-∞,0]、（0,5]。







什么情况会触发间隙锁？

select ... for update

对于当前读的select语句，查询一条不存在的记录，查询条件是范围查询，查询条件未走索引，都会触发间隙锁。







#### 加锁规则

这里的加锁规则指的是select ... for share、select ... for update的加锁规则，这些加锁规则是从代码中总结出来的，适用于以下版本：

- 5.x-5.7.24
- 8.x - 8.0.13

> 个人理解：select ... for update等同于update，都是当前读。

只有在可重复读隔离级别下间隙锁才有效，下面的描述如果没有特殊说明都是基于可重复读。





原则1：默认加next-key lock，next-key lock是左开右闭。

原则2：只有扫描过的对象才会加锁。（对象指索引）

优化1：对于非唯一索引，会向右扫描到等值条件不满足的第一个值停止，这时next-key lock会退化为间隙锁。

优化2：对于唯一索引，当扫描到满足等值条件的对象时，next-key lock会退化为行锁。

bug：唯一索引的范围查询，会向右扫描到不满足条件的第一个对象为止。



innodb的锁是加在索引上的。

正常来说无论是lock in share mode或for update，都是排斥更新操作的（悲观锁），然而有一种特殊情况要注意。share mode在给索引加锁时，如果select的索引是覆盖索引，innodb会做一个优化只锁覆盖索引，因此可以通过主键索引进行更新。

```sql
select id from t where c = 5;		-- 覆盖索引
select id, d from t where c = 5;	-- 需要回表去读取数据，因此是非覆盖索引。
```





SELECT ... LOCK IN SHARE MODE sets shared next-key locks on all index records the search encounters. However, only an index record lock is required for statements that lock rows using a unique index to search for a unique row. 

SELECT ... FOR UPDATE sets an exclusive next-key lock on every record the search encounters. However, only an index record lock is required for statements that lock rows using a unique index to search for a unique row.













### 问题

- 读提交隔离级别下有间隙锁吗

  没有

- update一条不存在的记录，中间的空隙是否会加间隙锁。

  会的，update也是当前读。

- 读提交的锁是在什么时候释放？

  在事务提交后一起释放。



- bin log的日志格式有什么不同，我们知道读提交隔离级别不存在间隙锁，那么读提交下如何处理幻读这种问题？

- 为什么读提交隔离级别，bin log日志的格式是row。

- 不同的隔离界别现象有什么不一样？

- 在备份期间，备份线程用的是可重复度，业务线程用的是读提交，同时存在俩个隔离级别，还有问题吗？



其他命令

```
-- 查看加锁情况
select object schema,object name,index name,lock type,lock mode,lock data from performance schema.data locks;



READ COMMITTED
REPEATABLE READ


SET GLOBAL TRANSACTION ISOLATION LEVEL REPEATABLE READ;
SELECT @@GLOBAL.TX_ISOLATION;

SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ;
SELECT @@SESSION.TX_ISOLATION;

SELECT * FROM INFORMATION_SCHEMA.INNODB_LOCKS


参考：
https://cloud.tencent.com/developer/article/2450077
https://www.cnblogs.com/wzh2010/p/18030866
https://www.cnblogs.com/wzh2010/p/17855987.html


begin;
delete from t where c = 10;


begin;
select * from t where id>10 and id <=15 for update;
commit;



select * from t;
insert into t values();
update t set d=100 where id = 15;


insert into t values(10,10,10),(30,10,30);
```

