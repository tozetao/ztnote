有空要过下MySQL官方手册对于InnoDB数据引擎的介绍，查看有什么不同的地方。



https://dev.mysql.com/doc/refman/5.7/en/innodb-locks-set.html

https://dev.mysql.com/doc/refman/5.7/en/mysql-acid.html









A < B
begin; /* 启动事务 */
insert into `like`(user_id, liker_id, relation_ship) values(A, B, 1) on duplicate key
​     update relation_ship=relation_ship | 1;

select relation_ship from `like` where user_id=A and liker_id=B;

/* 代码中判断返回的 relation_ship，
  如果是 1，事务结束，执行 commit
  如果是 3，则执行下面这两个语句：
  */
insert ignore into friend(friend_1_id, friend_2_id) values(A,B);
commit;

A > B
mysql> begin; /* 启动事务 */
insert into `like`(user_id, liker_id, relation_ship) values(B, A, 2) 
​    on duplicate key update relation_ship=relation_ship | 2;
select relation_ship from `like` where user_id=B and liker_id=A;
/*
  代码中判断返回的relation_ship，
  如果是 2，事务结束，执行 commit
  如果是 3，则执行下面这两个语句：
*/
insert ignore into friend(friend_1_id, friend_2_id) values(B,A);
commit;


这里user_id和like_id组成了唯一索引
如果A、B同时互相关注。

假设A先执行，插入的记录是(A, B, 1)，A事务还没提交。这时候B关注A，由于B > A，插入的记录是(A, B, 2)，但是B会被A阻塞，等待A执行。
A继续执行，查询出relation_ship的值为1，直接提交事务。锁释放后轮到B执行，由于A,B冲突，所以会在原有记录上进行更新，
然后插入一条好友记录。








on duplicate key update
> insert into table_name values(...) on duplicate key update set xxx
> 在插入记录的时候，如果发现记录重复的话则执行后面的update操作。
> 注：该语句执行的时候会对记录上锁。



create table account(
​    id int primary key auto_increment,
​    user_id int not null,
​    balance int unsigned not null
)engine=innodb;

```sql
insert into account values(1, 100, 1000), (2, 101, 500), (3, 102, 100);
```



A给B转账，B同时也给A转账，同时可能有其他人给A转账。

那么这会发生什么情况?

```sql
-- A给B转账100元
begin;
update account set balance += 100 where user_id = 2;
update account set balance -= 100 where user_id = 1;
commit;

-- B给A转账50元
begin;
update account set balance += 50 where user_id = 1;
update account set balance -= 50 where user+id = 2;
commit;
```

假设第一个事务先执行，在更新user_id等于2的时候，会加行锁。这时候第二个事务开始执行，在更新用户1的时候也会加行锁。

接着第一个事务继续往下执行，它会等待第二个事务释放锁用户1的锁，而事务2又在等待事务1释放用户2的锁，因此造成了死锁。



解决的办法：由应用程序来控制加锁和解锁，在转账的时候，对用户id和要转账的用户id进行升序排序，然后对用户1和用户2一起加锁。

例如将用户1和用户2的ID组成的key作为加锁的key来进行加锁。

这样的话俩个用户没办法同时一起转账，第二个用户必须等第一个用户释放锁了才能转账。



如果其他用户向A转账，它们之间是彼此没有冲突的。