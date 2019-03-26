join查询的俩种算法。



Test SQL

```sql
CREATE TABLE `t2` (
  `id` int(11) NOT NULL,
  `a` int(11) DEFAULT NULL,
  `b` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `a` (`a`)
) ENGINE=InnoDB;

drop procedure idata;
delimiter ;;
create procedure idata()
begin
  declare i int;
  set i=1;
  while(i<=1000)do
    insert into t2 values(i, i, i);
    set i=i+1;
  end while;
end;;
delimiter ;
call idata();

create table t1 like t2;
insert into t1 (select * from t2 where id<=100)

```







### Index Nested-Loop Join

直译索引嵌套循环连接，在一个连接查询中，主表被称为驱动表，从表被称为被驱动表，如果连接条件有使用到索引，这种连接查询就被称为Index Nested-Loop Join。比如：

```sql
select * from t1 straight_join t2 on (t1.a = t2.a);
```

在这条SQL中，使用straight_join以固定的连接方式查询，t1是驱动表，t2是被驱动表。执行流程如下：

- 先从t1表中取出一行记录R
- 从R中取出a字段，到t2表中进行查询
- 取出表t2中满足条件的行，跟R组成一行，作为结果集的一部分
- 重复执行上面步骤，直到t1表的末尾循环结束

整个查询中，在t1中扫描了100行，而通过t1表中的每条记录去t2表中查询时，走的是树搜索过程。t1和t2表构建的数据都是一对一的，因此每次搜索的过程都只扫描了一行数据，也就是总共扫描了100行，整个执行流程扫描了200行记录。

这个执行流程扫描记录的时间复杂度近似$N + N * 2 * logM$，N是驱动表的记录行数，M是被驱动表的记录行数。因为查询t2表时，搜索了一次普通索引树，查询数据时又搜索了一次主键索引，所以是$2 * logM$，而t1表中有N条记录，所以时间复杂度是近似于$N * 2 * logM$。

总结：使用join查询时，要选择小表作为驱动表。



### Block Nested-Loop Join

如果被驱动表没有使用到索引，就会使用该算法。比如：

```sql
select * from t1 straight_join t2 on t1.a = t2.a;
```

如果join buffer能够放入驱动表中的所有记录，该SQL的执行流程如下：

- 从t1表中取出所有数据，放入join buffer中
- 扫描t2表，取出t2表中每一条记录然后跟join buffer中的记录进行比较，满足join条件的，就作为结果集的一部分返回。

explain分析这条语句，可以看到Using join buffer。

整个流程扫描了t1和t2表，所以扫描行数时1100行。而join buffer中的记录是无需的，所有t2表的每一条记录都需要比较100次，也就是进行了100 * 1000次判断。假设小表的行数是N，大表的行数是M，则扫描行数为$N + M$，比较次数为$N * M$。

如果buffer中放不下驱动表的数据，那么会分多次放入到内存中，那么执行流程就是：

- 从t1表中取出一部分数据，放入join buffer中
- 扫描t2表所有记录，跟buffer中的记录进行比较，满足join条件的就作为结果集返回
- 清空buffer，重复执行步骤1、2，直到t1表中的所有数据都读取完毕

所谓的Block就是指分批次放入join buffer中，这个执行流程，假设t1表分K个批次，记录数是N，t2表记录数是M，那么扫描行数为$N + K * M$，内存判断次数为$N * M$。

扫描行数受到K和N的影响，也就是join buffer的大小和驱动表的记录数大小。



```ini
join_buffer_size
```

join查询缓存的大小，默认是256kb。





总结：

- 如果可以使用Index Nested-Loop Join算法，使用join查询是没问题的。如果SQL语句使用的是Block Nested-Loop Join算法，建议进行优化.

- 无论使用哪种算法，都要使用数据量小的表来作为主驱动表。

这里的小表指的是放入join buffer中的数据量小，才是小表。因此join查询中的多个表，按照各自的条件过滤后，计算参与join的各个字段的总数据量，数据量小的表就是小表了。





优化

Index Nested-Loop Join简称INL，Block Nested-Loop Join简称BNL。

