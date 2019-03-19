### 数据页

InnoDB的数据是按数据页为单位来读写的。当需要读一条记录的时候并不是将这个记录本身从磁盘读出来，而是以页为单位，将其整体读取内存。在InnoDB中，每个数据页的大小默认是16kb。

note：数据页我初步认为是b+树的节点的key，后续再查阅相应的资料。



### change buffer

change buffer用于解决更新数据时，随机读取磁盘数据页造成的IO开销。如果更新一条记录就需要去磁盘中读取对应的数据页然后更新，这种随机IO的开销过大了。



当需要更新一个数据页时，如果数据页在内存中就直接更新，而如果数据页不在内存中，在不影响数据一致性的前提下，InnoDB会将这些更新操作缓存在change buffer中，这样就不需要从磁盘中读取这个数据页了。

当下次需要访问这个数据页时，将数据页读入内存，然后执行change buffer中与这个页有关的操作。通过这种方式就能保证这个数据逻辑的正确性。

同时数据页在应用change buffer后，也是会写redo log的，redo log会记录数据的变动和change buffer的变更。



将change buffer中的操作应用到原数据页的过程，称为merge。除了访问数据页会触发merge外，系统后台线程也会定时merge，数据库正常关闭的过程中也会执行merge操作。



```ini
innodb_change_buffer_max_size
```

该参数用于设置change buffer的大小，change buffer用的是buffer pool里的内存，如果设置50，表示使用innodb pool大小的50%。



### example

```sql
create table t(
	id int primary key,
    k int,
    index(k)
);
insert into t values(100, 1),(200, 2),(300, 3),(500, 5),(600, 6);
-- 创建表并插入基本数据
```

```sql
insert into t(id, k) value(400, 4)
```

这里通过执行一些SQL语句，来说明下change buffer的执行流程。

比如执行上面这条语句会有俩种情况：

- 记录要更新的目标页在内存中

  对于普通索引来说，会在数据页中找到3和5之间的位置，然后插入。

- 记录要更新的目标页不在内存中

  InnoDB会将更新记录在change buffer中，语句执行结束。

```sql
select * from t where k = 4;
```

在查询数据时也分为俩中情况：

- 目标页在内存中

  从内存中读取记录，直接返回数据

- 目标页不在内存中

  如果目标页不在内存中，那么将会把change buffer中的更新应用到目标页中，这时数据页的数据就是最新的了。



### 应用场景

merge的时候才是真正对数据页进行更新的时候，所以一个数据页在merge之前，change buffer缓存的更新越多，收益越高。

因此change buffer适合写多读少的业务场景，页面在写完以后马上被访问到的概率比较小，change buffer的使用效果最好。例如账单类、日志类的系统。



在实践中，如果所有的更新后面，都马上伴随着对这个记录的查询，那么就应该关闭change buffer。而在其他情况下，change buffer能够提升性能。

注：change buffer只适合普通索引，而不适用于唯一索引。



### redo log

这里主要说明更新记录和查询记录是，change buffer与redo log都做了什么事情。

```sql
insert into t(id,k) values(id1, k1),(id2, k2);
```

继续使用上面example的表结构，比如要插入上面这条语句。我们假设在k索引树中，查找到位置后，k1所在的数据页在内存中，k2所在的数据页不在内存中。

那么change buffer与redo log的变化如下图所示：

![](https://static001.geekbang.org/resource/image/98/a3/980a2b786f0ea7adabef2e64fb4c4ca3.png)

该更新会涉及4个部分：InnoDB buffer poll、redo log、系统表空间、数据表空间，更新时做了如下变动：

- k1的page1数据页在内存中，直接更新内存
- k2的page2数据页不在内存中，就在内存的change buffer区域，记录下要往page2插入一行记录的信息
- redo log日志分别会记录page1的变动和change buffer的变动（新增了一个change buffer item）

执行到这里事务就完成了，可以看到执行一条更新语句就是写了俩处内存，然后写了一次redo log日志，俩次操作合并成一次操作，而且是顺序写的。



```sql
select * from t where k in(k1, k2);
```

如果要查询刚插入的k1和k2的记录，会有以下操作：

- 对于k1

  k1对应的数据页在内存中，因此直接读取

- 对于k2

  k2的数据页不再内存中，需要从表文件中读取数据页，然后执行change buffer中对应数据页的变更操作，得到新版数据页。

  最后写redo log，这个redo log包含数据的变更和change buffer的变更。



对于redo log和change buffer，它们的优化方向不一样。redo log主要节省的是随机写磁盘的IO消耗，转成顺序写，而change buffer主要是节省了随机读取磁盘的IO消耗。











