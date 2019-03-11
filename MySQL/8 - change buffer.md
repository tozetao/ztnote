在读取数据的时候，InnoDB的是按数据页为单位来读写的，当读取一条记录时并不是将这个记录本身从磁盘读取出来，而是以页为单位，将其整体读入内存。

InnoDB中每个数据页的大小默认是16kb。



将数据从磁盘读取内存涉及随机IO的访问，将数据从内存中随机写入到磁盘上，这俩种操作的成本都很高。因此才有redo log和change buffer的存在。



什么是数据页？



### change buffer

change buffer用于解决更新数据时，随机读取磁盘数据页造成的IO开销。

当需要更新一个数据页时，如果数据页在内存中就直接更新，而如果数据页不在内存中，在不影响数据一致性的前提下，InnoDB会将这些更新操作缓存在change buffer中，这样就不需要从磁盘中读取这个数据页去进行更新了。

当下次需要访问这个数据页时，将数据页读入内存，然后执行change buffer中与这个页有关的操作。通过这种方式就能保证这个数据逻辑的正确性。

- merge

  将change buffer中的操作应用到原数据页的过程，称为merge。除了访问数据页会触发merge外，系统后台线程也会定时merge，数据库正常关闭的过程中也会执行merge操作。

简单的说，change buffer通过缓存数据页的更新操作，以此达到减少随机读取磁盘的IO开销，而merge过程则保证了查询时数据的正确性。



example：举例说明change buffer是如何运行的

```sql
create table t(
	id int primary key,
    k int,
    index(k)
);
```

```sql
insert into t value(4, 400)
```

假设现在执行一条insert语句，如果要更新的这条记录的目标页在内存中，对于普通索引来说，找到3和5之间的位置，插入这个值，语句执行完毕；如果目标页在内存中，对于普通索引则是将更新记录在change buffer中就执行完毕了。



change buffer只限于普通索引，而不适用于唯一索引。





### redo log

```sql
insert into t(id,k) values(id1, k1),(id2, k2);
```

假设在k索引树中，查找到位置后，k1所在





change buffer可以与redo log结合起来看。

redo log是物理日志，记录的是数据页的更新（update、insert、delete）。



更新时并不会直接写入磁盘，而是写入redo log，当系统空闲时，或者redo log空间不够用时，InnoDB才会将日志写入到磁盘中。

redo log是减少了磁盘的随机写入，将随机写入变为顺序写入，而change buffer是减少了随机读取磁盘数据页，这俩个优化的方向是不一样的。



redo log主要节省的是随机写磁盘的IO消耗，转成顺序写，而change buffer主要是节省了随机读取磁盘的IO消耗。







应用场景

写多读少的业务场景，页面在写完以后马上被访问到的概率比较小，change buffer的使用效果最好。例如账单类、日志类的系统。





