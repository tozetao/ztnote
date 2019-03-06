有空要过下MySQL官方手册对于InnoDB数据引擎的介绍，查看有什么不同的地方。



https://dev.mysql.com/doc/refman/5.7/en/innodb-locks-set.html

https://dev.mysql.com/doc/refman/5.7/en/mysql-acid.html



问题1

一般在做备份的时候会在从库上执行，在用 --single-transaction方法做逻辑备份过程中，如果主库上的一个小表做了一个DDL，比如给表加上了一个列，这时候在备库上会看到什么效果?



从库开启了一个一致性视图的事务，首先会对表加上MDL读锁，主库执行了一个DDL语句，在修改表结构时需要得到MDL写锁。

从库在执行主库的binlog日志时，应该会被阻塞住，只有从库的一致性视图事务提交后，主库的binlog日志才会执行。