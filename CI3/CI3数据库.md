## CI_DB对象
该对象实现了数据库的操作，对于该类的方法可以按照实现上分为俩种：
- 实现DAO接口的方法，提供了一套以SQL语句去操作数据库的API接口。

- 实现查询构造器的方法，提供了一套可以用方法来构建查询语句的方法。

### 1. CI_DB_mysqlimysqli_driver
在CI中，CI_Model::db 对象是负责提供API接口操作数据库的对象。

通过输出CI_Model::db对象，发现它的类名是CI_DB_mysqlimysqli_driver，在框架中它主要做俩件事情，
- 实现DAO对象，提供访问数据库的API接口
DB::query()，该方法执行一条SQL语句去操作数据库，如果是select返回结果集对象，如果是写入操作则返回true或false。
query()查询主要是获取结果集，结果集将由CI_DB_result类操作,
除了上述的方法和类，DB对象还提供了一些辅助方法，具体看手册。

- 实现QueryBuilder，提供查询构造器的接口，QueryBuilder实际上是利用DAO对象接口的一种实现。



### 2. CI_DB_mysqli_result
CI实现的结果集对象，存储了从数据库中查询的数据
麻烦的是查询构造器只提供了基础的方法，查询出的数据都是结果集对象，操作数据需要result类提供的API来进行操作。