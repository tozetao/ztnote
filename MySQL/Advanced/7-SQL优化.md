## SQL优化思路
1条SQL语句慢在俩个方面：
1. 等待时间久，例如可能程序被锁了，队列处理等待中。
2. 执行时间就，扫描的记录数过多，或者取出的数据量过大


SQL语句主要从2个方面进行重构：
- 切分查询，操作过多的行分解成多次
- 分解查询，多表联查拆解成单表


## explain命令
explain命令用于解析SQL语句，了解该SQL语句的执行计划，例如是否使用全表扫描，利用索引等。
在工作中，捕捉SQL语句执行的性能问题最常见的是打开慢查询，定位执行有效率的SQL语句，再用explain分析语句。

案例表：
```sql
create table goods(
	id int primary key auto_increment,
	name varchar(10) not null,
	price int not null,
	c_id int not null default 0
)engine=myisam charset utf8;

insert into goods values(default, 'txu1001', 100, 1);
insert into goods values(default, 'txu1001', 100, 1);
insert into goods values(default, 'txu1001', 100, 1);

create table category(
	id int primary key auto_increment,
	c_name varchar(10)
)engine=myisam charset utf8;
insert into category values(default, 'T恤');


explain select g.name,g.price,c.c_name from goods g 
	inner join category g on g.c_id = c.id

```
explain执行计划参数

```
id
# 包含一组数组，表示SQL子句或操作表的顺序
# id相同的情况下，由上到下执行，如果是子查询，id序号会递增，值越大越优先执行。


select_type
# 表示每个select子句的类型
# simple：最简单的查询，不包含子查询或UNION
# primary key：查询中包含任何复杂的子查询，最外层的查询被标记为primary key

# subquery（非from型子查询）
# 在select或where列表中包含了子查询，该子查询被标记为subquery
# explain select * from goods where c_id = (select id from category where id = 1);

# derived（from型子查询）
# 衍生，服务器内部称为派生表，因为是从子查询中派生出来的。
# explain select name from (select * from goods where id = 3)\G; 

# union
# 若第二个select出现在union之后，则被标记为union
# explain select * from goods where id=1 union select * from goods\G;

# union result
# 从union表获取结果的SELECT被标记为union result

# subquery和union还能被分为dependent和uncacheable
# dependent意味着select依赖于外层查询中发现的数据，uncacheable意味着select中某些特性阻止记过被缓存在一个item_cache中

table
# 表示这条SQL语句执行的表名
# 可能是实际表名、别名、derived派生表（from型子查询）、null（直接计算）。

possible_keys
# 可能使用到的索引，mysql估计可能用到的索引，但是最终结果只会有1个被使用到。

key
# 最终使用到的索引	

key_len
# 使用的索引最大长度，单位字节。
# 根据表使用的字符集和索引字段的类型来计算，例如utf8字符集，index(name,age)，name是char(4)，age是int，长度是4*3 + 4 = 16

type
# 表示MYSQL在表中查找所需记录的方式，很重要，是分析“查询数据过程”的重要依据。
# 通过type的类型可以确定SQL语句执行的量级

# all，全表扫描，相当于data_all

# index，扫描所有索引节点，一般是能利用上索引，但是必须全索引扫描
# 例如利用索引来排序，explain select goods_id from goods order by goods_id desc\G;
# 或者条件是范围性查询，explain select goods_id from godos where goods_id=1 or goods_id+1 > 20;

# range，根据索引做范围的扫描，
# 例如：select godos_id from goods where goods_id>1;

# ref，引用的意思，通过索引引用某些行记录，
# 例如：select goods_name,goods_id from goods where cat_id=1;
	
# eq_ref，通过索引直接引用一行记录
# explain select goods_id,shop_price from ecs_goods inner join ecs_category using(cat_id) where goods_id>25\G;
	
# const/system/null，常量级，最优的查询效果
explain select count(*) from ecs_shop \G;


Extra
# 包含不适合在其他列中显示但十分重要的额外信息
# using index，该值表示在select中使用了索引覆盖

# using where，使用查询条件
# using temporary，建立临时表，常用于排序或分组
# using filesort，MySQL中无法利用索引完成的排序操作称为“文件排序”
# ...

ref
# 指表引用了哪些列，常见于多表查询。
	
rows
# 估算扫描的行数	
```

## in型子查询 ##
SQL语句：
```sql
create table s_goods(
	goods_id int primary key auto_increment,
	goods_name varchar(10) not null,
	cat_id int not null,
	goods_price int not null,
	key(cat_id)
)engine myisam charset utf8;

insert into s_goods
	values(default, 'dfsfdfsdf',0, 100);
insert into s_goods
	values(default, 'dfsfdfsdf',1, 100);
insert into s_goods
	values(default, 'dfsfdfsdf',6, 100);

create table s_category(
	cat_id int primary key auto_increment,
	cat_name varchar(10),
	parent_id int
)engine myisam charset utf8;

insert into s_category
	values(default, '手机类型',0);
insert into s_category
	values(default, 'CDMA手机',1);
insert into s_category
	values(default, 'GSM手机',1);
insert into s_category
	values(default, '3G手机',1);
insert into s_category
	values(default, '双横手机',1);

insert into s_category
	values(default, '手机配件',0);
insert into s_category
	values(default, '充电器',6);
insert into s_category
	values(default, '耳机',6);
insert into s_category
	values(default, '电池',6);
insert into s_category
	values(default, '读卡器',6);
insert into s_category
	values(default, '充值卡',0);
insert into s_category
	values(default, '充移动值卡',12);
insert into s_category
	values(default, '联通充值卡',12);
```
案例分析：
```sql

explain select goods_id, goods_name from s_goods
	where cat_id in (select cat_id from s_category where parent_id=6)\G;
# 上面这条SQL语句中，goods表的cat_id索引并没有发挥效果，这是因为MYSQL对这条SQL语句做了优化，修改成exists的执行效果，
# 执行时不是先执行子查询，而是逐行获取goods表的记录与子查询的记录进行对比，
# 可以看到外层goods表示全表扫描，这种语句在外层表数据量很大的情况下，效率是很低下的。
# 这是MYSQL in型子查询的陷阱，要注意。

explain select goods_id, goods_name from s_goods
	where cat_id in (7,8,9)
# 如果in修改成固定的值，则索引会生效。
```
改进方案：
修改成连接查询，或者是修改成俩条SQL语句。
```sql
explain select goods_id,goods_name from goods g
	inner join (select cat_id from category where parent_id=6) c
	on g.cat_id = c.cat_id \G;
# 改进成连接查询，效率就上来了

explain select goods_id, goods_name from goods
	inner join category on goods.cat_id = category.cat_id
	where category.parent_id=6\G；
# 这样修改似乎效率不好。

explain select * from goods where id>1;
# 我在测试索引是否生效的时候发现如果索引条件的值比较小，mysql会自动优化，走全表查询的，例如上面的SQL语句。
```



## exists的应用
需求：查询所有有商品的类目

分析：
1. 查询某个顶级类目下所有子类目的商品
2. 查询所有有商品的栏目

select c.cat_id,cat_name from category
	where exists (select * from goods as g where g.cat_id = c.cat_id);

## SQL优化技巧
### 1. max min的优化

首先在myisam表中，取min、max，速度是非常快的。

select min(id) from lx_com where pid=10001
这个语句会进行全表扫描。
因为mysql不知道pid10001的记录的最小id的记录，所以需要全表扫描出来pid10001的所有的记录，再筛选出最小的记录。

因为取出来pid=10001的记录，mysql无法判断哪条记录里面的id是最小的，所以需要全表扫描出来，最后进行计算来判断。

lx_area
地区表
id是主键
pid没有索引。

因为id是索引，那么记录是有序的，所以沿着id的索引方向走，那么第一个pid=10001的记录即是最小值。

select id from it_area use index(primary) where pid=1087;
-- 强制沿着主键来查询记录，这样查询出来的数据，是有序的。
沿着索引找的话，也是显示全表扫描，但是使用了limit的话，在截取到第一条记录的情况下就会停止查询了。


### 2. count优化

count在myisam下的速度是非常快的，innodb的count则比较慢。
因为myisam的系统缓存表会缓存count这些数据的。

select count(*) from table_name
但是在加上查询条件的情况下，count()的速度也会降下来的。


select count(*) from lx_com 
select count(*) from lx_com where id < 100
用总数 - 计算出来的数据，这样的数量就是正确的。

group by尽量用来统计，而不要用来筛选数据。
另外group by尽量用索引来避免临时表和文件排序。

以A/B表连接查询为例，如果主要查询A表的列，那么group by order by的列尽量相同，而且列应该显示的声明为A的列。
这样速度会更快。


### 3. union优化
union all不过滤，效率会提高，因为union去重的代价非常高，建议放在程序中去重。



### 4. 巧用变量

### 5. 注意点
注1：内层from语句查询到的临时表是没有索引表的，所以from的返回内容要尽可能的少。
注2：sql语句要尽量扫描多的行，尽量少用temporary、filesort，这是sql语句的优化点。

对于MYSQL的优化，不查，少查：能缓存的缓存，能静态化的静态化，快查：从表设计、索引优化，sql语句上来优化，

整个网站的优化要从服务器的架构上，缓存方面的设计入手，再考虑MYSQL表的设计，索引优化，范式设计，最后才是SQL的优化。。