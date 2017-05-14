## 子查询

### 1. where子查询
内层查询结果作为外层SQL语句的数据源，该内层查询语句就叫做子查询，
因为子SQL语句在where关键字后面，所以就将其叫做where子查询。

例如：
```sql
select * from goods where goods_id in (select max(goods_id) from goods group by cat_id);
# 查询goods_id最大为最新商品
```

### 2. from子查询
内层查询结果作为外层查询结果的数据源，就叫做from子查询，该数据源必须使用as关键字来命名，否则会报错。

例如：
```sql
select * from (
	select * from goods order by cat_id asc,goods_id desc
) as temp group by cat_id;

# 查询每个栏目下最新的产品(goods_id最大为最新)
# 1.子SQL语句使用cat_id升序排序，再用goods_id降序排序
# 2.外层SQL语句再按照cat_id分组排序即可
```

### 3. exists子查询
> 语法：exists(express)

express可以是表达式也能是SQL语句，存在某个值就会返回true，例如：
```sql
select * from pro_student 
	where exists(select * from pro_student where c_id is not null)
# exists子句的SQL语句只要有结果，外层SQL语句将执行成功。
```

## union查询
union是把俩条或多条SQL语句的查询结果合并成一个结果集，union语句必须满足一点的是：多条SQL语句取出的字段数量必须一致，字段名不用一致，否则报错。

> 语法： select ... union [options] select ...

options是选项，有：
- distinct，结果集中返回的记录是唯一的，会合并相同记录，不建议使用，极其消耗性能
- all：返回所有SQL语句所匹配到的结果集




## join查询
```sql
create table our(
	id int primary key auto_increment,
	name varchar(20),
	over varchar(20) default null
)engine myisam charset utf8;

create table friends(
	id int primary key auto_increment,
	name varchar(30),
	over varchar(20) default null
)engine myisam charset utf8;

insert into our values(default, '孙悟空', '斗战胜佛');
insert into our values(default, '猪八戒', '净坛使者');
insert into our values(default, '唐僧', '金蝉子');
insert into our values(default, '沙悟净', '金身罗汉');

insert into friends values(default, '牛魔王', '被降服');
insert into friends values(default, '蛟魔王', '被降服');
insert into friends values(default, '孙悟空', '成佛');
insert into friends values(default, '大力王', '被降服');

```

### 1. inner join
内连接inner join基于连接条件将俩张表（A和B）的列组合在一起，产生新的结果集。

内连接的本质是交集，即取得A、B俩个表中数据交集的部分，也就是同时存在于俩个表中的记录。

### 2. left join
A、B俩表进行左外连接，将会以A表作为基础去匹配B表，结果集不仅包括匹配的交集数据，还有以A表作为基础的非交集数据（右边为null）

### 3. right join
与left join相似，不同的是将以B表作为基础表去匹配A表，如果条件不匹配，左边的记录是null，右边的记录不变。


### 4. full join
full join是左连接和右连接的一个合集，首先能查询出A表，B表中所有交集的部分，同时A表与B表未交集的数据也会被查询出来，没有数据部分的表字段将会是null。

可以理解成左连接查询出来记录与右连接查询出来记录的集合。
注：MySQL本身不支持full join查询。

### union实现full join
```sql
select a.name, a.over, b.over from our a 
	left join friends b on a.name=b.name
union all
select b.name, b.over, a.over from our a 
	right join friends b on a.name=b.name
```

### 5. cross
Cross join是交叉连接，又被称为笛卡尔积或叉乘，如果A和B是俩个集合，它们的交叉连接就记为A*B。

A表的每条记录与B表的每条记录进行匹配所产生的结果集，例如A表有4条记录，B表有5条记录，cross join的结果集将会产生20条记录。

