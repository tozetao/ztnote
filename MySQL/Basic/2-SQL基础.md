

## create
create关键字可以创建数据库，也能创建表。

### 1. 创建数据库
> create database_name [charset x],[collate x]

- database_name：数据库名字
- charset：设置数据库编码
- collate：设置校对集

数据库对外体现是一个文件夹，表对外体现是文件。

### 2. 表的创建
```sql
create table table_name(
	column1 type,
	column2 type,...
)[charset xx,engine=xxx];
```
table_name是数据库表名，括号中是字段的定义，小括号后面跟表的设置信息。
- engine，设置表的存储引擎
- charset，设置表的编码
- auto_increment，自增长的起始id

数据库表在操作系统中就是文件，只不过这些文件存储数据的方式不太一样。不同存储引擎的表结构是不一样的，例如：
- myisam引擎使用3个文件来存取数据，分别是结构文件、索引文件和数据文件
- innodb引擎使用1个表结构文件，索引和数据是存储在ibdata1文件中，该文件一般位于根目录/data/ibdata1

相关创建语句：
if not exists：如果不存在就执行。
if exists：如果存在就执行。

## alter
alter关键字能够更改数据库或数据库表创建的信息。

### 1. 更改数据库
alter database database_name [库选项]
- database_name，数据库表名
- 库选项，包括编码、字符集等

### 2. 更改表
alter table table_name [add/modify/drop] [column] column_name [type] [position]
- table_name，表名
- add，添加字段
- modify，修改字段
- drop，删除字段
- column_name，字段名
- type，字段类型
- position，字段位置

案例：
```sql
alter table test add column age int;
# 添加一个字段

alter table test modify first varchar(10) after content;
# 更改字段类型

alter table user change first second varchar(10);
# 修改字段名字

alter table user drop name;
# 删除字段
```



## drop
drop database，删除数据库
drop table_name，删除表

## truncate
truncate table tab_name.
清空表数据

## show
show关键字能够显示系统信息，例如：
- show databases，查看所有数据库
- show tables，查看所有表格
- show create database/table，查看表或数据库的创建过程

也能查看MySQL配置信息，例如：

show table status	查看表的详细信息
show table status where name = ''
\G，可以竖着显示数据



## insert
语法：insert into 表名(字段列表) values(值列表)
insert 所有列
insert 指定列
字段与值俩者的数据类型要一致，除了整型可以不加引号之外，其他的都要加上引号。

注1：字段列表的字段数要与值列表的字段数一致。

注2：可以在value后面加,分隔，插入多条数据。
	例：insert into userinfo values(1,'zetao',25),(2,'yuxuan',23);

## update
update 表名 set 字段=值 [where 条件]

## delete
delete from 表名 [where条件]
			例：delete from userinfo where id=10;


## select
MySQL中最重要的关键字，用于查询数据，配合其他关键字能做到很多种查询方式。

### 1. 语法
select 表达式 [from] [where] [group by] [having] [order by] [limit]

### 2. 表达式
表达式可以是字段、运算过程、比较或函数，例如：
```sql
select 20*20;
select now();
select 1>2;
select name from student;
```
### 2. from
from子句指定查询的数据源，可以是一张或多张表，也能是查询结果。
```sql
select * from tab1, tab2
# 笛卡尔积，查询结果是俩个表数据的笛卡尔积

select * from (select * from class) as c;
```

### 3. as
对某个数据源所起的简单的用来描述的名字，别名能让你直接引用表。
```sql
select s.name from student s,class c;
# 引用
```
数据源是子查询，必须使用别名对数据源进行描述，否则会报错
```sql
select * from (select * from student);
# 错误的
select * from (select * from student) as s
# 正确的
```

### 4. where子句
用于筛选数据，where是针对数据源来做对记录进行筛选的，
where关键字后面跟条件表达式。

### 比较运算
比较运算符有：
- \>
- <
- =
- \>=
- <=
- !=
- between and：区间的判断
- is [not] null：null的判断比较特殊，要用is关键字判断，因为null参与任何运算它的结果都是null。

### 逻辑比较
- or：逻辑或比较
- and：逻辑与比较

### in
in关键字主要用于判断字段是否在某个集合中，即在多个值中是否匹配，in可以配合not关键字使用。


### group by
group by用于分组，即将查询出来的记录按照某个字段的值进行归类，分组后会将数据进行合并，默认显示每组的第一条记录，可以使用多个字段进行分组。

### 统计函数
对查询的数据进行数学方面的统计，配合group by使用，可以对每组信息进行分组的统计计算。

- count(column_name)，统计记录的个数，不统计null，如果某个字段的值是null，会显示0
- max：统计记录中字段最大值。
- min：与max相同，统计记录中字段的最小值。
- avg：求记录的平均数
- sum：求记录的和

### having子句
having子句也是用于条件判断，与where不同的是，它是针对已经查询出来的数据，存在于内存中的数据进行条件判断，例如：
```sql

 
select * from goods where cat_id=3
# 假设存储引擎是myisam，goods表的数据存储文件是goods.MYD
# 在查询数据的时候就会读取这个文件，where所查询的数据源就是指该文件

select name,sum(score<60) as s,avg(score) from result group by name having s>=2;
# 分组查询后数据是在内存中的，这时候having就能对记录再做条件判断
```
select子句执行顺序：where > group by > having > order by

### order by
order by是使用一个或多个字段进行排序，asc默认的，表示升序排序，desc降序排序，比较的依据是校对集。

### limit
设置取出数据的偏移量和返回数据的条数。

> 语法：limit offset,number or limit number

- offset：偏移量，偏移多少条记录。
- number：取出条目



## replace into insert ignore。