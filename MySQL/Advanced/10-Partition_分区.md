## 表分区
表分区主要针对数据库文件过大这种情况，
例如单个.myd文件超过10G，这种情况下读取效率是非常低下的，分表技术便是在这种情况下发生。

### 1. 水平切割
水平切割主要是在业务角度上实现。

例如：
```
1 % 5 = 1
2 % 5 = 2
3 % 5 = 3
4 % 5 = 4
5 % 5 = 0
# toppic表现在数据过大，将其拆分成tippic0-toppic45张表，通过id%5得到的余数来插入或查询某张表。
```

### 2. MySQL Partition分区技术
mysql能根据指定的规则将数据放在不同的表文件上面，相当于将文件拆成若干小块，但是对于客户端显示的仍然是一张表。

常用规则：
根据某列的范围来分区，也可以根据某列的散点值来分区。

按列的范围分区：
```
uid[1,10)
# user partition u0

uid[10,20)
# user partition u1

uid[20,30)
# user partition u2
```

列范围分区案例：
```sql
create table topic(
	tid int primary key auto_increment,
	title char(20) not null default ''
)engine myisam charset utf8
partition by range(tid)(
	partition t0 values less than(10),
	partition t1 values less than(20),
	partition t2 values less than(MAX)
);

partition by range(tid)
# 根据tid做范围分区

ls /var/lib/mysql
# 该目录存放mysql数据文件

# 测试：通过插入数据再来观察表的数据库文件

# 对于myisam数据库引擎，一张表是由3个文件组成
# frm(表结构)	  myd(存放表数据)		.myi(表索引文件)
```

根据列的散点值来分区：
```
# 例如有一张用户表，想按照地区字段来分区

create table area(
	aid int, zone char(10)
)engine myisam charset utf8;

insert into area values(1,'bz'),(2,'sh'),(3,'gd');

create table user(
	uid int,
	uname char(10),
	aid int
)engine myisam charset utf8
partition by list(aid)(
	partition bj values(1),
	partition sh values(2),
	partition gd values(3)
);
```
注1：分区的时候，列的字段不能为null，mysql会将null理解为0处理
注2：分区能按照表达式来进行分隔，但是表达式不如固定值，比如,用 partition by range (year(regtime)) 可以按注册年份来分区。
