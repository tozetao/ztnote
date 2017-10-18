
## JOIN技巧
说明：接上节的SQL查询。

### 1. 更新使用过滤条件中包括自身的表
需求：将同时存在于our表和friends表中的记录的our表over字段更新为'齐天大圣'

```sql
update our set over='qitiandasheng' 
	where our.name in (
	select name from our o inner join friends f on o.name = f.name
)
# 报错：You can't specify target table 'our' for update in FROM clause
# 更新的表是不能出现在from从句中的，MySQL不支持这种更新方式


update our a join (
	select b.name from our a join friends b on a.name=b.name
) b on a.name=b.name set a.over='qitiandasheng';


update our a inner join friends b on a.name = b.name set a.over='齐天大圣';
```

### 2. 使用join优化子查询
```sql
select a.name, a.over, 
	(select over from friends where a.name=b.name) as over2 from our a
# 这条SQL语句不仅查询our表，同时在查询our表的每条记录时会用这条记录去查询friends表中的所有记录，这样效率是极差的。

select a.name, a.over, b.over as over2 from our a left join friends b on a.name=b.name
# 改进

```

### 3. 使用join优化聚合子查询
引入一张新表
```sql
create table kills_record(
	id int primary key auto_increment,
	user_id int,
	timestr timestamp,
	kills int
)

insert into kills_record values(default, 2, '2017-04-07 15:52:51', 10);
insert into kills_record values(default, 2, '2016-12-17 15:52:51', 8);
insert into kills_record values(default, 2, '2017-06-27 15:52:51', 12);
insert into kills_record values(default, 4, '2017-01-17 15:52:51', 35);
insert into kills_record values(default, 1, '2017-02-05 15:52:51', 12);
insert into kills_record values(default, 3, '2017-03-04 15:52:51', 35);
insert into kills_record values(default, 4, '2017-04-03 15:52:51', 3);
insert into kills_record values(default, 3, '2017-01-02 15:52:51', 5);
insert into kills_record values(default, 3, '2017-04-01 15:52:51', 8);
insert into kills_record values(default, 3, '2017-04-10 15:52:51', 1);
```

需求：上述有一张打怪表，要求查询出our表中每个人打怪最多的日期
```sql
# 解法1
select o.*, k.* from our o inner join kills_record k
	on o.id=k.user_id where k.kills = 
	(
		select max(kills) from kills_record where user_id = k.user_id
	)

# 解法1的改进
# 缺陷是where条件的每条记录会都在子查询中遍历一次查询
SELECT a.use_name, b.kills, b.killtime 
	FROM im_user AS a
	INNER JOIN im_kill AS b ON a.id = b.user_id
	WHERE b.kills =
    (
    	SELECT kills FROM im_kill AS c WHERE c.user_id=b.user_id ORDER BY kills DESC  LIMIT 1
    );


# 解法2
# 子句是查询打怪数最大的id，对kills_record表的user_id升序kills倒叙并分组
select o.*, k.* from our o inner join kills_record k
	on o.id=k.user_id where k.id in 
(
	select id from (select id, user_id, kills from kills_record order by user_id asc,kills desc) as temp group by temp.user_id
)


# 解法3
select o.name, k1.timestr, k1.kills, k2.kills from our o
	join kills_record k1 on o.id=k1.user_id
	join kills_record k2 on k1.user_id=k2.user_id
	group by o.name, k1.timestr, k1.kills
	having k1.kills = max(k2.kills)

# 关联kills_record表俩次，再使用name、timestr、kills字段分组，最后再用having做临时的内存判断
# 第二次的kills_record表关联是用于在内存中做临时计算，算组分组后kills字段的最大值


技巧1：使用order by或group by作用于表中的多个字段，达到筛选排序的作用
技巧2：可以join连接查询自身，将查询出来的记录使用having条件比较。
```

### 4. 分组选择
需求：查询出每个分类中最受欢迎的前几条记录

```sql
# 查询出某个用户打怪最多的前俩条记录
select * from our o
	join kills_record k on o.id=k.user_id
	where o.name='sunwukong' order by k.kills desc limit 2


# 现在的需求是查询出每个用户打怪最多的前俩条记录
select user_id, timestr, kills, (
		select count(*) from kills_record b where b.user_id = a.user_id and a.kills <= b.kills
	) as nums from kills_record a group by user_id, timestr, kills
# 子语句的解析
# 从a表中根据用户id，时间戳，杀怪数进行分组，
# 在查询表中每条记录的时候，同时判断当前记录在同一个分组中的排序，这里在select关键字后面的表达式中进行。


select d.name c.timestr, kills from
(
	select user_id, timestr, kills, (
		select count(*) from kills_record b where b.user_id = a.user_id and a.kills <=  
	) as cnt from kills_record a group by user_id, timestr, kills
)c join our d on c.user_id = d.id where cnt <=2

# 1. 根据用户id、时间戳、杀怪数进行分组，分组是order by的实现，默认是升序的
# 2. 在分组的同时计算小于等于当前记录杀怪数的记录数量，这个值其实用于标识当前记录的位置
# 3. 最后查询出来的结果是根据user_id、日期、杀怪数倒叙分组好的结果集
# 4. 我们最后再从中来筛选数据
```

分组选择查询要理解俩个知识点：
1. SQL语句的解析顺序
2. select关键字后面的表达式可以做运算、判断、查询


## 行转列
使用行转列的场景主要是数据汇总，例如将多个用户的多条记录整理成excel报表格式。

行转列的技巧再与使用case关键字在select表达式中做条件判断，例如：
```sql
select sum(case when o.name = '孙悟空' then k.kills end) as,
	sum(case when o.name = '猪八戒' then k.kills end),
	sum(case when o.name = '沙悟净' then k.kills end)
	from our o
	inner join kills_record k on o.id = k.user_id

```


## 列转行
一般用于属性拆分，etl数据处理。
```sql
# 测试SQL语句
alter table our add column mobile varchar(100);
update our set mobile='13565439876,13512345876,135009939876' where id = 1;
update our set mobile='13689039876,13576545876,135999889876' where id = 2;
update our set mobile='13565439876,13512345876' where id = 3;
update our set mobile='13565439876,13512345876' where id = 4;

create table tb_sequence(id int auto_increment not null, primary key(id));
insert into tb_sequence values(),(),(),(),(),(),(),(),()
```

- 序列表的实现：
```sql
select name, concat(mobile, ',') as mobile, 
		length(mobile)-length(replace(mobile, ',', ''))+1 as size from our
# 该SQL语句明显的看出，表达式是能够进行逻辑运算，函数处理等操作的
# 前面的表达式能够被后面的表达式使用


select * from tb_sequence a
	cross join(
		select name, concat(mobile, ',') as mobile, 
		length(mobile)-length(replace(mobile, ',', ''))+1 as size from our
	) b
	on a.id<=b.size 


replace(
	substring(substring_index(mobile, ',', a.id), 
	char_length(substring_index(mobile, ',', a.id-1))+1),
	',', 
	''
) as mobile
# 这个子句替换上面的*号，可以完成对列转行
```

## 行列转换
```sql
create table equipment(
	id int primary key auto_increment,
	user_id int,
	arms varchar(20),
	clothing varchar(20),
	shoe varchar(20)
)charset=utf8;
insert into equipment value(default, 3, '长杖', '袈裟', '布鞋');
insert into equipment value(default, 1, '金箍棒', '黄金甲', '踏云靴');
insert into equipment value(default, 2, '九齿钉耙', '僧衣', '布鞋');
insert into equipment value(default, 4, '降妖仗', '袈裟', '布鞋');
```

- 行列转换的union来实现
```sql
select o.name, 'arms' as equipment, e.arms from our	o
	inner join equipment e on o.id=e.user_id
union all
select o.name, 'clothing' as equipment, e.arms from our	o
	inner join equipment e on o.id=e.user_id
```

- 行列转行的case实现
```sql
select o.`name`, 
	COALESCE(case when s.id=1 then arms end,
	case when s.id=2 then shoe end,
	case when s.id=3 then clothing end) as equipment
	from tb_sequence s
	cross join equipment e
	inner join our o on e.user_id=o.id
	where s.id <=3
```


## SQL语句解析顺序
1. FROM <left_table>
2. ON <join_condition>
3. <join_type> JOIN <right_table>
4. WHERE <where_condition>
5. GROUP BY <group_by_list>
6. HAVING <having_condition>
7. SELECT 
8. DISTINCT <select_list>
9. ORDER BY <order_by_condition>
10. LIMIT <limit_number>

