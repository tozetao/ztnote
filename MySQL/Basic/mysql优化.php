<?php
/*
一、常见数据库的优化方式：
	我们之前讲的静态化，memcache主要是少查询数据库，或者不查询数据库的。一个网站，必经要查询数据库，索引也要对数据库进行优化。
	1、表的设计要符合三范式。
	2、添加适当的索引，索引对查询速度影响很大，必须添加索引。主键索引，唯一索引，普通索引，全文索引
	3、添加适当存储过程，触发器，事务等。
	4、读写分离（主从数据库）
	5、对sql语句的一些优化，（查询执行速度比较慢的sql语句）
	6、分表分区
		分表：把一张大表分成多张表。分区：把一张表里面的分配到不同的区域存储，
	7、对mysql服务器硬件的升级操作。



三、定位慢查询
	慢查询：找出一个网站中，查询速度比较慢的语句。
		可以开启一个日志文件，记录查询速度比较慢的SQL语句。
		在默认情况下，mysql的慢查询日志是关闭的，默认记录时间是超过10秒的SQL语句。
	
	1. 慢查询的开启
		a. 先关闭mysql服务 
		b. 使用mysqld.exe启动慢查询
			mysqld.exe --safe-mode --slow-query-log
			通过慢查询日志定位执行效率较低的SQL语句。
			慢查询日志记录了所有执行时间超过long_query_time所设置的SQL语句。
			
	2. 慢查询日志文件
			在my.ini配置文件中，datadir=""。
			该参数用于配置日志文件路径。

	3. 进程测试
		查看当前数据库下慢查询记录时间
		show variables like 'long_query_time'

		修改慢查询时间：
		set long_query_time=5;

		通过如下的一个函数来进行测试：
		benchmark(count,expr)：该函数可以测试执行count次expr操作需要的时间。
		count：次数
		expr：表达式

		一般查询时间如果有1秒的话，这个时间度对于计算机已经够长了。


四、索引
	1. 主键索引
		在创建表的时候添加
		create table emp(id primary key)
		alert table tablename add primary key (列1，列2)
		主键索引特点：
		a. 一个表中最多只有一个主键索引
		b. 一个主键索引可以指向多个列
		c. 主键索引的列，不能有重复值，不能有null
		d. 主键索引的效率高。
	2. 唯一索引
		可以在建立表的时候添加：
			create table emp(name varchar(32) unique)
		在建完表之后，添加：
			alter table tablename add unique [索引名](列名)
			索引名不写的话，则默认列名为索引名。

		特点：
			a. 一个表中可以有多个唯一索引
			b. 唯一索引可以指向多个列，比如alter table tablename add unique [索引名](列1，列2)
			c. 唯一索引的值可以重复，也可以有null值
			d. 唯一索引的效率较高。

	3. 普通索引
		使用普通索引主要是提高查询效率
		添加：alter table tablename add index [索引名]（列1，列3）
		
	4. 全文索引
		mysql自带的全文索引，mysql5.5不支持中文，支持英文，同时要求表的存储引擎是myisam。如果希望支持中文，有两个方案， 
		a. 使用sphinx中文版coreseek，来替代全文索引
		b. 插件mysqlcft。

		

	5. 查看索引
		a. show  index from  表名 
		b. show  indexes from 表名 
		c. show  keys from 表名 
		d. desc 表名

	6. 删除索引
		a. 主键索引的删除：alter table tablename drop primary key.
			PS：删除之前，必须删除auto_increment
		b. 唯一索引的删除
			alter table tablename drop index 唯一索引的名字
		c. 普通索引的删除：
			alter table tablename drop index 普通索引的名字
	
	key_name：主键索引显示primary key，其他类型的索引直接显示索引名字

五、索引应用
	1. 优化group by语句
		默认情况下，mysql对所以的group by col1，col2进行配需，这与在查询中指定order by col1，col2类型
		如果查询中包括group by，但用户想要避免排序结果的消耗，则可以使用order by null禁止排序。
		PS：你可以使用desc查看查询的描述结果。
		
		desc select * from user group by classid;
		extrx：using temporary；using filesort
	
	2. 当取出的数据量超过表中数据的20%，优化器就不会使用索引，而是全表扫描。 
		这是因为扫描的行数太多了，优化器认为全表扫描比索引来的块。
		你可以认为取出的数据太多了，使用索引的数据反而更慢。
		
		
		如果你有5条数据
		desc select * from user where id>1;
		这时候你会看到没有使用索引。


	3. 查看索引的使用情况
		show status like 'handler_read%';
		会显示索引的使用情况
		handler_read_key：这个值越高越好，越高表示使用索引查询到的次数越高。

		handler_read_rnd_next:这个值越高，说明查询低效。


	4. 如果列类型是字符串，SQL语句一定要在条件中将数据用引号引用起来，否则不使用索引。
		desc select * from user where name=123;--不会使用索引
		desc select * from user where name='123';--会使用索引

		在mysql中，where的=号后面如果是数字是不会报错的。

		possible_key:name
		key:null
	


	5. 对应大批量插入数据，需要注意的：
		大批量插入数据(MySql管理员) 了解
		对于MyISAM：
			a. 先禁用索引：alter table table_name disable keys;否则影响插入数据的效率。

			b. loading data//insert语句; 执行插入语句

			c. 执行完成插入语句后，开启索引，统一添加索引。
				alter table table_name enable keys;


		对于Innodb：
			1，将要导入的数据按照主键排序
			2，set unique_checks=0,关闭唯一性校验。
			3，set autocommit=0,关闭自动提交。

	6. 添加索引主要的问题
		a. 较为频繁的作为查询的字段应该创建索引。
			example：select * from emp where empno = 1
			但是有几点要注意的。
			唯一性太差、更新非常频繁的字段都不适合作为索引。

				唯一性太差的字段不适合单独创建索引，即使频繁作为查询条件
				example：select * from emp where sex = '男‘

				更新非常频繁的字段不适合创建索引
				select * from emp where logincount = 1
	
		b. 不会出现在WHERE子句中字段不该创建索 

		索引的代价：
			索引主要用于提高查询的效率，但是添加索引的同时
	
索引应用讲解2
	1. 对于创建的多列(符合)索引，只要查询条件使用了最左边的列，索引一般就被引用。
		alter table tablename add index (name,email)
		desc select * from user where name='xiao';
	2. 对于使用like的查询，查询如果是 '%xxx'，不会使用索引。'xx%'才会使用索引
		desc select * from user where name like '%xxx';

	3. 如果条件中有or，则要求or的索引字段都必须有索引，否则不能使用个索引。
		or的多个字段查询，多个字段必须都有索引。
		

六、并发处理
	比如执行如下：
	a. 从数据库中取出id值。
	b. 进行加一操作
	c. 修改完成后保存到数据库中。
	比如原来 id的值为100，+1执行后就变成101。
	以上步骤执行100次，最后变成200

	有两个用户同时执行的话。
	a用户：
	100
	101

	b用户：
	100
	101

	同时取出1个相同的id的值，之后程序进行计算，再进行mysql更新，这时候就出现问题了，原本id对于的值应该是102的。
	这种情况一般是高并发会出现的问题，如秒杀活动，限量秒杀商品。

	对于这种情况，我们可以通过锁机制来进行解决。
	锁机制：在执行时，只有一个用户获得锁，其他用户处于阻塞状态，需要等待解锁。

	比如说刚才的常见，当a用户获得锁了，取出id的值，这时候其他用户，不管多少人，都处于阻塞状态，是无法取出数据的，需要等待a用户解锁操作完成。

	mysql的锁机制：
	1. 表级锁
		把整个表锁起来，一个用户操作了这张表，其他人就不能操作了，只能等待。
		开销小，加锁快、发生锁冲突的概率最高，并发度最低。myisam引擎属于这种类型。
		
		演示：
		对myisam表的读操作加读锁，不会阻塞其他进程对同一表的读请求，但是会阻塞其他进程对同一表的写请求(mysql服务器会一直以等待状态对待请求)。
		只有当读锁释放后，其他进程才可以对表进行其他操作。


		说明：表添加读锁后，其他进程对该表只能查询操作，修改时会被阻塞。
		而当前进程只能进行查询操作，不能执行修改操作（报错）。同时不能对没有锁定的表进行操作。

		语法：
			锁表：lock table 表名 read|write
				或lock table 表1 read|write,表2 read|write
			
		对myisam表的写操作(加写锁)，会阻塞其他进程对锁定表的任何操作，不能读写。
		表加写锁后，则只有当前进程对锁定的表，可以执行任何操作。


	2. 行级锁
		把要操作的一行记录锁起来。
		发生锁冲突的概率最低，并发度也最高。innodb属于这种类型。

		行锁的实现方式：
			innodb存储引擎是通过给索引上的索引项加锁来实现的，这就意味着：只有通过索引条件检索数据，innodb才会使用行级锁，否则，innodb使用表锁。

			通过列的索引项实现加锁，也就是说只有通过索引条件检索到数据，innodb才会使用行锁，否则使用表锁。

		行锁语法：
		begin;
			sql语句
		commit;

		begin;
			update user set name='yuxuan' where id=1;
		commit;

		开启行锁后，当前进程对某条记录执行操作(读写)时，其他进程不能操作(可以读，不能写)和当前进程相同id的记录。
		但是其他进程可以操作id不同的记录。


	3. 表锁的缺点
		因为表锁的机制，它会阻塞其他进程对当前表数据的访问。尤其是对表添加写锁后，其他进程就不能对表访问了，这样子会阻塞整个网站，拖慢整个网站的速度。

		所以php有文件锁，在实际项目中多数使用文件锁。


	类似的面试题：
		一件商品，库存量还有一件，这时有两个用户同时请求下订单，如何防止都下订单成功，却没有货发。

		首先表里面的记录，肯定是让人能随时读取的。

		所以只能使用行锁，在下订单，更改当前商品库存的时候，只能等当前用户请求处理完成，这时候才能处理其他用户请求。

七、分表技术
	把一个大表分成几个小表，分为垂直分割和水平分割。
	1. 垂直分割
		在dedecms里面，垂直分割：
		在一个数据库中想要存储各种数据，比如说文章数据，电影，音乐，商品数据，这个时候我们就可以进行垂直分割。

		以内容主表+附加表的形式来存储。

		内容主表：存储各种数据的一些公共信息，比如数据的名称，添加时间等，
		附加表：可以使用多个附加表，附加表存储一些数据的独特的信息或不经常访问的。
		
		垂直分割原因：是内容主表里面的数据访问比较频繁，这样子查询的时候，我们不用总是查询不经常使用的数据。
		
		example：
		内容主表：archives，存储内容的公共信息
		id	name	addtime

		若单个附加表：
			news表
			aid		author	content		c_length

			move表
			m_id	导演	主演	年代	语言

			music表
			m_id	作词	作曲	歌手
		
		垂直分割的好处，首先这样子设计的话，就可以存储各种类型的新闻。然后内容主表的内容是频繁访问的，我们查询的时候不会查询无用的数据，较少mysql服务器负担。
		
		适用场景：内容主表数据访问频繁，其他数据类型不同而且访问不频繁。

		垂直分割主要使用连接查询来获取我们想要的数据的。
	2. 水平分割
		水平分割主要通过id取模，在存储的时候。
		example：
			user_qq0表
				id	haomao	name

			user_qq1表
				id	haomao	name

			user_qq2表
				id	haomao	name
		
		当我们对qq表进行查询的时候，
		比如在申请是：qq/3取模，如果等于0则存储到user_qq0表，如果等于1则存储到user_qq1表，

		在查询时，根据输入的qq号和3取模，如果等于0则去user_qq0表查询，如果等于1则去user_qq1表查询，如果等于2则去user_qq2表查询。

		水平分割的话，可以使一个大表分割成几个小表。
		在执行查询和存储时，要通过一定的条件区别在哪个表里面操作。

八、分区技术
	就是把一张表的数据存储到磁盘不同区域，但是仍然是同一张表。

	1. 基本概念
		mysql5.1后有4种分区类型：
		a. Range（范围）–
			这种模式允许将数据划分不同范围。例如可以将一个表通过年份划分成若干个分区。 
		b. List（预定义列表）
			这种模式允许系统通过预定义的列表的值来对数据进行分割
		c. Hash（哈希）
			这中模式允许通过对表的一个或多个列的Hash Key进行计算，最后通过这个Hash码不同数值对应的数据区域进行分区。例如可以建立一个对表主键进行分区的表。 
		d. Key（键值）
			上面Hash模式的一种延伸，这里的Hash Key是MySQL系统产生的。 

		一般我们经常使用range和list类型。
	2. range分区
		需求：
			假如你创建了一个如下的表，该表保存有20家超市的职员记录，这20家超市的编号从1到20.如果你想将其分成4个小分区，可以采用range分区。
			1区：1-5
			2区：6-10
			3区：11-15
			4区：16-20
		语法：
		create table emp( 
			id int not null, 
			name varchar(32) not null default '' comment ‘职员的名称’, 
			store_id int not null comment ‘超市的编号范围是1-20’
		)engine myisam charset utf8 
		
		partition by range(store_id)( 
			partition p0 values less than(6), //是store_id的值小于6的存储区域。
			partition p1 values less than(11), //是store_id的值大于等于6小于11的存储区域。
			partition p2 values less than(16), 
			partition p3 values less than(21) 
		)
		
		insert into emp values(1,’杨过’,1)--数据是存储到p0区
		insert into emp values(23,’小龙女’,15)--数据是存储到p2区
		insert into emp values(100,’李莫愁’,11)=数据是存储到p2区。
		
		range(范围)分区模式，提供将表的数据划分为不同范围。需要你自己去手动规划数据。
		创建表的时候后面跟区域代码。


		PS：在取出数据时，条件中必须有partition by range(store_id)，range里面的字段，mysql才会去指定的分区中寻找数据，否则会在所有分区中寻找数据。
	3. list分区
		list分区与range分区有相似地方。
		让数据在某个范围内。

		例子：
			假如你创建一个如下的一个表，该表保存有20家超市的职员记录，这20家超市的编号从1到20.而这20家超市分布在4个有经销权的地区，如下表所示：
		地区	超市id
		北区	5，6，7，8
		东区	1，2，3，4
		核心区	12，15，16
		
		语法：
		create table emp( 
			id int not null, 
			name varchar(32) not null default '', 
			store_id int not null 
		) 
		partition by list(store_id)( 
			partition p0 values in(5,6,7,8), 
			partition p1 values in(11,3,12,11), 
			partition p2 values in(16), 
			partition p3 values in(21) 
		);


		注意：在使用分区时，where后面的字段必须是分区字段，才能使用到分区，否则mysql是到所有分区中寻找数据。
	4. 分区表的限制
		a. 只能对数据表的整型列进行分区，或者数据列可以通过分区函数转化成整型列。
			example：year(2010-10-9)->2010

		b. 最大分区数目不能超过1024个分区。
		c. 如果含有唯一索引和主键索引，则分区列必须包含在所有的唯一索引或主键索引之内。

		d. 按照日期进行分区是非常适合的，因为有很多日期函数可以使用。
		但是对于字符串来说合适的分区函数就不太多了。

		例如商城的订单，按照月份来分区，来进行设计。几百万条记录没什么意义，表的记录要有千万级别、上亿还差不多。几百万都是小kiss。

九、其他调优
	1. 选择合适的存储引擎
		a. MyISAM
			默认的MySQL存储引擎。如果应用是以读操作和插入操作为主，只有很少的更新和删除操作，并且对事务的完整性要求不是很高。
			
			其优势是访问的速度快。(尤其适合论坛的帖子表)

		b. InnoDB
			提供了具有提交、回滚和崩溃恢复能力的事务安全。但是对比MyISAM，写的处理效率差一些并且会占用更多的磁盘空间(如果对安全要求高，则使用innodb)。
			适合[账户，积分]

		c. Memory/heap 
			[一些访问频繁，变化频繁，又没有必要入库的数据 ：比如用户在线状态]
			
			说明: memory表的数据都在内存中，因此操作速度快，但是缺少是当mysql重启后，数据丢失，但表的结构在.
			
			注：从mysql5.5.x开始，默认的存储引擎变更为innodb，innodb是为处理巨大数据量时拥有最大性能而设计的。它的 cpu效率可能是任何其他基于磁盘的关系数据库引擎所不能匹敌的。


	2. 数据类型的选择
		a. 在精度要求高的应用中，建议使用定点数来存储数值，以保证结果的准确性。decimal 不要用float
		
		b. 考虑数据占用磁盘存储空间的大小。
			需求：要用于存储手机号，哪个类型比较合适。
			
			假如我们要用char(11),如果字符集是utf8，则占用多少个字节。11*3==33，如果是gbk字符集则占用11*2=22个字节，
			
			如果用bigint型存储，则占用8个字节。

			所以字节的占用度也是要考虑的。
		c. 如果要存储ip地址？
			假如用char(15)占用很多字节，能否用整型来存储呢？
			可以通过一个函数，把ip地址转换成整数。可以使用int来存储
			
			inet_aton()：把ip地址转换成整数。
			inet_ntoa()：把整数转换成ip地址。

			select inet_aton('192.168.1.1');
			select inet_ntoa(ip);
		d. 根据需求选择最小整数类型。
			比如用户在线状态：离线，在线，离开，忙碌，隐式等，可以采用0,1,2,3,5来表示，没有必要用char()或varchar()型来存储字符串。
		
	3. myisam表的定时维护
		对于myisam 存储引擎而言，需要定时执行optimize table 表名，通过optimize table语句可以消除删除和更新造成的磁盘碎片，从而减少空间的浪费。

		语法格式：optimize table  表名：

		create table temp2( id int) engine=MyISAM;
		insert into temp2 values(1);

		你可以执行清理碎片，测试的时候，查看文件名后缀为MYD的文件大小。

		PS：myisam存储引擎，对于删除和更新，表的大小是变化的，需要磁盘整理。
*/
