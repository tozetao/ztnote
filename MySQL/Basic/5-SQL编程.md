
12. 变量
	mysql中有系统变量和自定义变量。
	16.1 系统变量
		系统定义好的变量，比如autocommit、character_set系列。
		用户不能定义系统变量，但是能够修改和使用。
		
		使用：用select获取变量的值，但是select默认会把所有字符串当作一个表的字段来处理，所以需要使用@符号来访问。
		语法：select @@变量名;
		例如：select @@autocommit;

		查看所有变量：show variables

		修改变量
			局部修改：set 变量名=值
			这种修改这是回话修改，就是只会在当前客户端生效，其他客户端不声效。

		全局修改：修改mysql的配置文件，my.ini

	16.2 declare局部变量
		 declare var_name[,...] type [DEFAULT value]
			这个语句被用来声明局部变量。要给变量提供一个默认值，请包含一个DEFAULT子句。值可以被指定为一个表达式，不需要为一个常数。如果没有DEFAULT子句，初始值为NULL。 
		 
			局部变量的作用范围在它被声明的BEGIN ... END块内。它可以被用在嵌套的块中，除了那些用相同名字声明变量的块。 

	16.3 set语句
		set用于设置不同类型的变量，set可以对自定义变量和系统变量赋值。
		自定义变量可以被写作@var_name，并可以进行如下设置：
		set @var_name=expr;
		set @name='xiaoming';	#对变量name进行赋值
		select @var_name		#查看var_name变量

		关于赋值
			mysql为了区分比较运算符=，特别定制了一个赋值符号:=，虽然=在声明变量也能很好的工作，但是在select语句中=号不能工作的，需要使用:=符号
			set @age=0;
			select @age=age from user;	#@age=age被当作一个全局变量处理。
			select @age:=age from user;	#正确的赋值。

		变量SET语句
			SET var_name = expr [, var_name = expr] ...
			在存储程序中的SET语句是一般SET语句的扩展版本。被参考变量可能是子程序内声明的变量，或者是全局服务器变量。 

			在存储程序中的SET语句作为预先存在的SET语法的一部分来实现。这允许SET a=x, b=y, ...这样的扩展语法。其中不同的变量类型（局域声明变量及全局和集体变量）可以被混合起来。这也允许把局部变量和一些只对系统变量有意义的选项合并起来。在那种情况下，此选项被识别，但是被忽略了。

		SELECT ... INTO语句
			SELECT col_name[,...] INTO var_name[,...] table_expr
			这个SELECT语法把选定的列直接存储到变量。因此，只有单一的行可以被取回。 
			
			example：SELECT id,data INTO x,y FROM test.t1 LIMIT 1;
				相当于select @x_id
			
			注：局部变量中，col_name不能和表的列名一样，否则select语句会认为这是局部变量，而不是表字段。

	16.4 变量作用域
		全局变量
			例如：set @name=null;
			外部定义的变量叫做全局变量，全局变量能够在函数内部使用。
			访问的时候，也是通过@来进行访问。

		局部变量：
			declare 变量 数据类型
			局部变量不能在函数外部访问。
			访问的时候，不需要@符号。
	example：
		set @name='daha';

		delimiter $
		create procedure p1()
		begin
			declare age int default 18;
			set age:=age+20;
			select concat('20年后的年龄是：',age);
			select @name;
		end$

13. 语句分支
	17.1 if分支结构
		语法：
			if expr then
				//语句
			else
				//语句
			end if;
		example：
			delimiter $$
			create function myfunc(a int,b int) returns varchar(20)
			begin
				if a>b then
					return 'a>b';
				else
					return 'a<b';
				end if;
			end$$
			delimiter ;

	17.2 while循环
		语法
			while expri do
			//循环体
			end while;
		说明：
			expir：表达式
		
	17.3 iterate、leave关键字
		iterate：终止这次循环，执行剩下循环，类似continue；
		leave：结束这个循环，相当于break；
		
		用法：iterate/leave 循环名字;
		
		example：
			从1开始，求和到指定的值，返回计算结果，如果这个数值有5这个数值，不要。

			delimiter $$
			create function while_demo(int_1 int) returns int
			begin
				declare res int default 0;
				declare i int default 1;

				reswhile:while i<=int_1 do
					if i=5 then 
						set i=i+1;
						iterate reswhile;
					end if;

					set res=res+i;
					set i=i+1;
				end while;

				return res;
			end
			$$
			delimiter ; 

	17.4 case语句
		case value
			when [compare-value] then result 
			when [compare-value] then result
			[ELSE result]
		example：
			case $age
				when 10 then ...
				when 20 then select xxx
				else select...
	
	17.5 repeat 循环
		create procedure p()
		begin
			declare i int default 0;
			repeat
				select i;
				set i:=i+1;
			until i>10 end repeat
			#直到满足某个条件结束语句
		end		

14. 函数
		能够解决某个问题的一堆代码块，能够被重复使用，且有返回值。

		函数分为系统函数、自定义函数。

	17.1 函数调用
		select 函数名();
		例如：select now();--显示当前时间。

	17.2 系统函数
		substring(string val,int start,int length)
			字符串截取，start是起始下标，length表示截取长度
			按照字符来截取，PHP中是按照字节截取。

		char_length()
			字符的长度。
		length()
			字节的长度。
		instr(string target,string val)
			判断字符串是否在目标字符串中，返回字符串的起始位置(字符位置)。
			从1开始的。
		lpad(string val,int 目标长度,int 填充的值)
			length：目标长度，

			左填充，左边填充内容直到字符串到达目标长度。
		insert(目标字符串, 起始位置，替换长度，替换内容)
			向目标字符串中替换字符串

		strcmp(a,b)
			使用当前字符集来进行比较。
			a>b，返回-1，a<b 返回1，相等返回0

	17.3 自定义函数
		语法：
			create function 函数名(参数列表) returns 数据类型
			begin
				//函数体
				//返回值
			end
			
			mysql是强类型的语言，必须返回值。
			参数列表的定义：参数名 数据类型

		例如
			delimiter $$
			create function myfunc(num int) returns int
			begin
				declare res int default 0;
				return res;
			end
			$$
			delimiter ;

	2.3 调用自定义函数
		语法：select 函数名(参数列表)
		注：调用传入的值必须与定义的数据类型一致
		select myfunc('1');--整型加引号没关系的

	2.4 查看函数
		语法：show function status;--查看所有函数
		函数不用跨数据库应用。

	2.5 删除函数
		语法：drop function 函数名
		

15. cursor 游标
	游动的标识，字面解释。
	1条SQL语句，对应N条资源，而游标就像指针一样，指向这N条资源的接口/句柄。
	沿着游标，可以一次取出一行数据。

	declare 声明；declare 游标名 cursor for select_statement，
	open 打开；open 游标名
	fetch 取值；fetch 游标名 into var1,var2...
	close 关闭；close 游标名

	好处：如果我们用select查询，那么mysql会将所有结果显示给我们看，而游标就像指针一样，我们可以取每一行数据，指针会下移一位。
	当取出最后一行记录后，游标就会到头了。02000

	example：
		delimiter $
		create procedure p()
		begin
			declare _name varchar(20);
			declare _age int;
			declare getuser cursor for select name,age from user;
			open getuser;
			fetch getuser into _name,_age;
			select _name,_age;
			close getuser;
		end$
		delimiter ;
	
	example：循环游标
		delimiter $
		create procedure c5()
		begin
			declare _name varchar(20);
			declare _age int;
			declare cnt int default 0;
			declare i int default 0;
			declare getuser cursor for select name,age from user;

			select count(*) into cnt from user;
			open getuser;

			repeat
				fetch getuser into _name,_age;
				select _name,_age;
				set i:=i+1;
			until i>=cnt end repeat;

			close getuser;
		end$
		delimiter ;

	example：利用标识来防止游标过界
		delimiter $
		create procedure c6()
		begin
			declare _name varchar(20);
			declare _age int;
			declare hasRow int default 1;	#标识，值是0则无游标过界

			declare getuser cursor for select name,age from user;
			#声明一个句柄事件，一旦not found找不到数据，则执行一个语句
			declare continue handler for not found set hasRow:=0;

			open getuser;
			repeat
				fetch getuser into _name,_age;
				select _name,_age;
			until hasRow=0 end repeat;
			close getuser;
		end$
		delimiter ;
		#在mysql cursor中，可以declare continue handler来操作1个越界标识。
		#语法：declare continue handler for not found statement

		上面的例子中有逻辑错误，由于我们定义的是一个continue的handler，所以在执行set hasRow=0的语句之后，仍然会继续执行循环后面的语句，所以最后一路记录就会多查询一次。

		#所以将这个handler修改成exit handler就可以了。

		delimiter $
		create procedure c7()
		begin
			declare _name varchar(20);
			declare _age int;
			declare hasRow int default 1;	#标识，值是0则无游标过界

			declare getuser cursor for select name,age from user;
			#声明一个句柄事件，一旦not found找不到数据，则执行一个语句
			declare exit handler for not found set hasRow:=0;

			open getuser;
			repeat
				fetch getuser into _name,_age;
				select _name,_age;
			until hasRow=0 end repeat;
			close getuser;
		end$
		delimiter ;

		#除了exit handler，还有undo handler
		#continue，触发句柄后执行后面的语句
		#exit，触发句柄后终端执行后面的语句
		#undo，触发后，前面的语句撤销

	上述的代码逻辑不完善，如果表中一行数据都没有，那么刚去数据的时候就会报错。
	改进：
		delimiter $
		create procedure c9()
		begin
			declare _name varchar(20);
			declare _age int;
			declare hasRow int default 1;	#标识，值是0则无游标过界

			declare getuser cursor for select name,age from user;
			#声明一个句柄事件，一旦not found找不到数据，则执行一个语句
			declare continue handler for not found set hasRow:=0;

			open getuser;
			fetch getuser into _name,_age;

			repeat
				select _name,_age;
				fetch getuser into _name,_age;
			until hasRow=0 end repeat;

			close getuser;
		end$
		delimiter ;
		#回头测试下逻辑。