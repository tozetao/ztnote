<?php
/*
9. 自定义回话管理
	10.1 数据库存储
		默认的文件存储有i/o瓶颈，所以需要改变存储方式，以数据库来存储。

		session_set_save_handler(( callable $open , callable $close , callable $read , callable $write , callable $destroy , callable $gc )
			函数说明
				重写session的存储方式。

			参数说明
				6个函数，都是回调函数，用于重写session默认函数。
				$open：开启session
				$colose：关闭session资源
				$read：
				$write：
				$destory
				$gc

			function sess_open(){
				echo 'open';
			}
			function sess_close(){
				echo 'close';
			}

			//读取session
			//从数据库读取数据，只负责读取，返回一个序列化的字符串，由session系统自己去反序列化
			function sess_read($sess_id){
				//session过期时间戳边界，在读取数据的时候过滤掉过期的session
				$expire=time()-ini_get('session.gc_maxlifetime');

				//根据sessionID获取数据
				$sql="select * from	session where sess_id='{$sess_id}' and sess_expire >= '{$expire}'";

				$res=mysql_query($sql);//得到结果集
				
				//得到的是一个数组
				if($sess=mysql_fetch_assoc($res)){
					return $sess['sess_info'];//这是一个序列化字符串
				}
			}

			//写入session
			//sess_id：session的id
			//sess_info：序列化$_SESSION对象后的数据
			function sess_write($sess_id,$sess_info){
				//1. 获取时间戳
				$time=time();
				
				//2. 写入数据库中，因为session的id相同，所以使用replace
				$sql="replace into session values('{$sess_id}','$sess_info','{$time}')";
				
				//3. 返回写入结果
				return mysql_query($sql);			
			}

			//5. 销毁session
			function sess_destroy($sess_id){
				//删除数据库数据
				$sql="delete from session where sess_id='{$sess_id}'";
				return mysql_query($sql);

			}
			//6. 回收session
			function sess_gc(){
				//删除所有过期session

				//判断expire是否过期，
				$expire=ini_get('session.gc_maxlifetime');	//获取session有效期
				
				//当前时间戳-session有效期时间戳 = session存在最迟的时间戳
				$expire=time()-$expire;
				//在$expire之前的session都是过期的

				$sql="delete from session where sess_expire < '{$expire}'";
				echo __FUNCTION__,'<br/>';
				return mysql_query($sql);
			}

		//使用session_set_save_handler()修改session机制之前
		//session_start();	//初始化session

		//修改session机制
		session_set_save_handler('sess_open','sess_close','sess_read','sess_write','sess_destroy','sess_gc');

		session_start();//调用open、read、write、close函数

		//写入数据
		//$_SESSION['user']='zetao';
		//ppo5ssg222potbbjjp1m5bfp33	=>sessionID
		//user|s:5:"zetao";				=>session_info

		//$_SESSION['age']='25';
		//$_SESSION['height']='165';
		//$_SESSION['money']='100000000';

		//查看数据
		//var_dump($_SESSION['user']);
		var_dump($_SESSION);

		//销毁
		//session_destroy();	//销毁的是sessionID对应的记录或者文件
		//var_dump($_SESSION);
		//如果是第一次销毁session，那么仍然会输出session保存的数据，因为第一次销毁的时候，会调用read函数，只要session数据不过期，数据就会被保存到session中，所以可以使用这个session数据。
		//第二次访问的时候，由于销毁session，删除了session文件或者记录了，根据原有的sessionID，自然就读取不到对应的数据。
		//当然，你可以自己重新再设置


		//这4个函数的调用顺序：open、read、write(destroy)、close


		//无论写入和查看，都会调用这4个函数
		//由于sess_id是唯一键，不允许相同的值插入，这意味着，当$_SESSION变量存储的数据变化话，在数据库也无法插入成功

		//修改插入语句，使用replace关键字.



		//open：初始化资源，默认打开文件资源。
		//close：关闭资源，默认关闭文件资源。也就是session的start和close。

		//session_destroy();//这里的话，write就不会被调用了，因为你显示的销毁了。

		//回收：针对所有过期的session数据，而不是当前自己的数据。session的回收机制是千分之一，所以这里的回收函数没有被调用到。
		//垃圾回收指的是session的最后一次修改的时间如果与当前时间进行相减，结果超过session的最大声明周期，那么这部分session数据被称为垃圾，所以就会被系统删除，垃圾回收是按照指定的比例去产生的(一定几率调用)，如果一旦垃圾回收被触发，会删除所有过期的session数据。

		//销毁：默认删除sessionID对应的文件，只有当用户使用session_destroy()函数去销毁才会被触发的。


		//gc的调用方式：将gc的调用概率提高。open、read、gc、wirte、close
		//gc是在读取完数据后面调用的。 

		//为什么gc函数在write之前被调用？
		//如果write函数先被调用，那么当前session的时间戳也会被更新，对于那些本身存在很久的，没有动作(读取或者写入)的session，就不会被回收掉了。
		//gc函数在read之前，这是不合理的，因为你拿着过期的数据在使用。
		//所以在读取数据的时候，要过滤掉过期的数据。


		//session的有效期，如果session没有动作，那么有效期是系统默认设置的时间。
		/*
			例如session的有效期到了，但是生命周期仍存在，read()函数没有过滤脏数据。gc函数回收的时候，数据仍然会被读取出来，之后就会被销毁。如果gc函数没被调用，那么session的有效期到了，是仍继续使用，还是系统会有函数自动去销毁他？
		*/

		/*
		session系统注意的几个点

		1.session过期的时间戳边界：读取的时候，用于过滤失效的session，回收也会被使用到。

		2.time时间戳：每次session有动作的时候，写入当前时间，保证session不过期。

		3.session有效期：这里的有效期，是指session无动作情况下保存的时间，并不是session的生命周期。

		4.生命周期：利用cookie实现，默认就是cookie的生命周期，即关闭浏览器失效。通过sessionID，使用cookie技术来区分用户。


面试题
	浏览器禁止cookie之后，session就不能使用了。
	可以通过其他方式来实现，a标签。

	实现方式
		通过修改php.ini配置文件，将session中的sessionID使用cookie来进行传输的方式改变。
		1.修改配置文件
			a.不使用cookie保存sessionID
				session.use_cookies=0
			b.将sessionID只允许使用cookie进行数据传输改变
				session.use_only_cookies=0
			c.允许sessionID放到a标签后面
				session.use_trans_sid=1
				有该选项后，系统能被自动的为a标签增加sessionID。
		2.重启服务器即可。
	
	另外，也可以通过自己在页面里面重写a标签，通过session_name()、session_id()俩个函数，获取id和sessionID名，然后附带给a标签。
	name=id

	之后，需要使用session的php文件可以通过session_id()函数来设置sessionID，这种方式也可以使用session。
	$name=session_name();
	session_id($_GET[$name]);
	session_start();这时候就可以使用了。

 */



/*
1. session_set_save_handler()
	该函数用于自定义session的存储方式。
	open、read、write、close是会话生命周期内要调用的一组回调函数。
	还有一些回调函数被用来完成垃圾清理：destroy 用来删除会话， gc 用来进行周期性的垃圾收集。 

2. 回话的一些行为
	回话开始：
		PHP会调用open管理器，然后再调用read回调函数来读取内容，该回调函数返回已经经过编码的字符串。然后PHP会将这个字符串解码，并且产生一个数组对象，然后保存至 $_SESSION 超级全局变量。

	脚本结束：
		当 PHP 关闭的时候（或者调用了 session_write_close() 之后）， PHP 会对 $_SESSION 中的数据进行编码， 然后和会话 ID 一起传送给 write 回调函数。 write 回调函数调用完毕之后，PHP 内部将调用 close 回调函数。 

	销毁会话：
		PHP 会调用 destroy 回调函数。 

	GC：
		根据会话生命周期时间的设置，PHP 会不时地调用 gc 回调函数。 该函数会从持久化存储中删除超时的会话数据。 超时是指会话最后一次访问时间距离当前时间超过了 $lifetime 所指定的值。 

3. 回调函数说明
	open(string $savePath, string $sessionName)
		open 回调函数类似于类的构造函数， 在会话打开的时候会被调用。 这是自动开始会话或者通过调用 session_start() 手动开始会话 之后第一个被调用的回调函数。 此回调函数操作成功返回 TRUE，反之返回 FALSE。 

	close 
		回调函数类似于类的析构函数。 在 write 回调函数调用之后调用。 当调用 session_write_close() 函数之后，也会调用 close 回调函数。 此回调函数操作成功返回 TRUE，反之返回 FALSE。 

	read(string $sessionId)
		如果会话中有数据，read 回调函数必须返回将会话数据编码（序列化）后的字符串。 如果会话中没有数据，read 回调函数返回空字符串。 

		在自动开始会话或者通过调用 session_start() 函数手动开始会话之后，PHP 内部调用 read 回调函数来获取会话数据。 在调用 read 之前，PHP 会调用 open 回调函数。 

		read 回调返回的序列化之后的字符串格式必须与 write 回调函数保存数据时的格式完全一致。 PHP 会自动反序列化返回的字符串并填充 $_SESSION 超级全局变量。 虽然数据看起来和 serialize() 函数很相似， 但是需要提醒的是，它们是不同的。 请参考： session.serialize_handler。 

	write(string $sessionId, string $data)
		在会话保存数据时会调用 write 回调函数。 此回调函数接收当前会话 ID 以及 $_SESSION 中数据序列化之后的字符串作为参数。 序列化会话数据的过程由 PHP 根据 session.serialize_handler 设定值来完成。 

		序列化后的数据将和会话 ID 关联在一起进行保存。 当调用 read 回调函数获取数据时，所返回的数据必须要和 传入 write 回调函数的数据完全保持一致。 

		PHP 会在脚本执行完毕或调用 session_write_close() 函数之后调用此回调函数。 注意，在调用完此回调函数之后，PHP 内部会调用 close 回调函数。 

	open：session_start()，开启session
	read：开启session之后调用，将返回序列化的字符串，并初始化$_SESSION
	write：脚本结束之后调用

 */
function myopen(){
	echo 'open','<br/>';
}
function myread($sess_id){
	echo 'myread','<br/>';
	//该函数用于读取第一次序列化后的数据
	//必须返回序列化后的字符串，没有数据返回''
}
function mywrite($sess_id,$data){
	var_dump($sess_id);
	echo '<br/>';
	var_dump($data);
	echo '<br/>';
	echo 'mywrite','<br/>';

	//重写了write方法，php内置的write函数没执行，所以每次读取不到数据。
	/*
		第一次访问的时候，$_SESSION对象的数据已经被序列化作为参数传递进来，这时候要有你自己的存储方式。

		第二次访问的时候，初始化$_SESSION对象需要调用read函数返回的序列化字符串，否则无法初始化$_SESSION对象，自然也就获取不到$_SESSION对象的数据了。
	 */
}
function myclose(){
	echo 'myclose','<br/>';
}
function mydestroy(){
	echo 'mydestroy','<br/>';
}
function mygc(){
	echo 'mygc','<br/>';
}

session_set_save_handler('myopen','myclose','myread','mywrite','mydestroy','mygc');
session_start();


if(empty($_SESSION['name'])){
	$_SESSION['name']='zhangsan';
	echo 'first','<br/>';
}else{
	echo $_SESSION['name'];	
}
/*
说明：
		比如10点存储，存活期30分钟，10点就是存活边界，例如：
		10.15 - 30 = 9.45	未过期
		10.45 - 30 = 10.15 		过期
		11.00 - 30 = 10.30 		过期

		当前时间戳 - 最大存活时间 = session的存活边界
		存活边界 >= 10点的，都是过期的session，即10点(存活边界) >= (当前时间戳 - 最大存活时间)
	
	模拟：
		第一次写入的是session对象是10点，10点就是存活边界。

		第二次访问url，read()函数正常初始化，会按照 存活边界 >= time() - max_lifetime 条件来判断。
		但是wrrite()函数也被调用，存活边界被更新了。


		如果第二次访问或第二次以后的访问，write()函数不对 存活边界进行更新，那么read函数的判断是正确而且有效的。

		因为当超过session的有效期后，会查询不到数据，但是问题就出在这里，write重写了时间边界。
		也就是说我在30分钟内一直访问，这个$_SESSION就永远不会过期。

		因为开启session和脚本结束，这几个函数会被调用到。

		所以上面的代码有逻辑问题。
		正确做法是：写入的时候判断记录是否存在，如果存在，那么只更新记录。



	----------------------------

	判断文件是否过期
	
	文件：
		创建时间：这个点就是文件的有效期边界。
		最后修改时间

	缓存时间：文件的缓存时间

	公式：创建时间 >= (文件最后修改时间 - 缓存时间)，那么该文件就未过期。
 */