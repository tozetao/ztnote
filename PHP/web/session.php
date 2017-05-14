<?php
/*
1. session
	一种回话技术，通过cookie实现，session能存储用户在回话期间的数据，方便服务器使用。

	PHPSESSID:"n2q076nlu0e9dvs4jgl82vr625"
	session_name：PHPSESSID是session的名字。
	session_id：n2q076nlu0e9dvs4jgl82vr625是session的id，即会话的id，不同的回话sessionid是不一样的。

2. 启用
	session_start()
		默认的，session是关闭的，该方法用于启动新会话或重用现有会话，并初始化session变量。
	注：php.ini文件配置，session.auto_start=0，决定session的启用。

3. session使用
	同cookie一样，php将session信息保存在$_SESSION对象中供他人使用。
	
	$_SESSION['name']='hahahah';	//向session中写入数据。
	$name=$_SESSION['name'];	//从$_session中获取数据。

4. session的销毁
	5.1 session_destroy()
		将会话的session_id对应的文件进行删除。

	5.2 $_SESSION = array();
		让php序列化空的内容，session_unset()函数也是同样效果。

5. session机制
	6.1 启动新会话或重用现有会话
		session_start()
			启动新会话或者重用现有会话，并初始化$_SESSION。

			php会读取一个cookie名为PHPSESSID(session_name，会话名)的值，如果有值则根据该值(这个值其实是session_id)来创建session。
			如果没有php会生成一个唯一的session_id来初始化session。
			
			注：为了区分不同会话，session id是唯一且不同的，当然这是在同一台服务器下的情况。

	6.2 存储session对象数据
		当脚本执行结束会，$_SESSION存储的数据都会重新保存在服务器端，服务器端如何存储$_SESSION数据，默认是将对象序列化，以会话id(session_id)命名，存储序列化对象的字符串。

	6.3 将session写入响应头
		最后会将session_name和session_id以cookie形式写入响应头中。

	6.4 客户端接收到该cookie，之后在同一个会话内的每次请求都会携带该cookie，服务器会重用现有会话，以此来识别不同用户。

	一个窗口跟服务器的交互，你可以理解成一个会话。
	
6. session的存活时间
	6.1 存活时间
		由于session是使用cookie实现的，而cookie默认存活时间等于回话结束，所以session存活时间也等于回话结束。
		
	6.2 会话内的最大存活期
		session在同一个会话中，其存活时间也不行无限期的。

		session.gc_maxlifetime=1440;
		该配置用于设置session的存活时间。也即是回话未结束，当session超过这个存活时间，session也会自动失效的。
	
	6.3 动态设置session的存活期
		session_set_cookie_params()
			该用户用于设置回话cookie参数，详见手册。

		ini_get()
			获取运行期 ini 设置值。

		example：设置cookie存活时间，测试是否能影响session
			header('Content-type:text/html;charset=utf-8');
			session_start();

			if(empty($_COOKIE['PHPSESSID'])){
				setcookie(session_name(),session_id(),time()+10,'/');	
				//这行代码覆盖了php系统的session cookie
				//虽然也能修改session的有效期，但是客户端会多产生一个回话期session
			}

			//测试session存活时间
			if(!empty($_SESSION['name'])){
				echo $_SESSION['name'];
			}else{
				echo '这是session第一次赋值';
				$_SESSION['name']='hello';
			}

7. 会话和安全
	session.cookie_lifetime=0
		0 表示特殊含义，它告知浏览器不要持久化存储cookie数据。也即，关闭浏览器的时候，会话 ID cookie 会被立即删除。如果将此项设置为非0的值，可能会导致会话ID被其他用户使用。 

		大部分应用应该把此项设置为"0",如果应用中有自动登录的功能，请自行实现一种更加安全的方式，而不要使用会话ID来完成自动登录。 

	session.use_cookies=On 并且 session.use_only_cookies=On。 
		虽然 HTTP cookie 存在一些问题，但是它确实是实现会话 ID 管理的优选方案。 尽可能的仅使用 cookie 来进行会话ID管理，而且大部分应用也确实是只使用cookie 来记录会话ID的。 

	session.use_strict_mode=On
		此设置防止会话模块使用未初始化的会话ID。
		也就是说，会话模块仅接受由它自己创建的有效的会话ID，而拒绝由用户自己提供的会话ID。

		使用JavaScript对cookie进行注入就可以实现对会话ID的注入，甚至可以在URL的查询字符串中或者表单参数中实现会话 ID 的注入。

		大部分应用没理由也不应该接受由用户提供的未经初始化的会话 ID。 
	
	注：更多回话安全查看手册session章节。

8. 配置项
	session.save_handler=files
		数据保存方式，指session的保存方式，采用什么方式来保存。

	session.save_path="/tmp";
		数据保存路径。
		指定了session文件所存储的目录，如果这个路径没有开启，会保存到操作系统的临时目录。

	session.name=PHPSESSID
		session的名字
	
	session.auto_start=0;
		session是否自动开启，0，默认不开启。

	session.cookie_path=/
		session的作用域，表示sessionID能够在整个网站中都有效。
	
	session.cookie_domain=，
		cookie的跨域，默认为不能跨域。

	session的回收
	-------------
	session.gc_probability=1;
		被除数，垃圾回收分子
	session.gc_divisor=1000;
		除数，垃圾收回的分母
	
		说明：系统会有一个默认的机制，会有1000分之1的概率，它会把所有过期的session的文件都给删除掉。访问一千次有可能触发一次回收。
	
	文件分层
	--------
	session.save_path
		指定了session文件所存储的目录，如果这个路径没有开启，会保存到操作系统的临时目录。

		默认session数据以文件进行保存，当文件过多的时候，会影响文件的查找速度，所以可以对文件进行分层管理，相当于添加了个索引，增加序列化文件的查找速度。
		
		example：
			session.save_path="N;/path";
			//在php配置文件中，N是指文件夹的层级。
		
			session.save_path="d:/temp"; 相当于 session.save_path="0;d:/temp"

			session.save_path="1;d:/temp"
			//将所有文件分成1层。
		
			说明：
				子文件夹需要手动创建，命名是sessionID的第一个字母，例如：
				如PHPSESS_sdfsdfdsfsd，sessionID第一个字母是S，所以文件夹名是s。


session的默认存活时间为会话时间。
在会话时间内，有最大存活期。
通过cookie来实现的。


1. 服务端第一次初始化，创建一个唯一的session id，来区分不同会话。
2. 设置要存储的数据，以session id为文件名，序列化数据并存储起来
3. 将session id以cookie的形式写入响应头，让客户端保存。
4. 在会话期间，客户端都会携带该cookie给服务器
5. 服务器收到该cookie，会以该cookie存储的session id来创建会话，这样就为不同会话存储其各自的数据了。

6. 如果客户端cookie失效，重复步骤1。