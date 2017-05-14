<?php
/*
cookie
	将数据保存在客户端浏览器的一种技术。

1. 特点
	存储数据在客户端，方便我们下次对客户端进行处理，缺点就是不安全，因为数据在客户端浏览器中，容易伪造和被窃取。

2. cookie的设置
	cookie是http请求头的一部分，因此setcookie()函数必须在其他信息输出到浏览器之前被调用，这和对header()函数的限制类似，当然你可以使用输出缓冲函数来延迟脚本的输出，直到设置好了所有的cookie和其他的http请求头。

	bool setcookie ( string $name [, string $value [, int $expire = 0 [, string $path [, string $domain [, bool $secure = false [, bool $httponly = false ]]]]]] )

	函数说明
	--------
		在php中，是通过该函数来操作cookie。

	参数说明
	--------
		name
			cookie的名字。
		
		value
			cookie的内容。
			注：空字符串的cookie是无效的。
	
	说明：
		用于设置一个cookie，只要该函数执行，都会设置一个cookie，如果有同名的存活期cookie，会被覆盖。

3. cookie的获取
	在php中，请求所携带的cookie信息被封装成一个$_COOKIE全局对象，通过该对象来获取cookie的信息。
	例如：$_COOKIE['name']，获取cookie名是name的值。

4. cookie存活时间
	4.1 expire
		expire参数用于设置cookie的存活时间，该参数默认为0，即存活时间 = 回话时间。

		所以，不指定expire参数的值设置的cookie有效期时间等于会话时间，当关闭浏览器的时候，name cookie就失效了。

		setcookie('age','20',time()+10); 	//表示该cookie存活10秒
		setcookie('yourcookie','youvalue',time()-1);	//无效的cookie

5. cookie的作用域
	path
		cookie的作用域，一个cookie被设置后，只允许被当前脚本所在的文件夹和其子文件夹内的脚本访问，同级其他目录和父级目录是无权限访问的。

		example：//根目录作用域	
			setcookie('all','10090',time()+3600,'/');

5. 跨域
	domain
		默认的，cookie只能对当前域名有效(完整域名：有效的二级域名，如www.qidian.com)
		cookie跨域，指的是允许cookie在一级域名下的不同的二级域名之间共享数据。

		example：
			setcookie('www.example.com');	//只能在www.example.com域名使用
		
			setcookie('example.com');	//可以在example.com域名下的所有二级域名使用。
			
	域名的级别
		字符串.com这种形式的网址就是一级域名，在一级域名之前以.增加其他字符串，就是二级域名，例如：
		baidu.com是一级域名，
		3w.baidu.com是二级域名

6. cookie与http协议
	其实cookie就是一个文件，文件名对应cookie名字，文件内容对应cookie的值。
	
	cookie
	..........
	名称：demo
	内容：....
	主机：localhost
	路径：/myshop/
	发送条件：任意类型的连接
	过期时间：在会话结束时


	6.2 与协议的交互
		if(!isset($_COOKIE['name'])){
			setcookie('name','zhangsan'); //设置一个name的cookie，有效期为会话时间
		}
		
		用户第一次发出请求，会先检查是否有一个name的cookie，第一次访问肯定是没有的，这时候会创建一个cookie，写入到响应头中，反馈给用户。之后在会话期间内，只要用户有发出请求，都会将cookie写入请求头中携带过来。

7. cookie保存数组
	setcookie()函数无法直接保存数组。
	$arr=array(1,2,3,4,5);
	setcookie('arr',$arr);//错误的。

	//通过这种形式，将数组信息保存到cookie
	setcookie('arr[num1]',1);
	setcookie('arr[num2]',2);
	//下次请求的时候，php会当作数组来处理。




8. 微信内置浏览器的cookie
	
	微信端cookie的默认生命周期
		在PC端，cookie默认存活期等于会话期，而在微信内置浏览器，cookie默认存活期等于微信进程存活期。
		
		也就是说你在微信内置浏览器中设置了一个cookie，存活期默认0，在你关闭了微信浏览器窗口之后再进入页面，属于该脚本页面下的所有页面都可以访问这个cookie，除非微信进程被杀死或者重新进入，该cookie才会过期。

	微信端cookie 生命周期的设置
		在微信内置浏览器设置的cookie是有效的，关闭微信进程后再次进入，只要该cookie不过期，那么仍然是有效的。
