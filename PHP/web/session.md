

## Session
session是一种回话技术，它能够识别用户并且在回话期间存储用户的数据到服务器端。

### 1. Session使用
- 开启session
```php
session_start();
# 开启session，php.ini配置默认关闭session的
# session.auto_start选项配置session的默认开启
```

- 使用session
```php
$_SESSION['name'] = 'zhangsan';
# 将值保存在SESSION数组中，数据将会保存在服务器上面
```

- session销毁
```php
session_destory();
# php session销毁函数，将回话session_id对应的文件删除

$_SESSION = array();
# 让php序列化空对象也是删除的一种方式
```

### 2. Session原理
session本质是用cookie来实现的，在php中session_start()函数与session最重要。

当请求过来时，session_start()会负责初始化$_SESSION变量，它会读取一个叫做PHPSESSID的cookie，如果请求有携带该cookie，会使用该cookie的值作为文件名去读取session目录中存储的文件数据；如果请求没有携带该cookie，php会根据回话生成一个唯一的session_id，并将其作为cookie相应给客户端。

数据存储方面默认是序列化$_SESSION变量转为字符串，并以session_id作为文件名，序列化的字符串作为内容来存储。

这大体便是php Session的实现原理了。

### 3. Session生命周期
session本质是cookie实现的，cookie默认过期时间等于回话时间，所以session在回话结束后就会过期。

当然session在回话期间内的最大存活时间是1440秒，这是在php.ini配置文件中设置的。

- session.gc_maxlifetime=1440：session回话期间内的最大存活时间


example：
```php
session_start();

if(isset($_SESSION['name'])){
	echo $_SESSION['name'];
}else{
	$_SESSION['name'] = 'zhangsan';
}

// 通过cookie修改session过期时间
if(!isset($_COOKIE[session_name()])){
	setcookie(session_name(), session_id(), time() + 3600);
}

# 在使用cookie来修改session过期时间时，查看请求的相应头我们能发现会有俩个Set-Cookie相应头，因为也证明了session_start()函数是这是了cookie的响应头的

// 使用php函数来修改session过期时间，具体看手册
session_set_cookie_params()

```




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

