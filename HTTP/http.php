<!-- 
/*
http协议
1. 原理
	HTTP协议，超文本传输协议，指的是俩台电脑进行数据传输过程中，需要遵守的一些规则。

	俩台电脑之间要实现数据传输，需要满足俩点
	a. 通过网线连接，tcp/ip协议.
	b. b/s结构的系统，数据的传输是遵守http协议。
	
	1.1 http协议特点
		无连接
			每次请求只会处理一次，返回一个结果。需要再获取其他数据则需要重新连接。每个连接只处理一个请求，
		
		无状态
			服务器在处理两次请求的时候区分不出来是否两个请求来自同一个客户端。
		
		支持客户端/服务器模式
			意思就是不限于b/s结构，客户端并不一定是浏览器。但是b/s结构都必须遵循HTTP协议。
	
	1.2 URL / URI
		URL
			Uniformed Resource Locator，统一资源定位，指的是通过http协议以及服务器的域名加上对应文件的路径组成的一个网络绝对路径
			http://www.itcast.cn/index.php
			http://www.itcast.cn/php/teacher/index.php

		URI
			uniformed resource identifier，统一资源标识，指的是一个文件在网站服务器相对网站根目录的路径
			/index.php
			/php/teacher/index.php

	1.3 http请求
		http请求分为4个部分。
		a. 请求行
			请求行由请求方法、URL、协议版本组成。
			例如：get/post www.baidu.com http/1.1

		b. 请求头(Request Headers)
			请求头就是一项一项的规范，这是协议的真正内容，一项规范占一行。
			example：
				host：所请求的主机地址
				
				accept-encoding：可以接受的数据的编码，不是字符集，而是什么流文件（文件内容）
				accept-language：可以接收的语言
				accept：浏览器可以接收服务器返回的数据类型

				referer：引用，指从哪个界面过来的（跳转过来）
				
				connection：keep-alive，连接，在本次请求的过程中，直到服务器把数据全部交还给浏览器，两者之间一直保持连接状态

				content-length（post）：请求主体字符长度，在以POST方式提交请求才会有这个选项。
				content-type（post）：提交的文件的类型

				cookie：浏览器携带提供给服务器的cookie数据（保存在浏览器cookie中的数据）
				
		1.3.3 空行
				将请求头和请求主体分割，用来区分heander和Content。
				
		1.3.4 请求主体
			在以post方式提交数据的时候才会有请求主体，即以POST方式提交的内容。

	1.4 http响应
		http想要也分为4个部分。
		a. 响应行
			协议版本：HTTP/1.1
			状态码：200
			状态描述：对状态码的说明

		b. Response Headers
			响应头包含了响应的一系列规范。
			example：
				date：响应的时间
				last-modified：文件最后被修改的时间
				
				content-length：响应主体的数据长度
				content-type：响应主体的数据类型
				
				location：重定向，立即重定向
				refresh：刷新，指定时间后重定向
				
				content-encodeing：数据内容的编码
				
				cache-control：缓存控制，no-cache表示告诉浏览器不要缓存当前请求的界面
				
				keep-alive：timeout=5，max=100，数据量比较大，所以会一部分一部分数据的传送，
					这里表示5秒超时，最大连接次数为100，所以这个页面数据一直没完的话，这个页面最多能维持5*100秒。如果超过了这个时间，那么连接就会终端。
		
		c. 空行
			用来区分响应头和响应主体

		d. 响应主体
			具体的响应数据

2. 模拟请求发送
	请求的模式主要是PHP+socket编程发送http请求，更高级的可以使用curl。
	example get：
		//1. 打开一个网络流
		$f=fsockopen($host,80,$errno,$error);

		//2. 封装http协议
		$host='localhost';	//主机名
		$http="GET /html/myfloat.html http/1.1 \r\n";	//2.1 请求行，不要请求自己，会是一个死循环
		$http.="Host:localhost \r\n";	//2.2 请求头
		$http.="\r\n";	//2.3 空行

		//3. 写入网络流
		if(fwrite($f,$http)){
			while($line=fgets($f,1024)){	//输出所有响应信息
				echo $line;
			}
		}

	example post：
		set_time_limit(0);    //脚本可以无限时间执行

		//1. 连接目标服务器Apache
		$f = fsockopen('localhost',80,$errno,$error);

		//2.	拼凑http协议
		$http = "POST /20140824/project/login.php HTTP/1.1\r\n";	//请求行
		$http .= "Host:localhost\r\n";	//请求头
		$http .= "Content-length:41\r\n";
		$http .= "Content-Type:application/x-www-form-urlencoded\r\n";
		$http .= "\r\n";	//空行
		$http .= "submit=submit&username=mark&password=1254";	//请求主体

		//3. 写给Apache服务器
		if(fwrite($f,$http)){
			//echo fgets($f);	//写入成功，解析资源
			while($line = fgets($f,1024)){
				//输出
				echo $line,'<br/>';
			}
		}
		/*
		while(1){
			//无限攻击Apache
			//fwrite($f,$http);
			//最简单的防止被恶意访问的方法：验证码
		}
		*/

	header()
		该函数可以对响应头进行设置。
		注：
			使用该函数时，不能在函数之前输出数据，因为输出内容的时候，php默认会去构造响应头，之后在继续使用header函数，会默认为响应主体，所以会失效。

3. telnet 
	telnet客户端能模拟发送请求去连接远程电脑的服务器（模拟浏览器）
	3.1 连接主机
		语法：telnet 主机 端口
		Ctrl+]，回车，就会出现回显功能，便于调试。
	
	3.2 模拟HTTP请求
		GET /php/demo.php HTTP/1.1
		host:localhost

4. 请求方法和状态码
	4.1 请求方法
		http协议的请求方法有：get、post、head、options、trach。

	4.2 状态码
		状态码用于反应服务器的响应情况，主要有1-5系列。

		1XX
			服务器接收请求，继续处理
		
		2XX
			表示处理成功处理，例如：200表示成功处理。
		
		3XX
			3系列的表示重定向。
			301：永久重定向。
				由A网站转转B网站，搜索引擎可以肯定网页A永久的改变位置了，这时候搜索引擎会把B网页当成唯一有效目标。

			302：临时重定向。
				网页更改地址后对搜索引擎不友好，可能会造成网址劫持。

			304：note Modified，内容未修改，表示取缓存内容。

			307：
				解决POST重定向丢失数据的问题，会将请求的数据传递给重定向的脚本页面。
				例如：
					POST表单提交到demo1.php页面，demo1.php页面脚本重定向到demo2.php页面，这种情况的话，表单提交的数据就丢失了。

		4XX
			请求错误。
			403：forbidden，请求了一个不该请求的东西
			404：not found，找不到文件

		5XX
			响应错误。
			502：gateway，服务器向上级请求时，上级返回失败

		每个状态码都有文字来描述。

	2.2 HTTP协议防盗链
		referer 属于header的一部分，当浏览器向服务器发送请求的时候，一般会带上referer，告诉服务器是从哪个页面链接过来的。

		防盗链原理：
			配置web服务器的转发协议，即URL重写。
			在web服务器层面，根据请求头的referer头信息来判断，如果来自站外，则统一重写到一个很小的防盗链提醒图片上面。
		
		具体步骤
			a. 开启apache重写模块，mod_rewrite
			b. 在需要防盗的网站目录，写.htaccess文件。
			c. 指定防盗链规则。

		重写规则
			重写条件，哪种情况下重写？
				是 .jpeg .jpg .png .gif请求
				是 referer请求头与主机地址不一致
			如何重写
				统一rewrite到某个防盗链图片
			
			RewriteEngine On	//开启重写引擎

			RewriteCond %{REQUEST_FILENAME} .*\.(JPG|JPEG|GIF|PNG) [NC]			//重写条件
			RewriteRule .* www.baidu.com		//重写规则
			//将所有图片都重写到www.baidu.com网址上面

			RewriteCond %{HTTP_REFERER} !localhost [NC]

			[NC]表示不区分大小写。
		
		注：若.htaccess配置文件放在哪个目录下，哪个目录下的文件就会收到影响，如果只是希望在某个目录下发生作用，可以Rewrite Base /admin 指令来指定监控的目录。

		虽然可以在php文件中进行分析，但是这样子就仍然请求了脚本文件了，我们应该是在http层面上去拦截。

	2.3 HTTP协议与缓存
		服务端：
			ETag：标记码、签名。
			Last-Modified：请求文件的上次修改时间
			这俩个响应头主要反应所请求的文件是否被修改。
		
		客户端：
			If-Modified-Since：如果自 XX时间 更改，理解成所请求文件的最后修改时间。
			If-None-Match：如果不匹配，理解成签名。

			这俩个请求头主要用于判断是否要使用浏览器缓存。
			客户端将签名和请求文件的修改时间传送给服务器，服务器进行判断，若标记码和请求文件最后修改时间都不变，那么返回304状态码，表示客户端可以使用本地缓存，同时会将签名写入响应头。
		
		2.3.1 流程说明
			第1次请求时：200 ok
			
			第2次请求时：304 Not Modified 未修改状态
			
			解释：在网络上，有一些缓存服务器，另外浏览器自身也有缓存功能。我们第一次访问，正常下载图片，返回200状态码，基于一个前提，图片不会经常改动，服务器返回200状态码的同时，还返回该图片的签名ETag和最后修改时间Last-Modified，第二次访问的时候，服务器会验证ETag和Modified，若图片没有变化，则返回304状态码，同时将签名和时间戳返回。
		
		2.3.2 cache-control
			cache-control：max-age=3600，缓存有效时间
			Expires：请求文件缓存失效的时间点
			Date：当前响应的时间

			如果网站比较大，有n台缓存服务器，那么这n台缓存服务器如何处理主服务器上的文件。
			a. 要不要缓存
			b. 缓存多久
			
			缓存服务器与主服务器之间应该有一些协议，来说明这2个问题？
			可以继续使用http协议，使用头信息cache-control来控制。做集群的时候，cache-control的作用就出来了。

			需要有相关模块：mod_expires
			a. 开启apache的expires扩展，利用该扩展来控制图片、css、html等文件，控制是否缓存及缓存生命周期
				注：这里是有缓存服务器才能看出作用，不过我们可以模拟。

			b. 写缓存规则，.htaccess文件
				ExpiresActive On	//开启缓存模块
				ExpiresByType image/jpeg "access plus 1 month"

				ExpiresDefault "<base> [plus] {<num><type>}*"
				设置默认的缓存参数
				ExpiresByType type/encoding "<base> [plus] {<num><type>}*"
				按照文件类型来设计独特的缓存参数
				Base：基于哪个时间点来计算缓存有效期
					Access/now：基于请求响应的那一瞬间，比如从此瞬间到一个月之后
					Modification：基于被请求文件的最后修改日期来计算，比如被修改日期后的1周内仍然有效。
				num：缓存时间的大小
				Type：缓存时间的单位
			
			设置后，如果这是在集群环境中，缓存服务器得到此图片，将会认为一个月内有效，这样减轻了主服务器的负担。

			该模块利用cache-control头信息，以及Etag、last-Modified头信息来进行缓存的控制。
		
		代码的实现应该跟静态化页面的实现差不多。
		先根据文件的最后修改时间和签名，判断文件是否发生变化，若没发生变化，在进行下一阶段判断。
		判断缓存时间是否过期，若没过期则返回缓存文件。

		是否需要使用缓存
			a. 文件是否改动
			b. 缓存是否过期
				expires：缓存过期时间的节点
				cache-control：要缓存时间

		2.3.3 取消缓存
			Control-cache：no-store,must-sevalidate
			意味着不允许缓存，必须去主服务器验证。
			
			使用：利用apache的header模块。
			开启后进行设置
			header set Cache-control "no-store,must-revalidate"
			<FilesMatch "\.(gif)$">
				Cache-Control:no-cache
				Header unset Etag
				Header unset last_Modified
			</FilesMatch>
		Powered-By-ChinaCache：HIT from 06888145SR
		指当前页面来自于中国缓存服务器节点，HIT 命中的意思。

	2.4 内容压缩
		Accept：text/html
		Accept-Encoding：gzip,deflate
		Accept-Language：zh-Cn
		上面三个请求头，用于告诉服务器，客户端所支持的压缩方式
		客户端发Accept-Encoding请求头，与服务器进行协商，告知服务器客户端所支持的压缩方式

		注：当我们在采集的时候，可以不发送Accept-Encoding信息，这样采集得到的是源码，当然也可以采集gzip，再用php解压gzip。

		Content-Encoding：gzip，响应文件编码
		Content-length：36187，请求/响应的主体长度,单位为字节

		例如，你请求一个页面，Content-length是36178个字节，但是将页面保存成txt文件，会发生内容增多。
		这是因为响应的主体，经过gzip进行压缩了，Content-length响应的是压缩后的主体内容长度。

		原理：
			为了提高网页在网络上的传输速度，服务器会对主体信息进行压缩，常见的：gzip压缩、deflate压缩、compress压缩、google chrome的sdch压缩。
		
		压缩过程：
			a. 服务器返回压缩内容
			b. 客户端接受压缩内容，进行解压，再渲染页面。
		
		apache压缩功能的启用
			a. gzip压缩模块 或 deflate压缩模块
			b. 指定压缩主体内容
				
				<ifmodule mod_deflate.c>
					DeflateCompressionLevel 6		//压缩等级1-9，推荐级别是6
					AddOutputFilterByType DEFLATE text/plain	//压缩文本文件
					text/html	//html文件
					text/xml	//xml文件
				</ifmodule>
			注：只能在conf配置文件中进行配置

		为什么要指定文件类型来压缩？
			压缩是需要消耗CUP资源的，另外图片、视频等文件，压缩效果不好，压缩不了多大的内容，所以一般是用来压缩文本格式的文件

	2.5 http协议与持久链接+分块传输 -> 反向ajax
		反向ajax又叫cornet、server push、服务器推技术

		应用范围：网页聊天服务器，新浪微博在线聊天器，google mail 网页聊天

		原理：
			一般而言，HTTP协议的特点，连接   断开
			服务器响应content-length，客户端收到指定length的内容时，也就断开连接了。
		在http1.1协议中，允许你不写content-length，比如要发送的内容长度确实不知道时，这时需要一个特殊的content-type：chunked，chunked分块的意思。

		分块传输的原理：
			发送123H\r\n
			则有123H个长度的内容传输给客户端，客户端就等着接受。
			
			41H\r\n
			客户端继续接受41H长度的内容

			直到最后发送0\r\n(服务器表示发送内容完毕)
			客户端接受到0\r\n，就知道数据传输结束

			利用这个特点，我们可以构造一个
		example：
			set_time_limit(0);	//让连接不超时
			ob_start();

			$i=1;
			$pad=str_replace(' ',4000);
			echo $pad;

			ob_fulush();
			flush();

			while($i++){
				echo $pad;
				echo $i;
				ob_fulush();
				flush();
				sleep(1);
			}

			这个页面始终没有与服务器断开连接，该连接随时有内容，就会输出给浏览器。
			如果while循环中，是数据库中的内容，或是俩个人之间的聊天记录呢？
			这样子就能达到即时通信，也就是说服务器端不间断的推送信息到客户端

	作业：写一个采集网站的class



重定向
	重定向是客户端行为，指的是web服务器发送302状态码以及对应新的location给客户端浏览器，浏览器发现是302状态码，则会发出一个新的http请求，请求的url是location地址。

	在这里location可以是任何网址。

转发
	转发是服务器行为，调用内部的一个方法在容器内部完成请求处理和转发动作。


