<?php
/*
从url到文件系统的映射
---------------------
1. DocumentRoot
	apache根据请求url定位文件的默认操作是：取出url路径中主机名和端口后面的部分，附加到DocumentRoot指定的文件系统路径后面。这样就组成了网上看到的基本文件树结构。

	如果服务器有多个虚拟主机，则Apache会使用下述两种方法之一：使用每个虚拟主机自己的DocumentRoot来组成文件系统路径，或者使用由mod_vhost_alias提供的指令基于IP地址或主机名动态地定位文件。

2. 反向代理
	

3. url重写引擎
	基于rewrite模块来进行url重写。
	
	此模块基于正则表达式来重写url请求，它支持每个完整规则，可以拥有不限数量的子规则，以及附加条件规则的url操作机制。

	此模块可以操作URL的所有部分(包括路径信息部分)，比如服务器变量、环境变量、HTTP报头、时间标记。

	在服务器级的(httpd.conf)和目录级的(.htaccess)配置都有效，还可以生成最终请求字符串。重写的url可以是内部子处理，也可以是外部请求的转向，甚至还可以是内部代理处理。

	2.1 特殊字符处理
		注意用 \ 来转义特殊字符。
		比如，Substitution可以用"\$"来包含一个美元符号，以避免mod_rewrite把它视为反向引用。


4. RewriteRule
	说明：定义重写引擎的重写规则，

	语法：RewriteRule pattern Substitution [flags]
	作用域：
		Server config、virtual host、Directory、.htaccess
	覆盖项：
		FileInfo
	
	说明：
		该指令是重写引擎的根本，可以多次使用，每个指令定义一个简单的重写规则，这些规则的定义顺序非常重要，在运行时，规则是按照这个顺序逐一生效的。

	pattern：
		一个作用于当前url的perl兼容的正则表达式。
		
		一般情况瞎，pattern是匹配url主机后面的字符串，
		例如：http://localhost/public/index.php，pattern表达式是匹配/public/index.php这段字符串。

	Substitution:
		用来替代pattern所匹配的的字符串。除了纯文本，还可以包含：
			a. 对pattern的反向引用($N)
				RewriteRule ^(.*)public$ $1，在这里$1是对正则表达式括号中所匹配内容的引用。
			b. 对最后匹配的rewriteCond的反向引用(%N)
			c. 规则条件测试字符串(${VARNAME})中的服务器变量
			d. 映射函数调用(${mapname：key|default})
		
		注："/"是指当前主机地址，例如：RewriteRule ^(.*)public$ /，满足匹配会用"/"替换，跳转后的地址即主机地址。

			"./"是指当前URI所在目录，例如：http://localhost/public/function/index.php，这个url的"./"当前目录即/public/function

	flags
		标记，作为rewriterule指令的第三个参数，多个标记以逗号分隔，有以下几个标记：

		'chain|C'
			链接下一规则，此标记使当前规则与下一个规则相连接，它产生这样的效果：如果一个规则被匹配，则继续处理其后继规则，也就是这个标记不起作用，如果该规则不被匹配，则其后继规则将被跳过。
			比如，在一个目录级规则中执行一个外部重定向时，你可能需要删除".www"(此处不应该出现".www")。 
				'cookie|CO=NAME:VAL:domain[:lifetime[:path]]'(设置cookie)
			在客户端设置一个cookie。cookie的名称是NAME，值是VAL。domain是该cookie的域，比如'.apache.org'，可选的lifetime是cookie的有效期(分钟)，可选的path是cookie的路径。 
		
		env|E=VAR:VAL
			设置环境变量，大致意思是可以用来引用url匹配的变量。

			此标记将环境变量VAR的值为VAL，VAL可以包含可扩展的正则表达式反向引用($N和%N)。此标记可以多次使用以设置多个变量。

			这些变量可以在其后许多情况下被间接引用，通常是在XSSI(<!--#echo var="VAR"-->)或CGI($ENV{'VAR'})中，也可以在后继的RewriteCond指令的CondPattern参数中通过%{ENV:VAR}引用。使用它可以记住从URL中剥离的信息。 

		forbidden|F(强制禁止URL)
			强制禁止当前URL，也就是立即反馈一个HTTP响应码403(被禁止的)。使用这个标记，可以链接若干个RewriteConds来有条件地阻塞某些URL。 

		'gone|G'(强制废弃URL)
			强制当前URL为已废弃，也就是立即反馈一个HTTP响应码410(已废弃的)。使用这个标记，可以标明页面已经被废弃而不存在了。 

		'handler|H=Content-handler'(强制指定内容处理器)
			强自制定目标文件的内容处理器为Content-handler。例如，用来模拟mod_alias模块的ScriptAlias指令，以强制映射文件夹内的所有文件都由"cgi-script"处理器处理。 

		'last|L'(结尾规则)
			立即停止重写操作，并不再应用其他重写规则。它对应Perl中的last命令或C语言中的break命令。这个标记用于阻止当前已被重写的URL被后继规则再次重写。例如，使用它可以重写根路径的URL('/')为实际存在的URL(比如：'/e/www/')。 

		'next|N'(从头再来)
			重新执行重写操作(从第一个规则重新开始)。此时再次进行处理的URL已经不是原始的URL了，而是经最后一个重写规则处理过的URL。它对应于Perl中的next命令或C语言中的continue命令。此标记可以重新开始重写操作(立即回到循环的开头)。但是要小心，不要制造死循环！ 

		'nocase|NC'(忽略大小写)
			它使Pattern忽略大小写，也就是在Pattern与当前URL匹配时，'A-Z'和'a-z'没有区别。 

		'noescape|NE'(在输出中不对URI进行转义)
			此标记阻止mod_rewrite对重写结果应用常规的URI转义规则。 一般情况下，特殊字符('%', '$', ';'等)会被转义为等值的十六进制编码('%25', '%24', '%3B'等)。此标记可以阻止这样的转义，以允许百分号等符号出现在输出中，比如：
			RewriteRule /foo/(.*) /bar?arg=P1\%3d$1 [R,NE]，可以使'/foo/zed转向到一个安全的请求'/bar?arg=P1=zed'。 

		'nosubreq|NS'(不对内部子请求进行处理)
			在当前请求是一个内部子请求时，此标记强制重写引擎跳过该重写规则。比如，在mod_include试图搜索目录默认文件(index.xxx)时，Apache会在内部产生子请求。对于子请求，重写规则不一定有用，而且如果整个规则集都起作用，它甚至可能会引发错误。所以，可以用这个标记来排除某些规则。

			使用原则：如果你为URL添加了CGI脚本前缀，以强制它们由CGI脚本处理，但对子请求处理的出错率(或者资源开销)很高，在这种情况下，可以使用这个标记。 

		'proxy|P'(强制为代理)
			此标记使替换成分被内部地强制作为代理请求发送，并立即中断重写处理，然后把处理移交给mod_proxy模块。你必须确保此替换串是一个能够被mod_proxy处理的有效URI(比如以http://hostname开头)，否则将得到一个代理模块返回的错误。使用这个标记，可以把某些远程成分映射到本地服务器域名空间，从而增强了ProxyPass指令的功能。 

			注意：要使用这个功能，必须已经启用了mod_proxy模块。

		'passthrough|PT'(移交给下一个处理器)
			此标记强制重写引擎将内部request_rec结构中的uri字段设置为filename字段的值，这个小小的修改使得RewriteRule指令的输出能够被(从URI转换到文件名的)Alias, ScriptAlias, Redirect等指令进行后续处理[原文：This flag is just a hack to enable post-processing of the output of RewriteRule directives, using Alias, ScriptAlias, Redirect, and other directives from various URI-to-filename translators.]。

			举一个能说明其含义的例子： 如果要将/abc重写为/def， 然后再使用mod_alias将/def转换为/ghi，可以这样：
			RewriteRule ^/abc(.*) /def$1 [PT]
			Alias /def /ghi

			如果省略了PT标记，虽然将uri=/abc/...重写为filename=/def/...的部分运作正常，但是后续的mod_alias在试图将URI转换到文件名时会遭遇失效。 

			注意：如果需要混合使用多个将URI转换到文件名的模块时，就必须使用这个标记。。此处混合使用mod_alias和mod_rewrite就是个典型的例子。

		'qsappend|QSA'(追加查询字符串)
			此标记强制重写引擎在已有的替换字符串中追加一个查询字符串，而不是简单的替换。如果需要通过重写规则在请求串中增加信息，就可以使用这个标记。 

		'redirect|R [=code]'(强制重定向)
			若Substitution以http://thishost[:thisport]/(使新的URL成为一个URI)开头，可以强制性执行一个外部重定向。如果没有指定code，则产生一个HTTP响应码302(临时性移动)。如果需要使用在300-400范围内的其他响应代码，只需在此指定即可(或使用下列符号名称之一：temp(默认), permanent, seeother)。使用它可以把规范化的URL反馈给客户端，如将"/~"重写为"/u/"，或始终对/u/user加上斜杠，等等。

			注意：在使用这个标记时，必须确保该替换字段是一个有效的URL。否则，它会指向一个无效的位置！并且要记住，此标记本身只是对URL加上http://thishost[:thisport]/前缀，重写操作仍然会继续进行。通常，你还会希望停止重写操作而立即重定向，那么就还需要使用'L'标记。 

		'skip|S=num'(跳过后继规则)
			此标记强制重写引擎跳过当前匹配规则之后的num个规则。它可以模拟if-then-else结构：最后一个规则是then从句，而被跳过的skip=N个规则是else从句。
			注意：它和'chain|C'标记是不同的！
		
		'type|T=MIME-type'(强制MIME类型)
			强制目标文件的MIME类型为MIME-type，可以用来基于某些特定条件强制设置内容类型。比如，下面的指令可以让.php文件在以.phps扩展名调用的情况下由mod_php按照PHP源代码的MIME类型(application/x-httpd-php-source)显示：

			RewriteRule ^(.+\.php)s$ $1 [T=application/x-httpd-php-source]

5. RewriteCond
	说明
		定义了一个规则的条件，即在一个RewriteRule指令之前有一个或多个RewriteCond指令。

	语法
		RewriteCond TestString CondPattern [flags]

	作用域
		server config, virtual host, directory, .htaccess
	覆盖项
		FileInfo
	模块
		mod_rewrite

	TestString
		是一个纯文本的字符串，但是可以包含扩展部分：RewriteRule的反向引用、RewriteCond的反向引用、RewriteMap扩展、服务器变量










2. 防盗链技术
	主要原理，利用referer头信息
	referer头信息：表示请求的来源信息。
	可以用于防盗链、统计用户从某个网址过来的访问量

	apache指令：

	#如果请求的来源中没有当前域名(www.localcache.com)，则不允许访问
	注意：是大括号
	#若有来源，则允许访问
	rewritecond %{HTTP_REFERER} !www.localcache.com
	rewriterule .*\.gif [F]
	rewriterule .*\.jpeg [F]

	rewriteconf：定义重写发生的条件，
	RewriteCond指令定义了一个规则的条件，即在一个RewriteRule指令之前有一个或多个RewriteCond指令。条件之后的重写规则仅在当前URI与pattern匹配并且符合这些条件的时候才会起作用。


    #expiresactive on
    #expiresbytype image/jpeg "access plus 10 days"
    #expiresbytype application/javascript "access plus 10 days"
    #<filesMatch "\.(gif)$">
			#header set cache-control "no-store,must-revalidate"
	#</filesMatch>
