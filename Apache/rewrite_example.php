<?php
/*



Apache Rewrite
	Rewirte主要的功能就是实现URL的跳转，它的正则表达式是基于Perl语言，可基于服务器级的(httpd.conf)和目录级的(.htaccess)两种方式。

	注：基于目录级的(.htaccess)，必须打开此目录的FollowSymLinks属性且在.htaccess里要声明RewriteEngine on。

1. 案例说明
	下面是在一个虚拟主机里定义的规则。功能是把client请求的主机前缀不是www.updateweb.cn和203.81.23.202都跳转到主机前缀为http://www.updateweb.cn，避免当用户在地址栏写入http://updateweb.cn时不能以会员方式登录网站。

	请求头：
		NameVirtualHost 192.168.100.8:8080
		ServerAdmin
		DocumentRoot "/web/webapp"
		ServerName www.colorme.com.cn
		ServerName colorme.com.cn
		
	规则：
		RewriteEngine on #打开rewirte功能
		RewriteCond %{HTTP_HOST} !^www.updateweb.cn [NC] #声明Client请求的主机中前缀不是]的意思是忽略大小写
		RewriteCond %{HTTP_HOST} !^203.81.23.202 [NC] #声明Client请求的主机中前缀不是203.81.23.202,[NC]的意思是忽略大小写
		RewriteCond %{HTTP_HOST} !^$ #声明Client请求的主机中前缀不为空,[NC]的意思是忽略大小写
		RewriteRule ^/(.*) http://www.updateweb.cn/ [L] #含义是如果Client请求的主机中的前缀符合上述条件，则直接进行跳转到]意味着立即停止重写操作，并不再应用其他重写规则。这里的.*是指匹配所有URL中不包含换行字符，()括号的功能是把所有的字符做一个标记，以便于后面的应用.就是引用前面里的(.*)字符。

例二.将输入 folio.test.com 的域名时跳转到profile.test.com

listen 8080
NameVirtualHost 10.122.89.106:8080
ServerAdmin
DocumentRoot "/usr/local/www/apache22/data1/"
ServerName profile.test.com
RewriteEngine on
RewriteCond %{HTTP_HOST} ^www.updateweb.cn[NC]
RewriteRule ^/(.*) http://www.updateweb.cn/ [L]
3.Apache mod_rewrite规则重写的标志一览

1) R[=code](force redirect) 强制外部重定向
强制在替代字符串加上http://thishost[:thisport]/前缀重定向到外部的URL.如果code不指定，将用缺省的302 HTTP状态码。
2) F(force URL to be forbidden)禁用URL,返回403HTTP状态码。
3) G(force URL to be gone) 强制URL为GONE，返回410HTTP状态码。
4) P(force proxy) 强制使用代理转发。
5) L(last rule) 表明当前规则是最后一条规则，停止分析以后规则的重写。
6) N(next round) 重新从第一条规则开始运行重写过程。
7) C(chained with next rule) 与下一条规则关联

如果规则匹配则正常处理，该标志无效，如果不匹配，那么下面所有关联的规则都跳过。

8) T=MIME-type(force MIME type) 强制MIME类型
9) NS (used only if no internal sub-request) 只用于不是内部子请求
10) NC(no case) 不区分大小写
11) QSA(query string append) 追加请求字符串
12) NE(no URI escaping of output) 不在输出转义特殊字符
例如：RewriteRule /foo/(.*) /bar?arg=P1\%3d$1 [R,NE] 将能正确的将/foo/zoo转换成/bar?arg=P1=zed
13) PT(pass through to next handler) 传递给下一个处理
例如：
RewriteRule ^/abc(.*) /def$1 [PT] # 将会交给/def规则处理
Alias /def /ghi 
14) S=num(skip next rule(s)) 跳过num条规则
15) E=VAR:VAL(set environment variable) 设置环境变量

4.Apache rewrite例子集合

在 httpd 中将一个域名转发到另一个域名虚拟主机世界近期更换了域名，新域名为 www.updateweb.cn, 更加简短好记。这时需要将原来的域名updateweb.cn, 以及论坛所在地址 updateweb.cn/forums/定向到新的域名，以便用户可以找到，并且使原来的论坛 URL 继续有效而不出现 404 未找到，比如原来的http://www.updateweb.cn/forums/-f60.html, 让它在新的域名下继续有效，点击后转发到http://bbs.updateweb.cn/-f60.html, 这就需要用 apache 的 Mod_rewrite 功能来实现。

在中添加下面的重定向规则：

RewriteEngine On
# Redirect webhosting-world.com/forums to bbs.wbhw.com
RewriteCond %{REQUEST_URI} ^/forums/
RewriteRule /forums/(.*) http://bbs.wbhw.com/$1 [R=permanent,L]
# Redirect webhosting-world.com to wbhw.com
RewriteCond %{REQUEST_URI} !^/forums/
RewriteRule /(.*) http://www.wbhw.com/$1 [R=permanent,L]
添加了上面的规则以后， 里的全部内容如下：

ServerAlias webhosting-world.com
ServerAdmin admin@webhosting-world.com
DocumentRoot /path/to/webhosting-world/root
ServerName www.webhosting-world.com
RewriteEngine On
# Redirect webhosting-world.com/forums to bbs.wbhw.com
RewriteCond %{REQUEST_URI} ^/forums/
RewriteRule /forums/(.*) http://bbs.wbhw.com/$1 [R=permanent,L]
# Redirect webhosting-world.com to wbhw.com
RewriteCond %{REQUEST_URI} !^/forums/
RewriteRule /(.*) http://www.wbhw.com/$1 [R=permanent,L]
URL重定向

例子一:

1.http://www.zzz.com/xxx.php-> http://www.taobaoxs.com/xxx/
2.http://yyy.zzz.com-> http://www.taobaoxs.com/user.php?username=yyy 的功能

RewriteEngine On
RewriteCond %{HTTP_HOST} ^www.taobaoxs.com
RewriteCond %{REQUEST_URI} !^user\.php$
RewriteCond %{REQUEST_URI} \.php$
RewriteRule (.*)\.php$ http://www.taobaoxs.com/$1/ [R]
RewriteCond %{HTTP_HOST} !^www.taobaoxs.com
RewriteRule ^(.+) %{HTTP_HOST} [C]
RewriteRule ^([^\.]+)\.zzz\.com http://www.taobaoxs.com/user.php?username=$1
例子二：

/type.php?typeid=* --> /type*.html
/type.php?typeid=*&page=* --> /type*page*.html

RewriteRule ^/type([0-9]+).html$ /type.php?typeid=$1 [PT]
RewriteRule ^/type([0-9]+)page([0-9]+).html$ /type.php?typeid=$1&page=$2 [PT]
5.使用Apache的URL Rewrite配置多用户虚拟服务器

要实现这个功能，首先要在DNS服务器上打开域名的泛域名解析（自己做或者找域名服务商做）。比如，我就把 *.semcase.com和 *.semcase.cn全部解析到了我的这台Linux Server上。

然后，看一下我的Apache中关于*.semcase.com的虚拟主机的设定。

#*.com,*.osall.net
ServerAdmin
DocumentRoot /home/www/www.taobaoxs.com
ServerName dns.semcase.com
ServerAlias dns.semcase.com taobaoxs.com semcase.net *.taobaoxs.com *.semcase.net
CustomLog /var/log/httpd/osa/access_log.log" common
ErrorLog /var/log/httpd/osa/error_log.log"
AllowOverride None
Order deny,allow
#AddDefaultCharset GB2312
RewriteEngine on
RewriteCond %{HTTP_HOST} ^[^.]+\.osall\.(com|net)$
RewriteRule ^(.+) %{HTTP_HOST}$1 [C]
RewriteRule ^([^.]+)\.osall\.(com|net)(.*)$
/home/www/www.semcase.com/sylvan$3?un=$1&%{QUERY_STRING} [L]
在这段设定中，我把*.semcase.net和*.semcase.com 的Document Root都设定到了 /home/www/www.taobaoxs.com

但是，继续看下去，看到...配置了吗？在这里我就配置了URL Rewrite规则。
RewriteEngine on #打开URL Rewrite功能
RewriteCond %{HTTP_HOST} ^[^.]+.osall.(com|net)$ #匹配条件，如果用户输入的URL中主机名是类似 xxxx.semcase.com 或者 xxxx.semcase.cn 就执行下面一句
RewriteRule ^(.+) %{HTTP_HOST}$1 [C] #把用户输入完整的地址（GET方式的参数除外）作为参数传给下一个规则，[C]是Chain串联下一个规则的意思
RewriteRule ^([^.]+).osall.(com|net)(.*)$ /home/www/dev.semcase.com/sylvan$3?un=$1&%{QUERY_STRING} [L]
# 最关键的是这一句，使用证则表达式解析用户输入的URL地址，把主机名中的用户名信息作为名为un的参数传给/home/www/dev.semcase.com目录下的脚本，并在后面跟上用户输入的GET方式的传入参数。并指明这是最后一条规则（[L]规则）。注意，在这一句中指明的重写后的地址用的是服务器上的绝对路径，这是内部跳转。如果使用http://xxxx这样的URL格式，则被称为外部跳转。使用外部跳转的话，浏览着的浏览器中的URL地址会改变成新的地址，而使用内部跳转则浏览器中的地址不发生改变，看上去更像实际的二级域名虚拟服务器。

这样设置后，重启Apache服务器，测试一下，就大功告成了！





















-------------------

重写案例说明

	1．给子域名加www标记 
	RewriteCond %{HTTP_HOST} ^([a-z.]+)?example\.com$ [NC] 
	RewriteCond %{HTTP_HOST} !^www\. [NC] 
	RewriteRule .? http://www.%1example.com%{REQUEST_URI} [R=301,L] 
	这个规则抓取二级域名的%1变量，如果不是以www开始，那么就加www，以前的域名以及{REQUEST_URI}会跟在其后。

	2．去掉域名中的www标记 
	RewriteCond %{HTTP_HOST} !^example\.com$ [NC] 
	RewriteRule .? http://example.com%{REQUEST_URI} [R=301,L]

	3．去掉www标记，但是保存子域名 
	RewriteCond %{HTTP_HOST} ^www\.(([a-z0-9_]+\.)?example\.com)$ [NC] 
	RewriteRule .? http://%1%{REQUEST_URI} [R=301,L]
	这里，当匹配到1%变量以后，子域名才会在%2（内部原子）中抓取到，而我们需要的正是这个%1变量。 

	4．防止图片盗链 
	一些站长不择手段的将你的图片盗链在他们网站上，耗费你的带宽。你可以加一下代码阻止这种行为。 
	RewriteCond %{HTTP_REFERER} !^$ 
	RewriteCond %{HTTP_REFERER} !^http://(www\.)?example\.com/ [NC] 
	RewriteRule \.(gif|jpg|png)$ - [F] 
	如果{HTTP_REFERER}值不为空，或者不是来自你自己的域名，这个规则用[F]FLAG阻止以gif|jpg|png 结尾的URL 
	如果对这种盗链你是坚决鄙视的，你还可以改变图片，让访问盗链网站的用户知道该网站正在盗用你的图片。 
	RewriteCond %{HTTP_REFERER} !^$ 
	RewriteCond %{HTTP_REFERER} !^http://(www\.)?example\.com/.*$ [NC] 
	RewriteRule \.(gif|jpg|png)$ 你的图片地址 [R=301,L] 
	除了阻止图片盗链链接，以上规则将其盗链的图片全部替换成了你设置的图片。 
	你还可以阻止特定域名盗链你的图片： 
	RewriteCond %{HTTP_REFERER} !^http://(www\.)?leech_site\.com/ [NC] 
	RewriteRule \.(gif|jpg|png)$ - [F,L]
	这个规则将阻止域名黑名单上所有的图片链接请求。 
	当然以上这些规则都是以{HTTP_REFERER}获取域名为基础的，如果你想改用成IP地址，用{REMOTE_ADDR}就可以了。

	5．如果文件不存在重定向到404页面 
	如果你的主机没有提供404页面重定向服务，那么我们自己创建。 
	RewriteCond %{REQUEST_FILENAME} !-f 
	RewriteCond %{REQUEST_FILENAME} !-d 
	RewriteRule .? /404.php [L] 
	这里-f匹配的是存在的文件名，-d匹配的存在的路径名。这段代码在进行404重定向之前，会判断你的文件名以及路径名是否存在。你还可以在404页面上加一个?url=$1参数： 
	RewriteRule ^/?(.*)$ /404.php?url=$1 [L]
	这样，你的404页面就可以做一些其他的事情，例如默认信心，发一个邮件提醒，加一个搜索，等等。

	6．重命名目录
	如果你想在网站上重命名目录，试试这个： 
	RewriteRule ^/?old_directory/([a-z/.]+)$ new_directory/$1 [R=301,L]
	在规则里我添加了一个“.”（注意不是代表得所有字符，前面有转义符）来匹配文件的后缀名。 

	7．将.html后缀名转换成.php
	前提是.html文件能继续访问的情况下，更新你的网站链接。 
	RewriteRule ^/?([a-z/]+)\.html$ $1.php [L]
	这不是一个网页重定向，所以访问者是不可见的。让他作为一个永久重定向（可见的），将FLAG修改[R=301,L]。 

	8．创建无文件后缀名链接
	如果你想使你的PHP网站的链接更加简洁易记－或者隐藏文件的后缀名，试试这个: 
	RewriteRule ^/?([a-z]+)$ $1.php [L]
	如果网站混有PHP以及HTML文件，你可以用RewriteCond先判断该后缀的文件是否存在，然后进行替换： 
	RewriteCond %{REQUEST_FILENAME}.php -f 
	RewriteRule ^/?([a-zA-Z0-9]+)$ $1.php [L] 
	RewriteCond %{REQUEST_FILENAME}.html -f 
	RewriteRule ^/?([a-zA-Z0-9]+)$ $1.html [L]
	如果文件是以.php为后缀，这条规则将被执行。

	9．检查查询变量里的特定参数
	如果在URL里面有一个特殊的参数，你可用RewriteCond鉴别其是否存在： 
	RewriteCond %{QUERY_STRING} !uniquekey= 
	RewriteRule ^/?script_that_requires_uniquekey\.php$ other_script.php [QSA,L]
	以上规则将检查{QUERY_STRING}里面的uniquekey参数是否存在，如果{REQUEST_URI}值为script_that_requires_uniquekey，将会定向到新的URL。 

	10．删除查询变量
	Apache的mod_rewrite模块会自动辨识查询变量，除非你做了以下改动： 
	a).分配一个新的查询参数（你可以用[QSA,L]FLAG保存最初的查询变量） 
	b).在文件名后面加一个“?”（比如index.php?）。符号“?”不会在浏览器的地址栏里显示。

	11．用新的格式展示当前URI 
	如果这就是我们当前正在运行的URLs：/index.php?id=nnnn。我们非常希望将其更改成/nnnn并且让搜索引擎以新格式展现。首先，我们为了让搜索引擎更新成新的，得将旧的URLs重定向到新的格式，但是，我们还得保证以前的index.php照样能够运行。是不是被我搞迷糊了？ 
	实现以上功能，诀窍就在于在查询变量中加了一个访问者看不到的标记符“marker”。我们只将查询变量中没有出现“marker”标记的链接进行重定向，然后将原有的链接替换成新的格式，并且通过[QSA]FLAG在已有的参数加一个“marker”标记。以下为实现的方式： 
	RewriteCond %{QUERY_STRING} !marker 
	RewriteCond %{QUERY_STRING} id=([-a-zA-Z0-9_+]+) 
	RewriteRule ^/?index\.php$ %1? [R=301,L] 
	RewriteRule ^/?([-a-zA-Z0-9_+]+)$ index.php?marker &id=$1 [L]
	这里，原先的URL：http://www.example.com/index.php?id=nnnn,不包含marker，所以被第一个规则永久重定向到http://www.example.com/nnnn，第二个规则将http://www.example.com/nnnn反定向到http://www.example.com/index.php?marker &id=nnnn，并且加了marker以及id=nnnn两个变量，最后mod_rewrite就开始进行处理过程。
	第二次匹配，marker被匹配，所以忽略第一条规则，这里有一个“.”字符会出现在http://www.example.com/index.php?marker &id=nnnn中，所以第二条规则也会被忽略，这样我们就完成了。
	注意，这个解决方案要求Apache的一些扩展功能，所以如果你的网站放于在共享主机中会遇到很多障碍。

	12．保证安全服务启用
	Apache可以用两种方法辨别你是否开启了安全服务，分别引用{HTTPS}和{SERVER_PORT}变量： 
	RewriteCond %{REQUEST_URI} ^secure_page\.php$ 
	RewriteCond %{HTTPS} !on 
	RewriteRule ^/?(secure_page\.php)$ https://www.taobaoxs.com/$1 [R=301,L]
	以上规则测试{REQUEST_URI}值是否等于我们的安全页代码，并且{HTTPS}不等于on。如果这两个条件同时满足，请求将被重定向到安全服务URI.另外你可用{SERVER_PORT}做同样的测试，443是常用的安全服务端口 
	RewriteCond %{REQUEST_URI} ^secure_page\.php$ 
	RewriteCond %{SERVER_PORT} !^443$ 
	RewriteRule ^/?(secure_page\.php)$ https://www.taobaoxs.com/$1 [R=301,L]

	13．在特定的页面上强制执行安全服务 
	遇到同一个服务器根目录下分别有一个安全服务域名和一个非安全服务域名，所以你就需要用RewriteCond 判断安全服务端口是否占用，并且只将以下列表的页面要求为安全服务： 
	RewriteCond %{SERVER_PORT} !^443$ 
	RewriteRule ^/?(page1|page2|page3|page4|page5)$ https://www.taobaoxs.com/%1 [R=301,L] 
	以下是怎样将没有设置成安全服务的页面返回到80端口： 
	RewriteCond %{ SERVER_PORT } ^443$ 
	RewriteRule !^/?(page6|page7|page8|page9)$ taobaoxs.com%{REQUEST_URI} [R=301,L]