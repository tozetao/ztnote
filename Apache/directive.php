<?php
/*
指令说明
版本：apache2.2
------------------
	apache中，指令可以用于配置服务器该如何运行。
	指令有5个属性，分别是默认值、作用域、类型、状态、所属模块。

	作用域说明
		它表示指令出现在配置文件的什么位置才是合法的，指令应该仅仅出现在允许出现的作用域中，否则会报错或者无法启动web server。
	
	类型
		directive-type，即指令的类型。有如下几种类型：
			AuthConfig：允许使用认证授权相关的指令。
			Limit：允许使用控制主机访问的指令(Allow Deny Order)
			FileInfo：允许使用控制文档类型的指令，例如mod_rewrite模块、mod_actions模块的一系列指令
			Indexes：允许使用控制目录索引的指令。
			Options：。。。
	
	所属模块
		即指令是属于什么模块的，例如RewriteRule是属于rewrite模块的。

	ServerName
		127.0.0.1:80；主机名

	DocumentRoot
		webroot，即发布目录，发布在这个目录下的项目都会被装载成标准的web工程，修改成我们自己发布的目录，例如：d:/project/

	<Directory/>
	    Options FollowSymLinks
	    AllowOverride None
	    Order deny,allow
	    deny from all 	#访问限制，修改成allow from all
	</Directory>

	<Directory/>元素用于控制目录的访问权限和重写机制等。


	<Directory "d:/demo">
		Options all 	#不知道为什么要改成all，该能重写成功。
		AllowOverride All
		Order Allow,Deny
		Allow From All
	</Directory>

	Directory
		说明：
			封装一组指令，使之对某个目录及其子目录生效。
		语法：
			<Directory directory-path> ... </Directory>
		作用域：
			server config, virtual host

		directory-path
			该参数可以是一个完整的路径，也可以是一个正则表达式。
		
		多个指令的应用情况
			服务器将以短目录优先的规则进行应用，并包括.htaccess文件中的指令，例如：
			<Directory />
				AllowOverride None
			</Directory>
			<Directory /home/>
				AllowOverride FileInfo
			</Directory>

			访问文档/home/web/dir/doc.html的步骤如下：
			a. 应用指令AllowOverride None(禁用.htaccess文件)。 
			b. 应用指令AllowOverride FileInfo(针对/home目录)。 
			c. 按顺序应用所有/home/.htaccess 、/home/web/.htaccess 、/home/web/dir/.htaccess中的FileInfo组指令。 
			注：dir-path如果是正则表达式，将在所有普通的directory指令应用后才考虑。

	Options
		说明：配置在特定目录中使用哪些特性。
		默认值：Options All
		作用域：Service config、virtual host、directory、.htaccess
		特性
		----
		All 
			除MultiViews之外的所有特性。这是默认设置。 
		
		ExecCGI 
			允许使用mod_cgi执行CGI脚本。 
		
		FollowSymLinks 
			服务器允许在此目录中使用符号连接。 
			注意：即使服务器会使用符号连接，但它不会改变用于匹配<Directory>段的路径名。

			注意：如果此配置位于<Location>配置段中，则此设置会被忽略。

		Includes 
			允许使用mod_include提供的服务器端包含。 
		
		IncludesNOEXEC 
			允许服务器端包含，但禁用"#exec cmd"和"#exec cgi"。但仍可以从ScriptAlias目录使用"#include virtual"虚拟CGI脚本。 
		
		Indexes 
			如果一个映射到目录的URL被请求，而此目录中又没有DirectoryIndex(例如：index.html)，那么服务器会返回由mod_autoindex生成的一个格式化后的目录列表。 
		
		MultiViews 
			允许使用mod_negotiation提供内容协商的"多重视图"(MultiViews)。 
			SymLinksIfOwnerMatch 
			服务器仅在符号连接与其目的目录或文件的拥有者具有相同的uid时才使用它。 
			注意：如果此配置出现在<Location>配置段中，此选项将被忽略。

		example：
			<Directory "d://pro">
				Options FollowSymLinks	#服务器允许在此目录中使用符号链接，这里可以进制浏览目录
			</Directory>

	Order

	Allow

	AllowOverride
		说明：确定允许存在于.htaccess文件中的指令类型。
		语法：
			AllowOverride All|None|directive-type[directive-type]...
		默认值：
			AllOverride All
		作用域
			Ditectory
		当服务器发现一个.htaccess文件时，它需要知道这个文件中声明的哪些指令能覆盖在此之前配置的指令。
	
	AccessFileName
		说明：分布式配置文件的名字
		语法：AccessFileName filename
		默认值：AccessFileName .htaccess
		作用域：
			Server config、virtual host
		example：
			AccessFileName .acl
			在返回文档 /user/locl/web/index.html 之前，服务器会为此指令读取/.acl，/user/.acl、/user/locl/.acl、/user/locl/web/.acl，除非此功能被禁用。
	


 */