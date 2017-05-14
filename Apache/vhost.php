<?php
/*
1. apache的虚拟主机
	虚拟主机是指一个机器上运行多个网站，例如：www.company1.com和www.company2.com。
	如果每个网站拥有不同的ip地址，则虚拟主机可以是基于IP的；如果只有一个ip地址，也可以是基于主机名，最终的实现对用户都是透明的。

	具体配置详见手册。

2. 简单配置说明
	定义虚拟机，需要在httpd.conf文件中加载虚拟机配置文件httpd_vxxxxconf.conf。
	
	#定义一个虚拟机，接受所有80端口的请求
	<VirtualHost *:80>
	    DocumentRoot "d:/demo"	#项目根目录
	    ServerName localhost	#主机名
		DirectoryIndex index.php index.html 	#访问目录时的默认html页面

		#以下是目录访问的权限限制
		<Directory "d:/demo">
			Options indexes
			Order Allow,Deny
			Allow From All
		</Directory>
	</VirtualHost>
 */