<!--

apache手册
	参考手册模块：apache的整体使用流程。

	虚拟主机文档：虚拟主机使用说明。
	支持程序：bin目录支持程序使用说明。
	常见问题：一些问题举例。

	SSL/TLS加密：加密说明
	
	指令索引：罗列出所有指令说明。
	模块索引：罗列出所有模块说明

	注：查看apache手册首页引导进行学习。

1. apache
	一个轻量级的web服务器，也叫做apache超文本传输协议(HTTP)服务器，该服务器最大的特点是功能是以模块化来区分和加载的。
	Apache/conf目录下，存储了apache的配置信息，httpd.conf是apache的配置文件

	1.1 apache的模块
		加载模块语法：LoadModule module_name(模块名) module_dir(模块绝对路径)
		注1：模块名不能随意。
		注2：模块路径有空格，需要加上引号，例如："D:/Program Files/php/php5apache2_2.dll"

	1.2 编译apache
		windos平台的编译：
		解压apache源码，打开cmd窗口并切入目录，用apache make文件编译
		nmake /f Makefile.win _apacher	#release(发行版本)
		nmake /f Makefile.win _apached	#debug版本

2. apache、php、mysql整合配置
	2.1 apache加载php模块
		LoadModule php5_module "D:/Program Files/php/php5apache2_2.dll"
	
	2.2 加载php配置文件
		PHPIniDir "D:/Program Files/php"

	2.2 指定php模块处理的文件
		AddType application/x-httpd-php .php 	#将.php结尾的文件交给php模块处理
	
	2.3 开启php.ini的mysql扩展。
		extension=php_mysql.dll 	#加载php对应的mysql扩展
	
	2.4 指定php.ini的扩展文件目录
		默认：;extension_dir="ext" 修改 extension_dir=d:/.../php/ext

	说明：默认的php安装目录下游俩个配置文件，一个代表开发模式，一个代表生产模式，将生产模式的文件复制一份，修改成php.ini，用来配置php基本信息。
	

云虚拟主机
	虚拟主机和服务器是有差别的，虚拟的是在一台机器模拟了一个服务器环境，实际上一台机器上可能会模拟了n多个服务器环境，而服务器是真正的机器环境，需要你自己配置一系列运行环境，如ftp服务器、git服务器、web服务器

云服务器


ngrok服务器搭建
	让外网访问内网。
	ngrok是一个go语言的反响代理软件，通过在公共的端点和本地运行的web服务器之间建立起一个安全的通道。
	ngrok可以捕获和分析所有通道上的流量，便于后期分析和回放。

	1. 编译准备
		一个vps，阿里云或者其他，linux系统即可，这里我推荐debian或ubuntu.
		一个域名，将域名泛解析到你的服务器上。
		为服务器安装git和go

-->