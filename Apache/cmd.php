<?php
/*
apache服务器的支持程序
	4.1 httpd
		属于apache服务器的主程序，被设计为一个独立运行的后台进程，它会建立一个处理请求的子进程或者线程池。
		httpd.exe可用于启动关闭apache或输出模块信息，更多操作语法详见手册的httpd。

		说明：
			在windows平台，通过配置环境变量(path指向apache的bin目录)或者在cmd窗口进入apache的bin目录，可以使用httpd.exe程序。
 */