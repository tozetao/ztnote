<?php
/*
1. 安装laravel
	1.1 通过laravel安装工具
		首先使用composer下载laravel安装包，
		在下载安装完成后，便可以使用laravel命令来创建应用了。

	1.2 通过composer创建项目
		composer create-project laravel/laravel=5.0.* --prefer-dist
		

2. laravel配置
	laravel/config：框架的配置文件目录。
	
	2.1 环境配置
		/.env文件，用于配置应用程序的环境变量，例如APP_HOST、APP_DEBUG模式。
		通过这个配置文件，你可以在不同环境下加载不同配置，当然这个文件不应该提交到服务器上。

	2.2 配置缓存文件
		使用 Artisan 命令 config:cache 将所有的配置文件缓存到单一文件，能让框架快速加载。
	维护模式
	优雅链接

3. 目录结构
	blog：假设blog是你应用的根目录
	
	app
		应用程序目录，包括MVC，服务提供者等模块。

	app/Http
		该目录存放中间件、控制器、路由配置文件等
		
	app/Models
		laravel并没有这个目录，只是在创建模型的时候可以指定模型类的目录。
		一般情况下，laravel将模型放在app目录瞎。

	resources
		存放资源文件的目录，例如视图文件，语言包等

4. laravel的自动加载
	laravel通过composer来管理类的自动加载，只要你的类名和命名空间是正确的，composer的自动加载器就会帮你加载类所在的文件，规则如下：

	完整的类名结构：
		\命名空间 (\子命名空间*) \类名
		完整的类名必须要有一个顶级命名空间，vendor namespace

	如何根据完整的类名载入对应的文件：
		完整的类名中，去掉最前面的命名空间分隔符(\)，前面连续的1个或多个命名空间和子空间，将其作为命名空间前缀，这个命名空间前缀必须与至少有一个对应的文件根目录。
		
		例如：
			App\Http\Kernel，App\前缀 可能对应的根目录是 ./blog/app/Http

			Illuminate\前缀 可能对应的根目录是 blog\vendor/laravel/framework/src/Illuminate 

