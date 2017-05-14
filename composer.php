<?php
/*
Composer
	一个php的依赖管理工具，能根据你项目中声明所依赖的代码库，为你安装和管理它们。

	Composer并不是一个包管理器，默认情况下它不会在全局安装任何东西，只是在你项目的基础上进行管理，所以它仅仅是一个依赖管理工具。

1. 安装
	详见手册。

2. 使用
	a. 编写Composer.json配置文件
		在文件中声明项目所需要的类库。
		{
			"require" : {
				"monolog/monolog" : "1.0.*"
			}
		}
		require键需要一个包名称映射到包版本的对象。
		包名称：由供应商名称和其项目名构成。
		包版本：可以指定，详见手册

	b. 执行命令
		执行composer install，Composer会自动下载这些类库的，依照惯例放到vendor目录中，并生成一个autoload.php文件，这个文件加载了composer所下载的所有类文件。

		另外还会创建一个Composer.lock文件到项目的根目录中。

3. 自动加载
	对于库的自动加载信息，Composer生成了一个vendor/autoload.php文件，在项目中引入这个文件，可以得到一个免费的自动加载支持。

	Composer允许你自定义自动加载，详见手册。

4. composer.lock
	Composer初始使用会产生的文件，该文件锁定了你项目中依赖的类库版本。
	在你以后Composer安装类库的时候，会优先按照锁文件中的版本来进行安装类库，哪怕你定义了json文件。



 */