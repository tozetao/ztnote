<?php
/*
文件上传
--------
1. 普通文件上传
	1.1 客户端
		在html模板的表单中，需要设置为二进制传输，否则服务器解析不了。
		example：
			<form enctype="multipart/form-data"></form>
		
		说明：post默认提交字符流数据，若没有设置enctype属性，提交的是文件名。
	
	1.2 服务器端
		a. 文件被上传后，上传的信息会封装到$_FILES全局变量中。
		同时默认地会被储存到服务端的默认临时目录中，除非 php.ini中的upload_tmp_dir设置为其它的路径。

		b. 进行一些列安全检查，例如：
			通过$_FILES['userfile']['type']检查文件类型，
			通过$_FILES['userfile']['size']检测文件大小
			$_FILES['userfile']['error']判断文件错误

		c. 使文件上传生效，由于转存在服务器的临时目录中，当脚本结束，$_FILES变量会释放，临时目录的文件会被删除，所以需要拷贝或者转存到其他目录。
			move_uploaded_file(path1,path2)：将文件直接移动到目标路径，不会保留原文件

	1.3 $_FILES对象
		a. $_FILES['userfile']['name']
			客户端机器文件的原名称。 

		b. $_FILES['userfile']['type']
			文件的 MIME 类型，如果浏览器提供此信息的话。一个例子是"image/gif"。不过此 MIME 类型在 PHP 端并不检查，因此不要想当然认为有这个值。 

		c. $_FILES['userfile']['size']
			已上传文件的大小，单位为字节。 

		d. $_FILES['userfile']['tmp_name']
			文件被上传后在服务端储存的临时文件名，例如：
			 "C:\Windows\Temp\phpEBE8.tmp" 

		e. $_FILES['userfile']['error']
			和该文件上传相关的错误代码。此项目是在 PHP 4.2.0 版本中增加的。 

		多文件上传，一般模板目录是设置file的name值为 name[]，$_FILES是这样访问的，例如：$_FILES['name']['type'][0]

	1.4 php.ini文件配置相关
		file_uploads：
			是否允许文件上传。

		upload_max_filesize
			允许上传单个文件的最大值，默认"2M"。

		post_max_size
			php能够接受表单的最大值。
		
		upload_tmp_dir：
			文件上传到服务器产生的临时文件夹，默认值NULL。
			如果没有指定路径，默认实用系统的临时目录。

		max_file_uploads：
			一次上传文件允许的最大数目，默认20

		max_execution_time
			脚本最大执行时间。


大文件上传
	将一个大的文件切割成一份份的小文件进行顺序上传。

客户端文件切割
	1. flash
		使用flash对文件进行切割的缺陷是大文件首先要加载到客户端机器的内存中，所以上传的文件是无法图片可用内存的限制的。
	
	2. html5
		html5没有完全普及，会有浏览器兼容问题。
	
	3. activeX
		这种只针对IE浏览器，activeX可用切割文件，切割后的文件上传到服务器之后，需要通过程序进行拼接到一个完整的文件。

	由于大文件上传需要考虑中断问题，所以上传文件需要记录上传到哪一块的位置信息。
	上传过程使用什么协议实现，有http协议、简单php也好处理，也可以通过socket来上传，可以了解websocket、socket.io以及ruby在这方面的实现方案。

方案
	文件切割：flash + html5
	协议：http协议
	后台：php对文件进行拼接。



php手册
	支持的协议和封装的协议一章
		访问各个输入输出流
		
php://

 */