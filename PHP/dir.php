<?php
/*
1. 目录
	. 和 .. 是虚拟的目录，分别代表 当前目录 和 上一级目录,浏览器或者电脑，并没有路径概念，它是通过这俩个文件来确定路径的。		

2. 打开
	opendir(string $dir)
		函数说明：
			打开指定目录，该函数会将目录中所有的文件全部读入内存，包括子文件夹下面的所有文件，最后将目录指针指向第一个文件。

		返回值：
			返回一个资源句柄（路径资源）。
		
		注：./表示当前文件的路径，若使用chdir改变操作目录了，./表示改变后的操作目录。

3. 读取
	readdir(resources $dir)
		函数说明：
			从路径资源里读取一个对应文件，即当前文件指针所指向的文件，读取后会将文件指针下移一位。
			注：readdir不会读取子文件夹里的内容。
		
		参数：
			可以显式的使用资源参数，也可以不提供，因为系统会向上自动寻找文件路径资源。
		
		返回值：
			返回当前文件的文件名字，失败返回false。
	
	scandir(string $directory [, int $sorting_order [, resource $context ]] )
		返回一个 array，包含有 directory 中的文件和目录。 
	
	getcwd
		获取当前操作目录，返回目录字符串。

4. 更改
	chdir
		改变当前操作目录，表示进入到目标目录。
		注：改变当前操作目录的意思，就是程序已经进到这个目录里面来操作了，所以这时候，相对路径会出现问题.即代表了当前这个操作目录，而不是当前文件的目录了。

5. 创建
	mkdir
		创建文件夹，不能创建同名文件夹。
		注：linux下，要保证php对当前文件夹有权限进行读写操作。

6. 删除
	rmdir
		删除目录。
		注：只能删除空目录。

7. 释放目录资源
	closedir
		释放目录资源。

8. 相关函数
	rewinddir
		重置目录资源的指针，回到第一个文件(.文件)。

	is_dir
		 判断给定文件名是否是一个目录，如果文件名字存在并且是路径返回ture，不是或者不存在返回false。
		 是一个相对路径，则按照当前工作目录检查其相对路径。

	dirname 
		— 返回路径中的目录部分
		— 给出一个包含有指向一个文件的全路径的字符串，本函数返回去掉文件名后的目录名。 
	
	basename
		返回路径中的文件名。
		example：
			echo "1) ".basename("/etc/sudoers.d", ".d").PHP_EOL;
			echo "2) ".basename("/etc/passwd").PHP_EOL;
			echo "3) ".basename("/etc/").PHP_EOL;
			echo "4) ".basename(".").PHP_EOL;
			echo "5) ".basename("/");


9. 案例
	example：输入指定路径下的所有文件
		$dir=opendir('./');
		$dir_list=array();
		while(($filename=readdir($dir))!==false){
			$dir_list[]=$filename;
		}

		注：while(($filename=readdir($dir))!==false)，凡是使用赋值来进行判断，要注意返回值是0,null,""的问题。

		操作顺序：打开资源 => 读取资源 => 关闭资源
 */