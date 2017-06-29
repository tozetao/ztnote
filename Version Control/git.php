<?php
/*

0. Control Version Tools
	意为版本管理工具，用于备份文件、记录历史、回溯过去、多端共享。
	与svn不同之处，在于git是一个分布式版本控制系统。。

	因为Git是分布式版本控制系统，所以需要填写一个邮箱和用户名作为标识。
	git config --global 参数，有global参数，表示这台机器的所有仓库都会使用这个配置。

	example：
		git config --global user.name 'aaaa'
		git config --global user.email 'xxx@qq.com'

1. git base
	仓库也是版本库，repository，你可以看做是一个目录，该目录所有文件和目录都能被git管理起来，每个文件的修改、删除，Git都能进行跟踪，以便任何时候追溯历史进行还原。

	1.1 初始化仓库
		git init
		用于创建一个版本库，输入该命令后，你的当前使用目录就会有一个.git文件。

	1.2 git add
		用于将文件添加到版本库中(暂存区)，方便git管理。

		example：
			git add 1.txt
			git commit

			将文件添加到缓存区，直接提交到版本库中。

		example：
			git status -s #查看是否有文件未提交
			git add 1.txt
			vim 1.txt 	#修改文件1，也可以在编辑器进行
			git status 	#能看到文件处于被修改中
			git diff 	#查看被修改的内容
			git commit

			查看文件被修改的内容
	
	1.3 git log
		该指令用于版本回退。

		git log：
			会从最近到最远的显示日志，我们可以看到最近的三次提交。

		git log --pretty=oneline
			会格式化简单显示

		git reset --hard HEAD^
			回退到上个版本，依照^以此类推。

		git reset --hard HEAD ~100
			指定回退多少个版本
			说明：版本回退会对文件造成影响的，并不是只有版本库才发生变化。

		git reset -–hard 版本号
			回退到指定的版本号

		git reflog
			显示所有commit内容的版本号			

		说明：
				
	


2. 理解工作区和缓存区
	2.1 工作区
		
	
	2.2 版本库
		

	2.3 内在说明
		git add
			实际上就是把文件添加到暂存区。
		git commit
			实际上就是把暂存区的所有内容提交到当前分支上，commit只会提交暂存区的内容。

3. 远程仓库
	远程仓库，即在其他服务器上的仓库，github的远程仓库是通过ssh加密的，所以要进行设置才能链接。

	3.1 与github远程仓库建立ssh连接
		a. 创建SSH Key
			查看主目录下有没有.ssh文件，如果有再看这个目录下有没有id_rsa和id_rsa.pub这俩个文件，如果有直接跳过此步，没有的话输入以下命令：
			ssh-keygen -t rsc -C 'youremail@example.com'。
			
			注：id_rsa是私人钥匙，不能泄露，id_rsa.pub是公共钥匙，可以说给别人。
		
		b. 登陆github，在setting添加key。
	
	3.2 git clone
		$ git clone <版本库的网址> <本地目录名>
		
		example：
			git clone https://github.com/jquery/jquery.git

		该命令可以讲远程仓库克隆到本地过来，会在本地主机生成一个目录，与远程主机的版本库同名，该指令的第二个参数能指定目录名。

		注：git clone支持多种协议。

	3.3 git remote
		为了方便管理，git要求每个远程主机都必须指定一个主机名，git remote用于管理主机名。



	
 */