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


	3.4 git fetch
		fetch抓取回来的数据，不会影响本地仓库，因为没有将远程仓库和本地仓库合并。
		抓取 + 合并 = 更新数据

		git fetch <远程主机名>
			一旦远程主机的版本库有了更新，需要讲更新取回本地，就需要fetch指令。
			git fetch命令通常用来查看其他人的进程，因为它取回的代码对你本地的开发代码没有影响。
			
		git fetch origin master
			默认情况下，git fetch取回所有分支（branch）的更新。如果只想取回特定分支的更新，可以指定分支名，如上所示，就是取回origin主机名的master分支。

			注：所取回的更新，在本地主机上要用"远程主机名/分支名"的形式读取。比如origin主机的master，就要用origin/master读取。

		git branch -r，如下显示：
			origin/maste
			git branch命令的-r选项，可以用来查看远程分支，-a选项查看所有分支。

		git branch -a，如下显示：
			* master
		  	remotes/origin/master
		  	上面命令表示，本地主机的当前分支是master，远程分支是origin/master。
		
		git checkout -b newBrach origin/master
			上面命令表示，在origin/master的基础上，创建一个新分支。

		git merge origin/master
		git rebase origin/master
			上面命令表示在当前分支上，合并origin/master。
	
	3.6 git push
		
			git push命令用于将本地分支的更新，推送到远程主机。它的格式与git pull命令相仿。

			

			
		


4. 多人协调
	4.1 推送分支
		master是主分支，因此要时刻与远程同步，一些修复性的bug分支不需要推送到远程区，可以先合并到主分支上，然后再把主分支master推送到远程区。

		命令： git push origin master，origin是远程库名称，master是当地分支。

	4.2 抓取分支
		git pull：将远程库全部分支抓取下来
		git branch --set-upstream dev origin/dev
	
	先推送
		推送失败，因为远程分支比本地分支数据跟新，尝试合并
			合并失败，解决冲突问题，并在本地提交。
	没有问题，完成推送

	注意：
		如果git pull提示“no tracking information”，
		则说明本地分支和远程分支的链接关系没有创建，用命令git branch --set-upstream branch-name origin/branch-name。

5. 创建与合并分支
	在版本回退里，每次提交，Git都会把它们串成一条时间线，这条时间线就是一个分支。
	截止目前为止，只有master一条主时间线，这个叫做主分支，即master分支。
	HEAD严格来说不是指向提交，而是指向master，master才是指向提交，所以HEAD指向的是当前分支。

	创建分支
		git branch branch_name 	#只创建分支
		git checkout -b name 	#创建分支并切到到新建分支上
		git checkout -b name，-b参数表示创建并切换到创建分支，相当于如下俩条命令：
	
	切换分支
		git checkout branch_name

	查看分支
		git branch

	合并分支
		branch1 merge brahch2

	删除分支
		git branch -d branch_name
	
	*************
	在不同分支上修改文件内容，提交后，不同分支显示的内容是不一样的。

	3.1 合并分支
		合并分支仅限于其中一个分支并未对同一个文件进行操作才能合并，否则会产生合并冲突。

	3.1 解决合并冲突
		创建一个分支：fenzhi1，并切换过来。
		修改一个文件内容并提交

		回到主分支，修改同一个文件内容提交。
		git merge fenzhi1，会产生冲突，如下所示：
		$ cat 123.txt
		11111111
		22222222
		33333333
		update
		after
		<<<<<<< HEAD
		999999999999999999999999999
		=======
		this is new branch
		8888888888888888888
		>>>>>>> fenzhi1

		Git用<<<<<<<<,============,>>>>>>>>>划分出不同分支内容，其中HEAD就是master分支内容，>>>>>>>fenzhi1就是fenzhi1上修改的内容。
		对于这种情况，我们可以在master查看内容，并进行修改，最后在提交。

		git log，可以查看分支合并的各种情况。

	4. 分支管理策略
		合并分支时，git一般使用fast forward模式，在这种模式下，删除分支后会丢失分支信息。
		我们可以用参数 -no -ff来禁用 fast forward模式。

		合并dev分支，使用命令：
			git merge –-no-ff -m “注释” dev

		example：
			git log --graph --pretty=oneline --abbrev-commit
			显示合并分支

		分支策略：首先master主分支应该是非常稳定的，也就是用来发布新版本，一般情况下不允许在上面干活，干活一般情况下都是在新建的dev分支上干活，干完后要发布，或者说dev分支代码稳定后可以合并到主分支master上来。

	5. bug分支
		在开发中总是会碰到bug，每个bug都可以通过一个临时分支来修复，修复后合并分支，然后将临时的分支删除掉。

6. 案例说明
	1. 克隆一个远程仓库后
		从无到有，将远程仓库所有内容都克隆下来。
		git clone xxxx

	2. 关联一个远程仓库后
		所谓关联，是指在你有本地仓库下关联才能成功，否则失败。
		git remote add hostname http://xxx.git
		git remote add -o hostname http://xxxx.git

	3. 更新本地仓库
		Git提供了俩种抓取远程仓库的方式，一种是fetch，一种是pull。

		抓取指定分支，合并指定分支
			fetch hostname branch_name
			git merge branch_name
		
		抓取所有内容，合并所有内容
			fetch hostname
			不知道

	4. 提交本地数据
		4.1 提交指定分支
			git push origin master:master
				本地分支如果跟远程分支没有关联，语法报错，需要指定推送的分支。
			
			推送冲突
				如果发生推送冲突，需要先讲数据fetch下来，进行合并，再提交再推送。

			解决合并冲突
				你肯定是add commit，之后推送数据出现错误。
				这时候先将数据抓取下来，再进行合并，出现冲突。

				查看哪里发生冲突了，修改，再add commit，再推送，成功。
		4.2 提交全部分支
			git push -all origin
				推送本地所有分支
	
 */