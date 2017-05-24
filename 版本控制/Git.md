## Git简介
Git是一个版本控制管理工具，用于备份文件、记录历史、回溯过去、多段共享。与SVN不同的是Git是一个分布式版本控制系统。

在Git中有仓库的概念，英文名是reporitory，所谓的仓库可以看作是一个目录，Git通过管理这个目录下的所有文件和目录，每个文件的修改Git都能进行跟踪，以便任何时候回溯还原。

## Github的关联配置
使用Git我们不需要自己部署版本控制服务器，可以将数据存储在Github上面，Github就是一个版本控制的服务器。

具体配置如下：
```
git config --global user.name "humingx"
git config --global user.email "humingx@yeah.net"

ssh-keygen -t rsa -C "humingx@yeah.net"
# 生成秘钥

eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa
# 添加密钥到ssh-agent

# 最后在github上面配置key即可
```

## 基本操作

### 1. 初始化仓库
- git init
该命令会在本地初始化一个仓库，用于版本控制，执行命令后在本地会发现一个.git的隐藏目录。

### 2. 管理文件
- git add
将文件会将文件添加到版本库中，Git才能管理该文件，在这一步中会将文件添加到缓存区。

example：
```
git status -s #查看是否有文件未提交
git add 1.txt
vim 1.txt 	#修改文件1，也可以在编辑器进行
git status 	#能看到文件处于被修改中
git diff 	#查看被修改的内容
git commit
```

### 3. 提交仓库
- git commit
该命令会将缓存区中的文件提供到本地仓库中，方便与服务器进行同步。

### 4. 版本回退
- git log
log命令会从最近到最远的显示日志，可以看到最近的三次提交，如果要格式化的简单显示，可以在log命令会加--pretty=oneline参数

- git reset
log命令是来查看仓库的提交日志，reset命令则用于回退指定的版本。

example：
```
git reset --hard HEAD^
# 回退到上个版本，依照^以此类推。

git reset --hard HEAD ~100
# 指定回退多少个版本，
# 注：版本回退会对文件造成影响的，并不是只有版本库才发生变化。

git reset -–hard 版本号
# 回退到指定的版本号

git reflog
# 显示所有commit内容的版本号		
```

注意：所有的版本管理系统只能跟踪文本文件的改动，图片、视频这些二进制文件按，没法跟踪文件按的变化。

### 5. 撤销修改
- git checkout --filename
该命令可以丢弃工作区的修改。

example：
```
git checkout -- 123.txt
# 该命令会将123文件在工作区的修改全部撤销，有俩种情况：
# 一种是123文件修改后还没放入暂存区，撤销后跟回退版本一模一样的状态；另外一种是123文件已经暂存过一次，接着修改，撤销后就回到暂存后的初始状态。

注意：--参数非常重要，如果没有--，命令就变成创建分支。
```
		
### 6. 删除文件
- rm filename
删除指定的文件

## 缓存区与工作区
### 1. 缓存区
工作区有一个.git隐藏目录，这个目录是版本库。

版本库存储了很多东西，最重要的就是stage(缓存区)，还有Git为我们创建个一个master分支以及指向master分支的一个指针HEAD。

### 2. 工作区
working tree，就是你在电脑上看到的目录。比如d盘的demo文件是你的版本库，demo下的文件或者以后新建的目录文件都属于工作区的范畴。

git add命令实际上是将文件存储到缓存区，而commit命令会将缓存区中的修改提交到当前的分支上。
add、commit等命令操作的是.git本地仓库，要与github服务器同步需要pull或fetch、push命令

## 远程仓库管理
前面说过Git是一个分布式的版本控制系统，所以不需要部署版本控制服务器，而是将文件提交到Github上面，Github你可以理解成一个版本控制服务器，通过将文件同步在服务器中，这样便可以在不同客户端上共享和同步数据了。

### 1. 建立ssh通道连接
github的远程仓库是通过ssh加密的，所以要进行设置才能连接，这样才能同步数据。

### 2. clone
- git clone <版本库的网址> <本地目录名>
clone是一种与远程仓库建立数据同步的一种方式，它可以将远程仓库复制到本地下，会在本地生成一个与远程仓库同名的目录。

### 3. remote
remote命令也可以与远程仓库建立连接，更主要的是用于管理远程主机名，为了管理方便，Git要求每个远程主机（远程仓库）必须指定一个主机名，Git默认主机名叫做origina。

- git remote add <主机名> <网址>
将远程主机与本地仓库关联起来，前提是已经有本地仓库，如果远程主机有文件，需要先进行同步。

- git remote
列出所有远程主机。

- git remote -v
使用-v选项，能查看远程主机的网址。

- git remote show <主机名>
查看该主机的详细信息。

- git remote rm <主机名>
用于删除远程主机。

- git remote rename <原主机名> <新主机名>
用于远程主机的改名。

- git clone -o JQuery http://xxxxjquery.git
克隆版本库的时候，所使用的远程主机自动被Git命名为origin，如果想指定主机名，需要用clone的-o选项指定。

### 4. fetch
抓去远程仓库的数据，抓取的数据需要合并才能在本地发生作用。
抓去+合并=同步数据

```

```

### 5. pull
pull命令相当于fetch、merge俩个命令功能的集合，它会取回远程仓库的分支再进行合并。

语法：git pull <主机名> <远程分支名>:<本地分支名>
```
git pull origin next
# 取消origin主机的next分支并于当前分支合并，由于是合并当前分支，所以:后面的部分可以省略
```

在某些场合，Git会自动在本地分支与远程分支之间，建立一种追踪关系（tracking）。
比如，在git clone的时候，所有本地分支默认与远程主机的同名分支，建立追踪关系，也就是说，本地的master分支自动"追踪"origin/master分支。

在这种情况下，使用pull命令可以省略远程主机分支名。Git也允许手动建立追踪关系。

git branch --set-upstream master origin/next
上面命令指定本地master分支追踪origin主机的next分支，这样使用git pull master会直接与远程主机进行数据同步。

example：
```
git pull origin
# 上面命令表示，本地的当前分支自动与对应的origin主机"追踪分支"（remote-tracking branch）进行合并。
# 如果当前分支只有一个追踪分支，连远程主机名都可以省略。

git pull
# 上面命令表示，当前分支自动与唯一一个追踪分支进行合并。
# 如果合并需要采用rebase模式，可以使用--rebase选项。

git pull --rebase <远程主机名> <远程分支名>:<本地分支名>
# 如果远程主机删除了某个分支，默认情况下，git pull 不会在拉取远程分支的时候，删除对应的本地分支。这是为了防止，由于其他人操作了远程主机，导致git pull不知不觉删除了本地分支。
# 但是，你可以改变这个行为，加上参数 -p 就会在本地删除远程已经删除的分支。

git pull -p
# 等同于下面的命令

git fetch --prune origin 
git fetch -p
```
### 6. push
push命令用于将本地的分支的更新推送到远程主机的分支上，
格式为：git push <远程主机名> <本地分支名>:<远程分支名>

如果省略远程分支名，则表示将本地分支推送与之存在"追踪关系"的远程分支（通常两者同名），如果该远程分支不存在，则会被新建。

注：分支推送是<来源地>:<目的地>，所以git pull是<目的地>:<来源地>，也就是<远程分支>:<本地分支>



		git push origin master
			上面命令表示，将本地的master分支推送到origin主机的master分支。如果后者不存在，则会被新建。
			如果省略本地分支名，则表示删除指定的远程分支，因为这等同于推送一个空的本地分支到远程分支。

		$ git push origin :master
		# 等同于
		$ git push origin --delete master
			上面命令表示删除origin主机的master分支。
			如果当前分支与远程分支之间存在追踪关系，则本地分支和远程分支都可以省略。

		$ git push origin
			上面命令表示，将当前分支推送到origin主机的对应分支。
			如果当前分支只有一个追踪分支，那么主机名都可以省略。
			git push

			如果当前分支与多个主机存在追踪关系，则可以使用-u选项指定一个默认主机，这样后面就可以不加任何参数使用git push。

		$ git push -u origin master
			上面命令将本地的master分支推送到origin主机，同时指定origin为默认主机，后面就可以不加任何参数使用git push了。

			不带任何参数的git push，默认只推送当前分支，这叫做simple方式。此外，还有一种matching方式，会推送所有有对应的远程分支的本地分支。Git 2.0版本之前，默认采用matching方法，现在改为默认采用simple方式。如果要修改这个设置，可以采用git config命令。

			$ git config --global push.default matching
			# 或者
			$ git config --global push.default simple
		
		git push --all origin
			还有一种情况，就是不管是否存在对应的远程分支，将本地的所有分支都推送到远程主机，这时需要使用--all选项。

			上面命令表示，将所有本地分支都推送到origin主机。

			如果远程主机的版本比本地版本更新，推送时Git会报错，要求先在本地做git pull合并差异，然后再推送到远程主机。这时，如果你一定要推送，可以使用--force选项。

		git remote add origin https://github.com/tozetao/mydemo.git
			添加一个远程仓库，并且主机名为origin

		git push -u origin master
			将本地仓库内容推送到远程库，并把当前分支master跟远程仓库的master分支关联起来。
		
		本地库推送成功，就可以把本地master分支的最新修改发送到github上了。
		example：
			git push origin master

		注：failed to open http://github.xxx.git，推送失败的问题
		解决方案：进行代码合并，git pull --rebase origin master，这时候就可以跟新远程库了，注：pull=fetch+merge。
