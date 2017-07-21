### echo
用于向终端输出字符串，配置-e选项才能才能输出反义字符

example：
```
echo 'this is demo';

echo -e "echo 3 lines\n\n\n";

echo 'the log file have be done' > log.txt
# 重定向输出

echo "hello world\""
# 引号是一个特殊字符，在输出时需要转义
```

### read
read命令用于在标准输入中读取一行，并把输入中的每个字段的值赋予shell变量。

example：
```
read name age address
echo $name;
echo $age;
echo $address;
```

### cat
显示文件内容，创建文件，还可以用于显示控制字符。
注：在文件分页处不会停下来，会显示整个文件内容，因此可以把cat命令的输出通过管道符传递到另外一个具有分页功能的命令中。

命令格式：cat [options] filename1...

options选项：
- -n：由1开始对所有输出的行数编号
- -b：同-n，只不过对于空白不编号
- -s：当遇上有连续俩行以上的空白行，就代替为一行的空白行
- -v：显示非打印字符

example:
```
cat file1
# 显示file1文件内容

cat file1 file2 file3
# 显示多个文件内容

cat file1 file2 file3 > bigfile
# 将多个文件的内容重定向到新文件中

$ cat
hello world
first
<ctrl + d>
$
# 新建文件

cat /dev/null > file
# 清空file文件内容

cat -b httpd.conf
# 输出文件内容并编号

cat -s /etc/X11/XF86Config | sed '/^[[:space:]]*$/d'
# 使用sec与cat去除空白行
```

### more

### tee
读取标准输入的数据，并将内容输出成文件。

格式：tee -options file1 file2 ...

options选项：
- -a：将内容附加到文件后面，而不覆盖。
- -i：忽略终端信号
- --help
- --version：显示版本信息

example：
```
tee teedemo.txt
# 读取标准输入内容，并输出到文件中。

cat demo.txt |tee file1 file2
# 显示demo.txt文件内容并把内容复制到其他文件中

ls -l |tee file 
```


### 管道符
通过管道把一个命令的输出传递给另外一个管道作为输入，管道用竖杆|表示。

格式： 命令1 | 命令2，其中|是管道符

### Linux命令执行顺序
linux命令执行顺序，||和&&和;
- command1 && command2：左边的命令执行成功，右边的命令才会执行
- command1 || command||：左边的命令执行失败，右边的命令才会执行
- command;command2：命令顺序执行

example：
```
cp test.txt test_bak.txt && cat test_bak.txt
# 拷贝成功则显示拷贝的文件

cata || touch b.txt
# 第一个命令执行失败，第二个命令会被执行

pwd;date;more d.txt
```


## linux命令
linux的命令和参数区分大小写，可以用tab键来补全命令。
命令格式：command [options] params，command是指令，option是选项，参数可以有多个，如果命令的长度多长，可以用 \ 来换行输入。

注：在linux命令行中，#为root用户，s为普通用户。

## 基础命令

### pwd
显示当前工作目录

### ls
ls用于显示目录，ls -options dirctoryname

选项：
- -s，显示所有文件，包括隐藏文件
- -l，文件以长格式显示
- -t，文件以修改时间先手显示
- -R，显示目录及下级子目录结构，一般会用tree命令代替
- -d，显示指定目录
- -m，横向输出文件夹，并以“，”作分隔符
- -s，以文件大小排序

ls命令输出时，不同的颜色代表不同的文件。
- 白色：普通文件。
- 红色：压缩文件。
- 蓝色：目录文件
- 浅蓝色：链接文件
- 绿色：可执行文件

### mkdir
用法：
		mkdir [option] 目录名
	
	选项：
		-p，一次性创建整个目录树。

### rmdir
	删除目录
	用法：
	rmdir [参数] 目录名
### rm
删除文件和文件夹
	用法：
		rm [参数] 	文件
	参数：
		-r 删除整个目录树
		-f 强制执行
	rm -rf，一定要谨慎使用，有的时候rm -rf /将会把根目录下所有文件删除，所以rm -rf最好用非超级用户来做，因为非超级用户没有很多权限。

```


tree
	显示文件和目录树，能够将目录以一棵树的形式显示出来。
	用法：
		tree [参数] 文件
	参数：
		-a，
		-d，只显示目录不显示文件
		-f，每个文件都显示路径
		-t，根据最后修改时间排序
		-L n，只显示n层目录（n为数字）

touch
	创建空文件和更改时间戳
	用法：
		touch [参数] 文件
	参数：
		-d，更改时间戳为自定义的值，格式为：20101005

cp
	复制目录
	用法：
		cp [参数] 原文件 目标文件
	参数
		-r，递归执行（可复制目录树）

mv
	移动或重命名文件
	用法：
		mv [参数] 原文件 目标文件 

cat
	由第一行开始显示文件的内容，因为会同时显示所有内容，
tac
	从最后一行开始显示文件内容

file
	显示文件类型

more
	一页一页的显示档案内容。
	空格：代表向下一夜。
	enter：代表向下翻一行。
	/字符串：代表在这个显示的内容当中，向下搜索字符串这个关键词。
	:f，立即显示出文件名以及目前显示的行数
	b或ctrl+b，代表往回翻页
	q:，退出

head
	显示文件开始几行
	用法
		head [option] filename
	参数：
		-n number查看前几行的内容
tail
	与head相反，是显示结尾几行。
```


### history
```
history
	用于查看命令的历史记录
	history -c
	#该命令可以清除当前用户输入的命令记录
```

##### 3. 环境变量
echo $PATH
该命令用于输出系统环境变量，与windows一样的道理。
在linux中，你输入的命令系统都会在环境变量中去寻找的。

### grep
```
grep
	一个强大的文本搜索命令，能使用正则表达式搜索文本，并把内容打印出来。

	-c，只输出匹配行的计数
	-i，不区分大小写（只适用于单字符）
	-h，查询多文件时只输出包含匹配字符的文件名。
	-l，查询多文件时只输出包含匹配字符的文件名
	-n，显示匹配行及行号
	-s，不显示不存在或无匹配文件的错误信息。
	-v，显示不包括匹配文本的所有行。
	-o，只输出匹配的部分，默认是输出匹配的全部信息。

example：
	管道符，在输入命令的时候以 | 连接，后面跟命令。
	netstat -plant | grep -v 80
	#显示80端口服务	

	ps aux | grep ls
	ps aux是查找服务
	
	sort -r 反向排序
	ps aux | grep -i BASH | sort -r
```


### 关机相关命令
linux不同于windows系统，它是有运行级别的，各级别如下：
- 0：系统关机状态
- 1：单用户工作状态，用于维护
- 2：多用户模式（NFS来启动）
- 3：多用户模式，字符界面
- 4：系统未使用，留给用户自定义
- 5：多用户模式，并且在系统启动后运行X Window，给出一个图形化的登录窗口
- 6：所有进程被终止，重新启动。

一般多用户模式使用的多，3、5。

```
sync
	在当前的模式下同步数据，将内存中的数据同步写入硬盘。
	在切换系统模式的时候使用，因为切换系统模式的时候，有些数据是在内存中的，如果需要做一次硬盘同步。

shutdown [参数] 时间 [警告信息]
	-t sec：送出警告信息和删除信息之间要延迟多少秒
	-k：并不真正关机而只是发出警告信息给所有用户
	-r：关机后立即重新启动
	-h：关机后停止系统
	一般这个命令是在多个用户的使用下才使用的，给其他人提示关机信息

init
	切换系统模式

runlevel
	查看当前系统模式级别
	
reboot init6
	用于重启系统，例如：reboot init6，该命令将重启到指定的模式
```

## 文件压缩
```
gzip
	gzip [option] file
	-c，将输出重定向到标准输出
	-d，解压缩文件
	-r，对目录递归，将里面的文件分别压缩
	-1..9，知道压缩办理
	文件后缀.gz

bzip2
	bzip压缩

tar
	用于创建、列出、抽取归档文件，归档文件通常也会一并压缩
	tar [option] file
	-c，创建归档文件
	-x，释放文档
	-v，显示详细信息
	-f，文件名（可带路径）
	-z，使用gzip压缩
	-j，使用bzip2压缩
```
