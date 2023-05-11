## Linux安装包概念
#### 1. 源码包
源码包大部分都是由c语言开发，要安装源码包需要将其编译成可执行的二进制文件，才能安装。
gcc：一个c语言编译器，要编译源码包需要该编译器。

##### 1.1 安装步骤
1. 通过./configure命令进行编译配置，这一步可用于定制功能，在这里会检测linux系统是否有编译该源码包时需要的库，执行成功会产生一个Makelife文件。

2. 执行make命令，根据Makeleft文件中预设的参数进行编译，这一步就是gcc在工作了。

3. 执行make install命令，安装步骤，生成相关的软件存放目录和配置文件的过程。

说明：可通过./config help查看帮助命令。
并不是所有的源码包都一样的，可以看源码包的readme和install说明。

##### 1.2 源码包与二进制包的区别
源码包解压后会有各种源码文件，例如头文件*.h，c代码源码文件*.c，c++源码文件*.cc/*.cpp等。

二进制包会有可执行文件，标识是其所在有名为/bin的目录，少数例外。

## rpm
rpm包就是二进制包，是经过编译的，不能看到源代码，但是安装更快，报错容易解决，只是有依赖问题。

rpm本身也是一个工具命令，通过该命令来安装rpm包。
#### 1. rpm包来源
rpm包在系统光盘中，可以通过 mount /dev/sr0 /mnt/cdrom 命令将光盘挂在到/mnt/chrom目录中，来查看系统光盘的Package目录下的安装包。

不同版本的linux光盘的文件目录似乎有些不同，例如Centos光盘下有个package目录，wubatu就没有。

#### 2. rpm包命名规则
例如：httpd-2.2.15-15.el6.centos.1.i685.rpm
- httpd：包名
- 2.2.15：软件版本
- 15：软件发布次数
- el6.contos：适合centos linux系统
- i686：适合的硬件平台，如果是x64，表示只适合64位操作系统
- rpm：包的扩展名

#### 3. rpm包的依赖性
rpm包比较麻烦的是按照是有其依赖性的，所谓的依赖性是指在安装某个rpm包的时候，该rpm包需要先安装其他的rpm包，这种就叫做依赖性。

依赖性主要分为：
- 树形依赖：a => b => c
- 环形依赖：a => b => c => a
- 模块依赖：模块依赖是指安装的rpm包需要某个库文件，以.so数组结尾的依赖报错就是库文件依赖报错，碰到这种情况只需要将库文件所对应的软件安装后就可以了。

www.rpmfind.net：查找库文件对应的包

#### 4. 包全名和包名
包全名：操作的软件包是没有安装的时候，就使用包全名，例如安装和升级。
包名：操作已经安装的软件包只需要使用包名就可以了，例如查询和卸载。 

#### 5. rpm包的安装信息
rpm包的安装信息：存储在/var/lib/rpm目录下的数据库文件
rpm包的安装路径：由rpm包作者指定或者安装系统的默认位置。

软件包的默认安装路径如下：
- /etc：软件配置文件的安装目录。
- /usr/bin：软件提供的可执行的命令安装目录
- /usr/lib：软件所使用的函数库文件的保存位置
- /usr/share/doc/：基本的软件使用手册保存位置
- /usr/share/man/：帮助文件保存位置

相对的源码安装包的位置是安装在/usr/local目录下。

#### 6. rpm命令
```
用法：
	rpm [options] 包名或包全名

参数说明：
	-i：安装，install。
	-q：查询，query。
	-U：升级，upgrade
	-e：卸载，erase
	-V：校验，verify
	
	-v：显示详细信息，verbose。
	-h：显示进度，hash

关于安装
	rpm -ivh 包全名

关于查询（-q参数的使用说明）
	rpm -q 包名
	# 查看该rpm包是否安装
	
	对于-q参数，你可以提供一些参数供使用查询，例如：
	rpm -qa
	# 查询全部已安装的rpm包，all的意思。
	
	rpm -qa | grep php
	# 查看php安装了哪些包

	rpm -qi 包名
	# 查询软件包信息，i是information。
	# -p，查询未安装的包信息
	
	rpm -ql 包名
	# 查询软件包各个文件的安装位置
	# l，list
	
	rpm -qf 文件名
	# 查询该文件所属哪个rpm包，前提是该文件时通过rpm包安装的
	
关于校验（-V参数）
	验证内容中的8个提示的具体内容如下：
	S：文件大小是否改变
	M：文件的类型或文件的权限(rwx)
	5：文件MD5校验和是否改变，可以看成文件的内容是否改变。
	D：设备的主从代码是否改变
	L：文件路径是否改变
	U：文件的属主(所有者)是否改变
	G：文件的属组是否改变
	T：文件的修改时间是否改变

	c：配置文件，config file
	d：普通文档，documentation
	g："鬼"文件，ghost file，该文件安不应该被RPM包包含。
	L：授权文件，license file
	r：描述文件，read me。
	
	你可以通过校验命令，来判断你的rpm包是否被人修改过。

	一般来说，文件都是通过rpm包安装的。	

关于卸载：
	rpm包提供了卸载命令，如果是源码包安装的话则是不提供卸载命令的，
	在linux中只要将安装的软件文件删除完毕，该软件就卸载干净了。

注1：安装和卸载都是有依赖性的。
注2：preparing...是只安装预准备，只有俩个进度都是100%才表示安装成功。

``` 

#### 7. rpm文件的提取方法
```
rpm2cpio 包全名 | cpio -idv .文件绝对路径

-rpm2cpio：将rpm包转换成cpio格式的命令
cpio：#是一个标准工具，它用于创建软件档案文件和从档案文件中提取文件。

cpio 选项<[文件或设备]
选项：
	-i：copy-in模式，还原
-d：还原时自动新建目录
-v：显示还原过程。

pwd：显示当前路径。

挂在的光盘是只读的，不能写


rpm2cpio /mnt/cdrom/Packages/corentils-8.4-19.el6.i686.rpm | cpio -idv ./bin/ls
将rmp包作为cpio文件来提取，.是代表当前文件，/bin/ls是要提取的文件。

```

## yum
rpm安装因为依赖性的问题很繁琐，所以推出yum，yum是linux系统的软件包管理器。

yum安装会自动解决依赖性问题，它的缺点是不能进行rpm包的查询和包校验，如果要查询rpm包是否安装和校验仍然需要使用rpm命令。

注：redhat的yum是需要付费的，centos不需要。

#### 1. yum源文件
/etc/yum.repos.d/
该目录存放了yum的源文件，*.base是基本源文件，默认生效的，其余几个yum源文件是不生效的。

yum文件内容说明：
```
[base]
	基本yum源，容器名称，一定要放在[]中。
	name
		容器说明，随意写。
	mirrorlist
		镜像站点，可以注释掉
	baseurl
		yum源服务器的站点，默认是CentOS官方的yum源服务器，也可以自己修改yum源地址
	enabled
		此容器是否生效，如果不写或写成enabled=1都是生效，0是无效。
	gpgckeck
		如果是1是指RPM的数字证书生效，如果是0不生效。
		为了rpm包的安全性，是要开启的。
	gpgkey
		rpm数字证书的位置，centos linux系统装好后，这个证书是默认存在的。
		例如：file:///etc/pki/rpm-*目录下。

		file://表示文件协议，就像http://表示http协议一样

[updates]
	更新yum源
```
当然你可以替换yum源的下载地址，换成国内的镜像地址。
wget命令下载yun源文件替换后

yum clean all
yum makecache

#### 2. yum源搭建
```
yum源的搭建
在无网络的情况下，使用光盘来进行安装。

mkdir /mnt/cdrom：
	建立挂载点

mount /dev/cdrom /mnt/cdrom
	挂在光盘，mount /dev/sr0
	mount是挂载命令 ，/dev/cdrom是光驱，/cd是你要挂载到什么位置

让yum源失效，编辑yum源base文件，将enabled设置为0或者将yum源文件改名，然后重新编写一个yum源文件。

例如：
	[base]
		name = ...
		baseurl = file:///mnt/cdrom，将地址改成光盘挂载点，其他不变。

yum list查看yum rpm包列表。
```

#### 3. yum命令
```
用法：
	yum [option] action

参数说明：
	list，查询所有可用的软件包列表
	search，查询yum源服务器上与关键字有关的包
	
	install，安装包
	update，更新升级
	remove，卸载

关于查询：
	yum list php
	# 查询php的可安装包

	yum search 关键字
	# 搜索服务器上所有和关键字相关的包，并不能查询本地的包。

关于安装
	yum -y install 包名
	yum -y install gcc
	# -y，自动回答yes，安装gcc软件包
	# yum安装是不需要包全名的，只需要包名即可。

关于更新
	yum -y update 包名
	# 将指定的软件包进行更新
	# 不太了解咋个更新法
	
	注意：
		yum -y update
		# 如果不加包名，是包括升级所有软件包以及linux内核版本的升级，新内核升级的话，需要一定配置才能正常使用，也就是说你升级内核的时候，系统是会先奔溃，配置完成之后才能使用的。

	升级的话，需要你的服务器上有更高级别的版本包，否则是无法升级的。
	一般情况下，如果当前的包没有漏洞或者完全问题，建议使用稳定版本即可。

关于卸载
	yum -y remove 包名
	# 卸载指定的包，yum卸载会将所有依赖该包都卸载掉，如果要卸载建议用rpm命令来卸载。

关于软件安装原则
	服务器安装软件建议最小化安装，用什么就装什么，尽量不卸载。
	因为按照的rpm包邮依赖性问题，卸载的时候也会有依赖性。

其他参数
	yum grouplist
		列出所有可用软件组列表
	
	yum groupinstall 组名
		软件组名
		安装指定软件组，组名可用通过grouplist查询。
	
	yum groupremove 组名
		移除执行软件组
```

```
gcc：c语言编译器，将程序语言转换为机器语言。

LAN=en_US，改变系统语言
LANG=zh_CN.utf8
linux本机是不支持中文的，哪怕你是用中文安装的系统，但是显示出来都是英文。

#在linux中是注释，且必须在第一行的第一个字符，前面不能有空格
```

## LAMP yum安装
```
mysql安装
	yum -y install mysql mysql-server
	
	chkconfig --level 35 on
	# 将mysql服务自启动
	
	/etc/init.d/mysqld start
	# 启动mysql服务

	mysqladmin -u root password 123456
	# 修改root用户密码
	
	
	vim /etc/my.conf
	# 编辑mysql配置文件
	
	# /usr/bin，该目录中存放了mysql的脚本命令
	# echo $PATH，输出path路径也可以看到/usr/bin目录
	# 所以像mysql、mysqladmin等命令才能执行
	
	# /usr/bin/mysqladmin -u root -h localhost localdomain password 'new-password'
	# /usr/bin/mysqladmin -u root password 'new-password'
	
apache安装
	yum -y install httpd
	
	chkconfig --level 35 httpd on
	# 设置服务自启动

	/etc/init.d/httpd start
	# 启动服务
	
	vim /etc/httpd/conf/httpd.conf
	# 编辑httpd web服务器的配置文件

	# apache配置
	Listen 80
	ServerName 127.0.0.1:80
	DocumentRoot "/var/www/html"	# 修改apache的web根目录
	DirectoryIndex index.php index.html index.htm

	service httpd restart
	# 重启apache服务器

php安装
	yum -y install php
	
	service httpd restart
	# 重启web服务器服务

	yum -search php
	# 搜索php组件
	
	# php常用组件
	# php-mysql
	# php-gd
	# php-imap
	# php-ldap
	# php-odbc
	# php-pear
	# php-xml
	# php-xmlrpc
	
	# php-mbstring
	# php-bcmath

	# php配置文件
	vim /etc/php.ini
	mermory_limit = 128m
	post_max_size=8m
	upload_max_filesize=2m

# 如果你安装成功也启动成，但是无法在外部访问web服务，请检查linux的防火墙
# service iptables status
# service iptables stop
```