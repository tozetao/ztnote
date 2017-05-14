## Linux服务

### 1. 系统运行级别
linux的运行级别有7种，
- 0：关机
- 1：单用户模式，类似windows的安全模式，主要用于系统修复
- 2：不完全的命令行模式，不包括NFS服务
- 3：完全的命令行模式，就是标准字符界面
- 4：系统保留模式，没有使用的模式
- 5：图形模式
- 6：重启动

与系统运行级别相关命令：
```
runlevel
# 查看当前运行级别
# 例如：N 3，N是null，3是当前运行级别；n字母是表示在进入系统之前的运行级别。

init
# 切换当前系统的运行级别，例如：init 5，切换到图形界面。
# 注：使用init命令来重启和关键，由于是调用系统级别模式来关机，它并不一定会正确结束系统服务，
# 所以仍然是建议使用shutdown命令。
```

/etc/inittab，该文件配置系统默认开机级别，内容大体是"id:3:initdefault:"，通过修改该文件可以设置系统默认的运行级别。

### 2. 服务的分类
Linux的服务根据安装方式可以分为俩种：
- RPM包默认安装的服务
- 源码包安装的服务

一般根据安装位置来区分俩种服务，RPM包服务一般是由包作者来决定安装位置，源码安装服务一般是安装在/usr/local/目录下，而RPM包服务又可以分为：
- 独立的服务
- 基于xinetd服务

独立的服务分别运行在内存中，可以随时访问，xinetd服务又被叫做超级守护进程，xinetd本身也是独立的服务，也是运行在内存中。

xinetd服务管理了一些后台程序，如果用户需要xinetd管理的某个服务，需要先访问xinetd，再通过xinetd访问对应的服务，相应也需要通过xinetd，使用xinetd服务优点在于只需要xinetd占用内存空间，它所管理的服务是不需要占用内存空间的，虽然这回影响访问速度。

说明：在当前linux系统中，基于xinetd服务越来越少了，基本快淘汰，centos7已经剔除了。

## 独立服务(RPM包服务)管理
以下是rpm包安装目录：
- /etc/init.d：启动脚本目录
- /etc/sysconfig：初始化环境配置文件目录
- /etc：配置文件目录
- /etc/xinetd.conf：xinetd配置文件
- /etc/xinetd.d：基于xinetd服务的启动脚本
- /var/lib：服务产生的数据存放目录
- /var/log：日志文件目录

rpm包安装目录是由作者决定的，大体是一致的，如果rpm包是基于xinetd管理的，对应的配置文件和启动脚本会在/etc/xinetd目录下。

### 1. chkconfig
chkconfig命令主要用于管理RPM包的自启动状态。

Linux服务分为启动和自启动，
- 服务启动是指让服务在当前系统中运行，并提供功能
- 服务自启动是指让服务在系统开机或重启动之后，随着系统的启动而自动启动服务
```
chkconfig --list
# 查看RPM包服务列表的自启动状态
# 注：查看的状态是自启动状态，该服务是否在当前系统中启动是未知的

chkconfig --list [name]
# 指定显示某个RPM包服务的自启动状态

chkconfig --add name
# 增加一项新的服务

chkconfig --del name
# 删除服务，并把相关符号连接从/etc/rc[0-6].d删除

chkconfig mysqld on 
# 指定某个服务自启动状态为on
 
chkconfig --level 35 mysqld on 
# 设置mysql服务在3、5模式下自启动
```

### 2. ntsysv
ntsysv命令与chkconfig命令相同，也是管理服务的自启动命令，同样是redhat专有命令，
使用nesysv命令修改服务的自启动状态也会影响到chkconfig命令。

### 3. /etc/rc.d/rc.local文件
该文件是设置服务自启动的第二种方式，在系统启动所有服务后，在用户登录之前linux系统将会执行文件中的命令，通过这点可以在文件中编写启动服务命令来启动服务。

### 4. 独立服务的启动
独立服务的启动脚本一般位于：/etc/init.d目录下，有俩种方式来管理服务的启动：
```
/etc/init.d/独立服务名 start/stop/restart/status
# 通过启动脚本管理

service 独立服务名 start/stop/restart/status
# 通过service命令管理
# service命令是redhat系统独有的命令
```

### 4. xinetd服务管理
基于xinetd服务是不占用内存的，但是效率更慢，因为只有当客户端需要xinetc所管理的服务，xinetd才会启动，因为效率问题基于xinetd服务管理会在redhat版本系统中去除。

xinetd服务管理需要安装对应的RPM包，通过yum安装后，再使用chkconfig --list命令查看时就可以看到xinetd服务管理的RPM包服务。

启动xinetd管理的服务案例：
```
# 例如启动rsync服务

/etc/xinetd.d/
# 进入xinetd.d目录下，该目录存储了xinetd所管理的服务启动脚本

vim ./rsync
# 编辑rsync脚本，将disable值修改为no

service xinetd restart
# 重启xinetd服务，使修改生效
```
注：xinetd管理的服务启动状态和自启动状态是一致的，这点是与独立服务是不同的。

### 5. 服务与端口
1万以内的端口是系统服务使用，1万以上的端口可以由用户自己使用。

- /etc/services
该文件对常规服务锁占用的端口号进行了说明，对某个服务不了解可以查看该文件，例如：grep rsync /etc/services


注：如果要查看服务当前启动状态可以用以下命令
```
ps aux
# 查看当前系统服务所运行的进程，包括服务的进程，一些程序的进程

netstat -tlunp
# 查看系统端口号，判断以启动的服务
# -t，列出tcp数据
# -u，列出udp数据
# -l，列出正在监听的网络服务
# -n，用端口号来显示服务，而不是服务名
# -p，列出该服务的进程ID

netstat -an
# 查看所有端口号，包括正在连接的、监听中的服务和程序。
```

## 源码包服务管理
