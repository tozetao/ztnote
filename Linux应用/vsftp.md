## vsftp
file transfer protocol，文件传输协议，即能让用户在网络中上传和下载文件的一个协议，而ftp服务器就是支持ftp协议的主机，要完成文件传输需要ftp服务端和ftp客户端的配合才行。

ftp协议占用俩个端口：
- 21端口负责命令的传输，用于接受客户端执行的ftp命令。
- 20端口负责数据的传输，用于文件上传和下载。

ftp数据传输的类型：
- 主动模式：ftp服务端主动向ftp客户的发起连接请求。
- 被动模式：ftp服务器等待ftp客户的的请求

vsftpd目录说明：
- 主程序：/usr/sbin/vsftpd
- 配置目录：/etc/vsftpd，
- 主配置文件：/etc/vsftpd/vsftpd.conf
- 用户禁止登录文件列表：/etc/vsftpd/ftpusers、user_list

### 1. vsftpd配置文件说明
- listen：YES\NO，是否以独立的方式监听服务
- listen_address：监听的ip地址
- listen_port：21，监听端口
- download_enable：是否允许下载文件
- userlist_enable：是否启动禁止登录用户名单
- userlist_deny：同上，user_list文件跟该参数有关，如果是yes，文件中的用户是不允许登录的，如果是no那么只允许文件中的用户登录，类似一个黑白名单机制。
- 
- max_clients：最大客户的连接数，0为不限制
- max_per_id：同一个ip地址的最大连接数，0位不限制
- anon_root：匿名用户的ftp根目录
- anonymous_enable：是否允许匿名用户访问
- anon_max_rate：匿名用户最大传输率（字节），0为不限制。
- anon_upload_enable：是否允许匿名用户上传文件
- anon_umask：匿名用户上传的umask值
- anon_mkdir_write_enable：
- anon_other_write_enable：
- local_enable：是否允许本地用户登录ftp
- local_umask：本地用户上传文件的umask值
- chroot_local_user：是否将用户权限禁锢在ftp目录
- local_max_rate：本地用户的最大传输率（字节）

### 2. 匿名访问模式
```
anonymous_enable=YES
# 允许匿名访问模式

local_enable=YES
write_enable=YES
local_umask=022
dirmessage_enable=YES
xferlog_enable=YES
connect_from_port_20=YES
xferlog_std_format=YES
listen=YES

pam_service_name=vsftpd
userlist_enable=YES
tcp_wrappers=YES

anon_umask=022
anon_upload_enable=YES
# 允许上传文件

anon_mkdir_write_enable=YES
# 允许匿名用户创建文件

anon_other_write_enable=YES
# 允许匿名用户修改目录名或删除目录
```
anon系列参数是关于匿名用户的配置信息

### 3. 本地用户访问模式
```
anonymous_enable=NO
local_enable=YES
write_enable=YES
local_umask=022
userlist_deny=YES
userlist_enable=YES

```

### 4. SELinux
SELinux因为配置麻烦，大部分人都是将其关闭的，暂时就不了解这玩意了。
有的时候你的ftp配置失败就是这个玩意在捣乱。
```
/usr/sbin/sestatus
# 该文件可以查看SELinux的状态

gettenforce
# 查看SELinux的状态

setenforce 0
# 设置SELinux 成为permissive模式
# setenforce 1 设置SELinux 成为enforcing模式

/etc/selinux/config
# 修改配置文件，将SELINUX=enforcing改为SELINUX=disabled
```



### 5. 关于ftp一些文件的权限测试
我在使用ftp软件上传文件或创建目录的时候，权限是不同的。
创建目录的默认权限是755，同时所属用户和所属组都是ftp登录用户，
而创建文件的默认权限是644，所属用户和所属组同上。

对于目录，755代表用户拥有任意权限，5代表所属组只有进入和读取目录，无法在目录里面做写入操作（不能创建、删除文件和目录）。

对于文件，644代表用户只能读取和执行，所属组和其他人只能读取。

### 1. 上传的文件由谁执行说明
在使用linuxftp用户登录ftp软件客户端，上传php文件后，我通过浏览器去访问该php文件，该php文件的代码会创建一个目录，经过测试，这个目录的权限也是755，但是所属用户和所属组是nobody。
我查看了下php的独立进程也是nobody，因为服务器的环境是nginx服务器，

所以可以看出这个文件是由php下的子进程来执行的，由哪个进程来执行，就是由该进程的用户来执行，差不多可以认为是这个意思。

### 2. 关于权限不足的说明
如果是ftp用户上传或创建的目录和文件，默认目录的权限是755，文件的权限是644。

如果是其他进程执行代码，例如php进程或apache进程，要创建文件或目录，需要工作目录给予w（写入）权限，即将所在文件的目录修改成757的权限，才能正常执行。
如果要读取文件，则需要对该文件有写入权限。

大体的权限问题基本都是上述情况才出现的。
