### Nginx

#### 安装

- yum install pcre pcre-devel zlib zlib-devel gcc make

  安装依赖文件

- ./configure --prefix=/usr/local/nginx

  编译配置

- make && make install

  安装

- useradd nginx -s /sbin/nologin -M

  创建nginx用户

#### 目录

安装完毕后，可以看到4个目录：
- conf：关于服务器配置相关目录
- html：网页文件
- logs：日志文件
- sbin：可执行的二进制文件，以s字母开头的估计要root用户才能执行

在启动后会新增一系列temp目录：
- client_body_temp
- fastcgi_temp
- proxy_temp
- uwsgi_temp



#### 启动管理

Nginx的管理除了用sbin目录下的nginx二进制文件外开可以使用kill命令配置进程信号来管理。

sbin目录二进制文件管理：
- 启动：sbin/nginx
- 重启：sbin/nginx -s reload
- 关闭：sbin/nginx -s stop



进程信号

- INT：快速杀死进程
- HUP：改变配置文件，平滑的重读配置文件
- USR1：重读日志文件，在日志按月/日分割时有用
- USR2：平滑的升级
- WINCH：优雅关闭旧的进程(配合USR2来进行升级)



通过进程信号来平滑的重启Nginx

- kill -HUP \`more  /usr/local/nginx/nginx.pid`


- kill -USR2 \`more  /usr/local/nginx/nginx.pid`

  USR2将会重新启动一份Nginx master/worker进程，原有master/worker进程仍然会存在的。


- kill -INT 进程id

  关系Nginx进程

