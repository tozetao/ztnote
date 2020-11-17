### install

- 源码安装python3

- pip安装supervisor

  pip install supervisor

- 安装成功后supervisor的执行文件会放在python的bin目录中。



- echo_supervisord_conf

  该文件用于输出默认配置







### 配置

Supervisor.conf是supervisor的配置文件，具体配置如下：

```ini
[unix_http_server]
file=/tmp/supervisor.sock   ;UNIX socket 文件，supervisorctl 会使用
;chmod=0700                 ;socket文件的mode，默认是0700
;chown=nobody:nogroup       ;socket文件的owner，格式：uid:gid
 
;[inet_http_server]         ;HTTP服务器，提供web管理界面
;port=127.0.0.1:9001        ;Web管理后台运行的IP和端口，如果开放到公网，需要注意安全性
;username=user              ;登录管理后台的用户名
;password=123               ;登录管理后台的密码
 
[supervisord]
logfile=/tmp/supervisord.log ;日志文件，默认是 $CWD/supervisord.log
logfile_maxbytes=50MB        ;日志文件大小，超出会rotate，默认 50MB，如果设成0，表示不限制大小
logfile_backups=10           ;日志文件保留备份数量默认10，设为0表示不备份
loglevel=info                ;日志级别，默认info，其它: debug,warn,trace
pidfile=/tmp/supervisord.pid ;pid 文件
nodaemon=false               ;是否在前台启动，默认是false，即以 daemon 的方式启动
minfds=1024                  ;可以打开的文件描述符的最小值，默认 1024
minprocs=200                 ;可以打开的进程数的最小值，默认 200
 
[supervisorctl]
serverurl=unix:///tmp/supervisor.sock ;通过UNIX socket连接supervisord，路径与unix_http_server部分的file一致
;serverurl=http://127.0.0.1:9001 ; 通过HTTP的方式连接supervisord
 
;包含其它配置文件
[include]
files = relative/directory/*.ini    ;可以指定一个或多个以.ini结束的配置文件
```



对于要管理的子进程可以单独编写一个配置文件，放在include配置项指定的目录下。

```ini
; [program:xx]是被管理的进程配置参数，xx是进程的名称
[program:xx]
command=/opt/apache-tomcat-8.0.35/bin/catalina.sh run  ; 程序启动命令

autostart=true       ; 在supervisord启动的时候也自动启动
startsecs=10         ; 启动10秒后没有异常退出，就表示进程正常启动了，默认为1秒

autorestart=true     ; 程序退出后是否自动重启,可选值：[unexpected,true,false]，默认为unexpected，表示进程意外杀死后才重启
startretries=3       ; 启动失败自动重试次数，默认是3

user=tomcat          ; 用哪个用户启动进程，默认是root
priority=999         ; 进程启动优先级，默认999，值小的优先启动
redirect_stderr=true ; 把stderr重定向到stdout，默认false
stdout_logfile_maxbytes=20MB  ; stdout 日志文件大小，默认50MB
stdout_logfile_backups = 20   ; stdout 日志文件备份数，默认是10
; stdout 日志文件，需要注意当指定目录不存在时无法正常启动，所以需要手动创建目录（supervisord 会自动创建日志文件）
stdout_logfile=/opt/apache-tomcat-8.0.35/logs/catalina.out
stopasgroup=false     ;默认为false,进程被杀死时，是否向这个进程组发送stop信号，包括子进程
killasgroup=false     ;默认为false，向进程组发送kill信号，包括子进程
```







### supervisord

管理子进程的督程。

启动：supervisord -c /etc/supervisor/supervisored.conf



重点要明白的是，supervisord可以帮你管理要启动的子进程。如果你的子进程意外崩溃了它可以帮你一直重新去启动它。



### supervisorctl

服务监控终端，可以管理的supervisord的所有子进程的。

启动方式如下：

> supervisorctl -c /etc/supervisor/supervisord.conf



常用命令如下：

- status

  显示当前运行进程列表

- update

  更新新的配置到supervisord，但是不会重启已运行的进程

- reload

  载入所有配置文件，并按型的配置启动，管理所有进程。该选项会重启所有已运行的进程。

- start N

  启动某个进程

- stop N

  停止某个进程

- restart N

  重启某个进程

- stop al停止全部进程

- reread



注意：start、restart、stop都不会载入最新的配置项。





相关链接：

https://www.cnblogs.com/toutou/p/supervisor.html



启动脚本：https://github.com/Supervisor/initscripts/blob/master/redhat-init-mingalevme