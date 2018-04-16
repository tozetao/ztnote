#### PHP-FPM配置

php-fpm是master/worker的工作方式。它有俩个配置文件，一个是php-fpm.conf，另外一个是php-fpm.d/www.conf。安装完后创建的默认配置文件都有详细注解，只有www.conf文件几个配置要注意下。

> pm = dynamic | static

选择以何种方式来管理进程；

static静态模式，启动的子进程数量是固定的。dynamic是动态模式，进程数量会在一定区间范围内变化。

> pm.max_children = number

必填，最大子进程的数量，至少为1。

注意：php-fpm是阻塞模型，这个参数决定了服务器的服务能力。假设这个值为1，某个请求请求了一个很耗时的文件，那么其他所有请求都会被挂起，直到这个请求被处理完毕后其他请求才会被处理。

> pm.start_servers = number

启动时启动的子进程数量。pm = dynamic的时候，该选项才会生效。

> pm.min_spare_servers

保持空间进程的最小值，如果空闲进程的数量小于该值，将会创建进程

> pm.max_spare_server

保证空闲进程数最大值，如果空闲进程大于此值，将会进行清理

> pm.max_requests = number

默认0，表示不限制。

该参数表示当一个worker进程处理了number个请求后，该进程会重启。这是为了防止PHP第三方插件可能发生的内存泄漏，重启后内存就重置了。

建议该值不要太小，否则可能出现某个时刻多个process（进程）都在重启，服务能力下降。

> request_terminate_timeout = 0

设置单个请求的超时后的中止时间。

该选项可能会对php.ini设置中的"max_execution_time"配置项因为某些特殊原因没有中止运行的脚本有用.。设置为 '0' 表示 'Off'，当经常出现502错误时可以尝试更改此选项。



#### listen

> listen = ip:port | port | '/path/to/unix/socket'

fpm监听端口，即nginx中php处理的地址，一般默认值即可。

> listen.backlog = number

backlog数，-1表示无限制，由操作系统决定，此行注释掉就行。

backlog含义参考：http://www.3gyou.cc/?p=41

> listen.allowed_clients = ip

允许访问FastCGI进程的IP，默认值any，表示不限制。

可以设置多个ip地址，每个地址是用逗号分隔。如果没有设置或者为空，则允许任何服务器请求连接。

> listen.owner = www
>
> listen.group = www
>
> listen.mode = 0666

unix socket设置选项，如果使用tcp方式访问，这里注释即可。



#### 其他基本配置项

> pid = run/php-fpm.pid

pid设置，默认在安装目录中的var/run/php-fpm.pid，建议开启

>  error_log = log/php-fpm.log

错误日志，默认在安装目录中的var/log/php-fpm.log

> log_level = notice

错误级别. 可用级别为: alert（必须立即处理）, error（错误情况）, warning（警告情况）, notice（一般重要信息）, debug（调试信息）. 默认: notice.

> request_slowlog_timeout = 10s

当一个请求该设置的超时时间后，就会将对应的PHP调用堆栈信息完整写入到慢日志中. 设置为 '0' 表示 'Off'

> slowlog = log/$pool.log.slow

慢请求的记录日志,配合request_slowlog_timeout使用

