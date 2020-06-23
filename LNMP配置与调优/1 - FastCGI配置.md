### 进程配置

```ini
pm = dynamic | static
```

pm管理进程的启动方式，static是静态模式，启动的子进程是固定的。dynamic是动态模式，进程数量会在一定区间内变化。与dynamic相关的配置选项有：

- pm.start_servers
- pm.min_spare_servers
- pm.max_spare_servers

```ini
pm.max_children = number
```

必填，最大子进程的数量，至少为1。

php-fpm是阻塞模型，这个参数决定了服务器的服务能力。假设这个值为1，某个请求请求了一个很耗时的文件，那么其他所有请求都会被挂起，直到这个请求被处理完毕后其他请求才会被处理。

```ini
pm.start_servers = number
```

启动时启动的子进程数量。pm = dynamic的时候，该选项才会生效。

```ini
pm.min_spare_servers
```

保持空闲进程的最小值，如果空闲进程的数量小于该值，将会创建进程

```ini
pm.max_spare_server
```

保证空闲进程数最大值，如果空闲进程大于此值，将会进行清理

```ini
pm.max_requests = number
```

该参数表示当一个worker进程处理了number个请求后，该进程会重启。这是为了防止PHP第三方插件可能发生的内存泄漏，重启后内存就重置了。

默认0，表示不限制请求的处理。该值建议不要太小，否则访问量大的情况，worker进程会重启，使得服务器能力下降。

```ini
request_terminate_timeout = 0
```

处理单个请求的最大超时时间。设置为 '0' 表示 'Off'，当经常出现502错误时可以尝试更改此选项。



### 端口相关

```ini
listen = ip:port | port | '/path/to/unix/socket'
```

fpm监听端口，即nginx中php处理的地址，一般默认值即可。

```ini
listen.backlog = number
```

存储准备中或已就绪的socket连接的队列大小 ，-1表示无限制，由操作系统决定，此行注释掉就行。

```ini
listen.allowed_clients = ip
```

允许访问FastCGI进程的IP，默认值any，表示不限制。可以设置多个ip地址，每个地址是用逗号分隔。如果没有设置或者为空，则允许任何服务器请求连接。

```ini
listen.owner = www
listen.group = www
listen.mode = 0666
```

设置unix socket的选项，在Linux中读和写必须设置权限才允许从web服务器进行连接。如果使用tcp方式访问，这里注释即可。





### 错误日志

```ini
pid = run/php-fpm.pid
```

pid设置，默认在安装目录中的var/run/php-fpm.pid，建议开启

```ini
error_log = log/php-fpm.log
```

错误日志，默认在安装目录中的var/log/php-fpm.log

```ini
log_level = notice
```

错误级别，可用级别为: alert（必须立即处理）, error（错误情况）, warning（警告情况）, notice（一般重要信息）, debug（调试信息）. 默认: notice.



### 慢日志

```ini
request_slowlog_timeout = 10s
```

慢日志的记录时间。如果处理一个请求超过该选项的时间，就会将对应的PHP调用堆栈信息完整写入到慢日志中。设置为 '0' 表示 'Off'

```ini
slowlog = log/$pool.log.slow
```

慢日志的存储位置。

