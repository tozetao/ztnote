#### 基本配置

Nginx是模块化的，配置也不例外。分别对events模块、http模块、server模块配置完毕后基本就能运行了。

example：全局模块与events模块

```
user  www www;
pid        /usr/local/nginx/logs/nginx.pid;
error_log  /home/wwwlogs/nginx_error.log  crit;

worker_processes auto;
worker_rlimit_nofile 51200;

events
{
    use epoll;
    worker_connections 51200;
    multi_accept off;
}
```

example：server模块

```
server
{
    listen 80;
    server_name _;
    index index.html index.htm index.php;
    root  /home/wwwroot/default;
}
```



#### fastCGI配置

- fastcgi_param  key  value

  该指令用于配置cgi参数，对于PHP来说就是配置$_SERVER数组的键值对，key是键，value是值。

- fastcgi_pass  value

  与cgi程序通信的地址，Nginx会将请求讲给cgi程序来处理

- fastcgi_index value

  如果URI以斜线结尾，文件名将追加到URI后面，这个值将存储在变量$fastcgi_script_name中。

基本配置

```
location ~ \.php {
	fastcgi_pass   127.0.0.1:9000;
    fastcgi_index  index.php;
    fastcgi_param  SCRIPT_FILENAME  $document_root$fastcgi_script_name;
    include        fastcgi_params;    
}
```



#### fastcgi_split_path_info

- 作用于：location
- 用途：该指令定义捕获$fastcgi_path_info变量的正则表达式，正则表达式可以有俩个捕获，第一个会捕获\$fastcgi_script_name变量的值，第二个会捕获\$fast_path_info变量的值

通过该指令可以方便的配置PATH_INFO，主要是一些框架的路由会使用到PATH_INFO；

example：

```
location ~ [^\]+\.php(/|$)
{
    fastcgi_pass 127.0.0.1:9000;
    fastcgi_index index.php;
    
    # PATH_INFO config
	fastcgi_split_path_info ^(.+?\.php)(/.*)$;
	fastcgi_param PATH_INFO $fastcgi_path_info;
	try_files $fastcgi_script_name =404;

    include fastcgi_param.conf;
}

```

老版本Nginx PATH_INFO配置示例：

```
location ~ [^\]+\.php(/|$) {
	fastcgi_pass   127.0.0.1:9000;
	fastcgi_index  index.php;

	#先加载默认后解析赋值
	include        fastcgi_params;

	#正则解析路径
	set $path_info "";
	set $real_script_name $fastcgi_script_name;
	if ($fastcgi_script_name ~ "^(.+?\.php)(/.+)$") {
		set $real_script_name $1;
		set $path_info $2;
	}
	fastcgi_param PATH_INFO       $path_info;
	fastcgi_param SCRIPT_FILENAME $document_root$real_script_name;
	fastcgi_param SCRIPT_NAME     $real_script_name;
}
```



#### 配置优化建议

- master_process on|off;

  是否以master/worker模式工作，默认on。如果设置为off，则有master进程亲自处理请求，不启动worker；否则master进程只起管理worker进程的作用。

- worker_processes xxx;

  Nginx worker进程的数量，默认为auto（1.9.10之后）。通常设定为cpu逻辑核心数，这样可以发挥并行的优势。如果设置为auto，则有Nginx自动设置为cpu核心数。

- worker_cpu_affinity xxx;

  worker进程绑定cpu选项，默认不设置。

  如果不设置则不同的worker会绑定到不同的cpu核心执行，这样会降低并发的效果。如果设置为auto（1.9.10），系统自动绑定到合适的cpu核心。也可以手动使用掩码设置，例如4个worker设置为：worker_cpu_affinity 0001 0010 0100 1000;

- worker_priority xxx;

  worker的优先级，linux在新一轮的调度中，执行顺序和时间分片长度都与worker的优先级有关，值越小优先级越高。默认是0，取值范围是-20值20。

- events { worker_connections number; }

  每个worker可以同时处理的最大连接数，默认1024。这个值的大小不能超过worker_rlimit_nofile的大小；

  另外如果Nginx作为反向代理服务器，需要耗费2倍的连接数（服务器+客户端）

- worker_rlimit_nofile

  指定通过worker进程可以打开的最大文件的数值



- events { accept_mutex  on|off; }

  1.11.3之前的版本默认值是on，之后默认值是off；

  该选项是为了负载均衡设计，如果是on，nginx会使worker一个一个的接收请求，而不是一起唤醒去争抢该请求（惊群问题）。但是实际上负载大的情况下处于休眠的worker进程也不多，关闭该选项能够加快tcp连接的建立速度，所以之后该选项默认关闭了。

- events { accept_mutex_delay number; }

  当accept_mutex开启时，若尝试accept失败，worker会最多等待number秒来重试。默认值500ms

- events { use method; }

  使用什么方法来管理连接，默认不同的系统使用不同的方法，一般linux2.6以后版本使用epoll；

  http://nginx.org/en/docs/events.html

- events { multi_accept on|off; }

  默认off。如果打开，则一个worker会同时accept多个连接。关于这个的讨论比较少，个人觉得是打开这个选项可能会导致负载很不平衡，但是对效率应该是有提高的。