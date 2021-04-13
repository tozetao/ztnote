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
