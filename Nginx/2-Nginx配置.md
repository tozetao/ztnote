## Nginx配置说明
```
# Nginx工作进程数量，即子进程的数量，数量设置要合理，过多会抢占CPU资源
# 根据CPU数量，CPU核心数量来设置，例如有4个4核心CPU，值设置为14
worker_processes 1;


# events是进程与连接的特性配置
events{
	# 单个子进程最大允许连接数，这里是1024表示允许连接处理1024个请求连接
	worker_connection 1024;
}

# http是对web服务器的配置
http{
	# server是虚拟主机配置段
	server{
		listen 80;			# 指定监听的端口
		server_name z.com;	# 指定匹配的域名

		# URL路径解析或文件定位
		Location /{
			# 该路径所映射的目录路径
			root test	
			# 默认访问文件
			index index.html index.php
		}
	}
	
	
	image_server {	
		listen 2022;
		server_name z.com;
		location / {
			root /var/www/html;
			index index.html;
		}
	}
	# 创建一个图片服务器
}

```

## Nginx案例说明
### 1. 基本配置优化
```
user  www www;

worker_processes auto;

error_log  /home/wwwlogs/nginx_error.log  crit;

pid        /usr/local/nginx/logs/nginx.pid;

#Specifies the value for maximum file descriptors that can be opened by this process.
worker_rlimit_nofile 51200;

events
{
    use epoll;
    worker_connections 51200;
    multi_accept on;
}

http
{
    include       mime.types;
    default_type  application/octet-stream;

    server_names_hash_bucket_size 128;
    client_header_buffer_size 32k;
    large_client_header_buffers 4 32k;
    client_max_body_size 50m;

    sendfile   on;
    tcp_nopush on;

    keepalive_timeout 60;

    tcp_nodelay on;

    fastcgi_connect_timeout 300;
    fastcgi_send_timeout 300;
    fastcgi_read_timeout 300;
    fastcgi_buffer_size 64k;
    fastcgi_buffers 4 64k;
    fastcgi_busy_buffers_size 128k;
    fastcgi_temp_file_write_size 256k;

    gzip on;
    gzip_min_length  1k;
    gzip_buffers     4 16k;
    gzip_http_version 1.1;
    gzip_comp_level 2;
    gzip_types     text/plain application/javascript application/x-javascript text/javascript text/css application/xml application/xml+rss;
    gzip_vary on;
    gzip_proxied   expired no-cache no-store private auth;
    gzip_disable   "MSIE [1-6]\.";

    #limit_conn_zone $binary_remote_addr zone=perip:10m;
    ##If enable limit_conn_zone,add "limit_conn perip 10;" to server section.

    server_tokens off;
    access_log off;

	server
	{
        listen 80 default_server;
        #listen [::]:80 default_server ipv6only=on;
        server_name _;
        index index.html index.htm index.php;
        root  /home/wwwroot/default;

        #error_page   404   /404.html;

        # Deny access to PHP files in specific directory
        #location ~ /(wp-content|uploads|wp-includes|images)/.*\.php$ { deny all; }

        include enable-php.conf;

        location /nginx_status
        {
            stub_status on;
            access_log   off;
        }

        location ~ .*\.(gif|jpg|jpeg|png|bmp|swf)$
        {
            expires      30d;
        }

        location ~ .*\.(js|css)?$
        {
            expires      12h;
        }

        location ~ /.well-known {
            allow all;
        }

        location ~ /\.
        {
            deny all;
        }

        access_log  /home/wwwlogs/access.log;
    }
	include vhost/*.conf;
}
```
### 2. 解析php脚本配置
enable-php.conf：
```
location ~ [^/]\.php(/|$)
{
    # try_files $uri =404;
	include pathinfo.conf;
    fastcgi_pass  unix:/tmp/php-cgi.sock;
    fastcgi_index index.php;
    include fastcgi.conf;
}
```

pathinfo.conf：
```
# PATH_INFO变量的配置，
fastcgi_split_path_info ^(.+?\.php)(/.*)$;
set $path_info $fastcgi_path_info;
fastcgi_param PATH_INFO       $path_info;
try_files $fastcgi_script_name =404;
```


fastcgi.conf：
```
# Nginx要传递给fastcgi的参数
fastcgi_param  SCRIPT_FILENAME    $document_root$fastcgi_script_name;
fastcgi_param  QUERY_STRING       $query_string;
fastcgi_param  REQUEST_METHOD     $request_method;
fastcgi_param  CONTENT_TYPE       $content_type;
fastcgi_param  CONTENT_LENGTH     $content_length;

fastcgi_param  SCRIPT_NAME        $fastcgi_script_name;
fastcgi_param  REQUEST_URI        $request_uri;
fastcgi_param  DOCUMENT_URI       $document_uri;
fastcgi_param  DOCUMENT_ROOT      $document_root;
fastcgi_param  SERVER_PROTOCOL    $server_protocol;
fastcgi_param  REQUEST_SCHEME     $scheme;
fastcgi_param  HTTPS              $https if_not_empty;

fastcgi_param  GATEWAY_INTERFACE  CGI/1.1;
fastcgi_param  SERVER_SOFTWARE    nginx/$nginx_version;

fastcgi_param  REMOTE_ADDR        $remote_addr;
fastcgi_param  REMOTE_PORT        $remote_port;
fastcgi_param  SERVER_ADDR        $server_addr;
fastcgi_param  SERVER_PORT        $server_port;
fastcgi_param  SERVER_NAME        $server_name;

# PHP only, required if PHP was built with --enable-force-cgi-redirect
fastcgi_param  REDIRECT_STATUS    200;
fastcgi_param PHP_ADMIN_VALUE "open_basedir=$document_root/:/tmp/:/proc/";
```


### 3. fastcgi_split_path_info命令说明
fastcgi_split_path_info用于location上下文中，该命令定义捕获$fastcgi_path_info变量值的正则表达式，fastcgi_split_path_info指令是对URI的匹配。

正则表达式匹应该要有俩个捕获(子表达式)，第一个捕获是$fastcgi_script_name变量的值，第二个捕获是$fasccgi_path_info变脸的值。

理解该指令的使用需要理解url在web服务器变量中是什么样子的，例如http://localhost/index.php/blog/user?name=zhangsan，各个变量如下：

- SCRIPT_FILENAME ： =$document_root + $fastcgi_script_name，document_root是网站根目录(/var/www)，/index.php/blog/user
- SCRIPT_NAME: =$fastcgi_script_name，/index.php/blog/user
- REQUEST_URI： =$request_uri，/index.php/blog/user?name=zhangsan，如果你的uri重写过，request_uri仍旧指向第一次访问的uri地址。
- $query_string：name=zhangsan

默认情况下web服务器是没有PATHINFO参数的，所以在使用支持PATH_INFO的PHP框架时会报错。


PATH_INFO配置1：
```
fastcgi_split_path_info ^(.+?\.php)(/.*)$;
set $path_info $fastcgi_path_info;
fastcgi_param PATH_INFO $path_info;
try_files $fastcgi_script_name =404;
```
PATH_INFO配置2：
```
location ~ \.php {
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

进行配置后，服务器变量变化如下，
- SCRIPT_FILENAME：/var/www/blog/index.php
- SCRIPT_NAME：/index.php
- PATH_INFO：blog/user
- REQUEST_URI：/blog/index.php/user/add?name=lisi

$fastcgi_script_name被切割分成俩部分，脚本文件后的部分路径是PATH_INFO，剩余的变量则是$fastcgi_script_name的值。

### 4. URL重写，隐藏index.php
```
location / {
     index  index.php index.html index.htm;
     if (!-e $request_filename)
     {
        #地址作为将参数rewrite到index.php上。
        rewrite ^/(.*)$ /index.php/$1;
        #若是子目录则使用下面这句，将subdir改成目录名称即可。
        #rewrite ^/subdir/(.*)$ /subdir/index.php/$1;
     }
}

location ~ \.php {
    include fastcgi.conf;

    set $path_info "";

    set $real_script_name $fastcgi_script_name;

    if ($fastcgi_script_name ~ "^(.+?\.php)(/.+)$") {
            set $real_script_name $1;
            set $path_info $2;
    }
    fastcgi_param SCRIPT_FILENAME $document_root$real_script_name;
    fastcgi_param SCRIPT_NAME $real_script_name;
    fastcgi_param PATH_INFO $path_info;

    fastcgi_intercept_errors on;
    fastcgi_pass   127.0.0.1:9000;
}
```