## nginx配置文件说明
```
worker_processes 1;
# Nginx工作进程，即Nginx的子进程
# 主要根据CPU核心数量来设置，子进程过多会抢夺CPU资源，所以值要合理
# 例如服务器有4个CPU，每个CPU8个核心，值可以设置为32


# 进程与连接的特性配置
events{
	worker_connection 1024;
	# 单个子进程最大允许连接数，这里是1024表示允许连接处理1024个请求连接
}


# http服务器配置，当然也可以是其他类型的服务器
http{
	# server是虚拟主机配置段
	server{
		listen 80;			# 指定监听的端口
		server_name z.com;	# 指定监听的域名

		# URL路径解析或文件定位
		Location /{
			root test	
			# 该路径映射的根目录
			index index.html index.php
		}
	}
	
	# 基于端口的监听
	image_server {	
		listen 2022;
		server_name z.com;
		location / {
			root /var/www/html;
			index index.html;
		}
	}
}
```

## Nginx案例说明
基本优化配置：
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

    server_tokens off;

    #log format
    log_format  access  '$remote_addr - $remote_user [$time_local] "$request" '
         '$status $body_bytes_sent "$http_referer" '
         '"$http_user_agent" $http_x_forwarded_for';
            access_log off;

	server
    {
        listen 80 default_server;
        #listen [::]:80 default_server ipv6only=on;
        server_name www.zhongchuanxinxi.com;
        index index.html index.htm index.php main.html;
        root  /home/wwwroot/default/zc;

 		location /nginx_status
        {
            stub_status on;
            access_log   off;
        }

        location = /dicegame {
                #root  /home/wwwroot/default/zc/dicegame;
        }

        location = /dicetool {
                #root  /home/wwwroot/default/zc/dicetool;
        }
        location / {
            try_files $uri $uri/ /index.php?$query_string;
        }

        location ~ .*\.(gif|jpg|jpeg|png|bmp|swf)$
        {
            expires      30d;
        }

        location ~ .*\.(js|css)?$
        {
            expires      12h;
        }

        location ~ /\.
        {
            deny all;
        }
        access_log  /home/wwwlogs/access.log  access;
    }
	include vhost/*.conf;
}
```

enable-php.conf：php请求转发配置文件
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

pathinfo.conf：PATH_INFO配置文件
```
fastcgi_split_path_info ^(.+?\.php)(/.*)$;
set $path_info $fastcgi_path_info;
fastcgi_param PATH_INFO       $path_info;
try_files $fastcgi_script_name =404;
```

fastcgi.conf：fastcgi要传递的参数
```
fastcgi_param  REQUEST_METHOD     $request_method;
fastcgi_param  CONTENT_TYPE       $content_type;
fastcgi_param  CONTENT_LENGTH     $content_length;

fastcgi_param  SCRIPT_NAME        $fastcgi_script_name;
fastcgi_param  REQUEST_URI        $request_uri;
fastcgi_param  DOCUMENT_URI       $document_uri;
fastcgi_param  DOCUMENT_ROOT      $document_root;
fastcgi_param  SERVER_PROTOCOL    $server_protocol;
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


```


## Demo
重写：
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
```


```
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

