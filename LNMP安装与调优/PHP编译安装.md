### PHP Install

依赖库安装

```
yum -y install gcc automake autoconf libtool make gcc-c++ glibc libmcrypt-devel mhash-devel libxslt-devel libjpeg libjpeg-devel libpng libpng-devel freetype freetype-devel libxml2 libxml2-devel zlib zlib-devel glibc glibc-devel glib2 glib2-devel bzip2 bzip2-devel ncurses ncurses-devel curl curl-devel e2fsprogs e2fsprogs-devel krb5 krb5-devel libidn libidn-devel openssl openssl-devel

# 注意：PHP所依赖的libmcrypt类库需要源码编译安装，并且将其安装到/usr/local目录下，但是在php7中该扩展已经被移除。
```







新建用户

```
# 创建用户组
sudo groupadd www

# 创建用户并指定用户组
useradd www -g www -s /sbin/nologin -M

# 设置用户密码
sudo passwd www

# 查看用户信息
id www
groups www
```





编译

```
wget https://www.php.net/distributions/php-7.3.33.tar.gz

./configure \
--prefix=/usr/local/php \
--with-config-file-path=/usr/local/php/etc \
--enable-fpm \
--with-fpm-user=phpfpm \
--with-fpm-group=phpfpm \
--with-mysqli \
--with-libxml-dir \
--with-gd \
--with-jpeg-dir \
--with-png-dir \
--with-freetype-dir \
--with-iconv-dir \
--with-zlib-dir \
--enable-soap \
--enable-mbstring \
--enable-exif \
--disable-ipv6 \
--with-pear \
--with-curl \
--with-openssl \
--enable-gd-jis-conv

# 下面这3项扩展php7不支持
--enable-gd-native-ttf \
--with-mcrypt=/usr/local/mcrypt \
--with-mysql
```



安装

```
make && make install
```



### PHP Config

配置PHP环境

```
cp php.ini-production /usr/local/php/etc/php.ini

chown -R www:www /usr/local/php
```





FPM配置文件

```
[global]
pid = /usr/local/php/var/run/php-fpm.pid
error_log = /usr/local/php/var/log/php-fpm.log
log_level = notice

[www]
listen = /tmp/php-cgi-73.sock
listen.backlog = -1
listen.allowed_clients = 127.0.0.1
listen.owner = www
listen.group = www
listen.mode = 0666
user = www
group = www
pm = dynamic

pm.max_children = 100
pm.start_servers = 10
pm.min_spare_servers = 10
pm.max_spare_servers = 30
request_terminate_timeout = 100
request_slowlog_timeout = 2
slowlog = /usr/local/php/var/log/slow.log
```
















#### 目录

- bin：二进制可执行文件，例如php、phpize、php-cgi
- etc：php-fpm配置文件的目录，php.ini文件也可放置在这里
- include
- lib：包含核心类库文件
- sbin：php-fpm进程管理控制器
- var：日志文件目录












#### 启动管理

在安装完毕后，需要从安装包中拷贝php.ini配置文件到etc目录中。

```
启动
/usr/local/php/sbin/php-fpm 


/usr/local/php/sbin/php-fpm

/usr/local/php/var/run/php-fpm.pid


关闭
kill -INT `cat /usr/local/php/var/run/php-fpm.pid`

重启
kill -USR2 `cat /usr/local/php/var/run/php-fpm.pid`


php-fpm常用参数:
-c：指定启动时的php.ini配置文件
-y：指定启动时的php-fpm.conf配置文件
-t：测试fpm配置文件是否出错

HUP进行信号无法重启PHP主进程，它会杀死master进程。
USR1进程信号会杀死所有PHP进程。
```






#### phpize

phpize用于安装php扩展，例如当前php需要安装sockets扩展：

- 进入扩展包目录

  cd /home/php7.1.2/ext/sockets

- 执行phpize

  /usr/local/php7/bin/phpize

- 指定sockets扩展的php-config二进制文件

  ./configure --with-php-config=/usr/local/php/bin/php-config

最后在php.ini启用该扩展即可。























### Nginx Config

http模块

```
	default_type  application/octet-stream;

	sendfile   on;
    tcp_nopush on;
    tcp_nodelay on;
    keepalive_timeout 60;




    server_names_hash_bucket_size 512;
    client_header_buffer_size 32k;
    large_client_header_buffers 4 32k;
    client_max_body_size 50m;

    fastcgi_connect_timeout 300;
    fastcgi_send_timeout 300;
    fastcgi_read_timeout 300;
    fastcgi_buffer_size 64k;
    fastcgi_buffers 4 64k;
    fastcgi_busy_buffers_size 128k;
    fastcgi_temp_file_write_size 256k;
    fastcgi_intercept_errors on;

    gzip on;
    gzip_min_length  1k;
    gzip_buffers     4 16k;
    gzip_http_version 1.1;
    gzip_comp_level 2;
    gzip_types     text/plain application/javascript application/x-javascript  text/javascript text/css application/xml;
    gzip_vary on;
    gzip_proxied   expired no-cache no-store private auth;
    gzip_disable   "MSIE [1-6]\.";

    limit_conn_zone $binary_remote_addr zone=perip:10m;
    limit_conn_zone $server_name zone=perserver:10m;

    server_tokens off;
    access_log off;
```







pathinfo.conf

```
set $real_script_name $fastcgi_script_name;
if ($fastcgi_script_name ~ "^(.+?\.php)(/.+)$") {
        set $real_script_name $1;
        set $path_info $2;
 }
fastcgi_param SCRIPT_FILENAME $document_root$real_script_name;
fastcgi_param SCRIPT_NAME $real_script_name;
fastcgi_param PATH_INFO $path_info;
```



fastcgi.conf

```
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
```





enable-php73.conf

```
location ~ [^/]\.php(/|$)
{
    try_files $uri =404;
    fastcgi_pass  unix:/tmp/php-cgi-73.sock;
    fastcgi_index index.php;
    include fastcgi.conf;
    include pathinfo.conf;
}
```



server配置

```
server
{
    listen 80;
    # server_name wmsv2.yijiaxinxi.cn wmsv2.jiujiuqilu.com;
    
    index index.html index.htm;
    root /var/www/admin/public;

	# 跨域配置
    add_header Access-Control-Allow-Origin *;
    add_header Access-Control-Allow-Methods 'GET, POST, OPTIONS';

    include enable-php-73.conf;
    
    location / {
        if (!-e $request_filename){
            rewrite  ^(.*)$  /index.php?s=$1  last;
        }
    }


    #禁止访问的文件或目录
    location ~ ^/(\.user.ini|\.htaccess|\.git|\.svn|\.project|LICENSE|README.md)
    {
        return 404;
    }

    #一键申请SSL证书验证目录相关设置
    location ~ \.well-known{
        allow all;
    }

    location ~ .*\.(gif|jpg|jpeg|png|bmp|swf)$
    {
        expires      30d;
        error_log off;
        access_log off;
    }

    location ~ .*\.(js|css)?$
    {
        expires      12h;
        error_log off;
        access_log off;
    }
    access_log  /var/wwwlogs/admin.log;
    error_log  /var/wwwlogs/admin.error.log;
}

```

ssl配置

```
    #SSL-START SSL相关配置，请勿删除或修改下一行带注释的404规则
    #error_page 404/404.html;
    # listen 443 ssl http2;
    ssl_certificate    /etc/letsencrypt/live/wmsv2.yijiaxinxi.cn/fullchain.pem;
    ssl_certificate_key    /etc/letsencrypt/live/wmsv2.yijiaxinxi.cn/privkey.pem;
    ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
    ssl_ciphers ECDHE-RSA-AES128-GCM-SHA256:HIGH:!aNULL:!MD5:!RC4:!DHE;
    ssl_prefer_server_ciphers on;
    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;
    error_page 497  https://$host$request_uri;
    if ($host = wmsv2.yijiaxinxi.cn) {
        return 307 $scheme://wmsv2.jiujiuqilu.com$request_uri;
    }
    #SSL-END
```





Nginx rewrite

```
location / {
    index index.html error/index.html index.php;
    autoindex  off;
    try_files $uri $uri/ /index.php?$query_string;
}

location / { 
   if (!-e $request_filename) {
           rewrite  ^(.*)$  /index.php?s=/$1  last;
    }
}
```



```
rsync --daemon --config=/etc/rsyncd.conf

rsync -avz --password-file=/var/php_project/admin.pass --exclude-from="./exclude" /var/php_project/admin rsync://root@124.222.110.86/var/www/admin



rsync -avz --password-file=/var/php_project/admin.pass --exclude-from="./exclude" /var/php_project/admin/ rsync://root@124.222.110.86/var/www
```





### Redis

redis安装

```
wget https://download.redis.io/releases/redis-5.0.9.tar.gz
make PREFIX=/usr/local/redis
make install
cp /home/soft/redis/redis.conf /usr/local/redis



/usr/local/redis-5.0.9/src/redis-server /usr/local/redis-5.0.9/redis.conf
```





redis扩展安装

```
wget https://pecl.php.net/get/redis-5.3.7.tgz
/usr/local/php7/bin/phpize
./configure --with-php-config=/usr/local/php7/bin/php-config
make && make install

extension="redis.so"
```







autoconf安装

```
wget ftp://ftp.gnu.org/gnu/autoconf/autoconf-2.68.tar.gz
tar -zxvf autoconf-2.68.tar.gz
cd autoconf-2.68
 ./configure --prefix=/usr/
make && make install
```







php安装

```
wget https://www.php.net/distributions/php-7.3.33.tar.gz
```







### SSH2

http://pecl.php.net/package/ssh2

```
wget http://pecl.php.net/get/ssh2-1.4.tgz

yum install libssh2 libssh2-devel -y


```





