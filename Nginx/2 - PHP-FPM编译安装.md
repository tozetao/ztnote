#### 编译安装

- 依赖库安装

  ```
  yum -y install gcc automake autoconf libtool make gcc-c++ glibc libmcrypt-devel mhash-devel libxslt-devel libjpeg libjpeg-devel libpng libpng-devel freetype freetype-devel libxml2 libxml2-devel zlib zlib-devel glibc glibc-devel glib2 glib2-devel bzip2 bzip2-devel ncurses ncurses-devel curl curl-devel e2fsprogs e2fsprogs-devel krb5 krb5-devel libidn libidn-devel openssl openssl-devel

  # 注意：PHP所依赖的libmcrypt类库需要源码编译安装，并且将其安装到/usr/local目录下
  ```

- 创建用户

  useradd phpfpm -s /sbin/nologin -M

- 编译配置

  ```
  ./configure \
  --prefix=/usr/local/php7 \
  --with-config-file-path=/usr/local/php7/etc \
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

- make && make install



#### 目录说明

- bin：bin目录，包含可执行文件
- etc：php-fpm配置文件的目录，php.ini文件也可放置在这里
- include
- lib：包含核心类库文件
- sbin：php-fpm进程管理控制器
- var：日志文件目录

在安装完毕后，需要配置PHP-FPM配置文件和php.ini配置文件
- 配置php.ini文件：cp /root/php5.6/php-devloper.ini /usr/local/php-fpm/etc/php.ini
- 配置php-fpm文件：cp etc/php-fpm.conf.default etc/php-fpm.conf

### 2. 启动说明
- 启动：./sbin/php-fpm
- 停止：kill -INT `cat /usr/local/php-fpm/var/run/php-fpm.pid`
- 平滑启动：kill -USR2 `cat /usr/local/php-fpm/var/run/php-fpm.pid`

php-fpm进程管理器在sbin目录下，常用参数有：
- -c：指定启动时的php.ini配置文件
- -y：指定启动时的php-fpm.conf配置文件

通过这俩个参数我们可以根据不同的

注：重启只能用-USR2进程信号来控制，使用-HUP在杀死主进程后无法启动

### 3. PHP-FPM配置文件说明
```
pm = dynamic 
# 如何控制子进程，选项有static和dynamic。
# 如果选择static，则由pm.max_children指定固定的子进程数。
# 如果选择dynamic，则由下开参数决定控制进程的方式

pm.max_children 
#子进程最大数

pm.start_servers 
#启动时的进程数

pm.min_spare_servers 
#保证空闲进程数最小值，如果空闲进程小于此值，则创建新的子进程

pm.max_spare_servers 
#保证空闲进程数最大值，如果空闲进程大于此值，此进行清理


pm.max_requests = 1000
# 设置每个子进程所能承受的最大请求数，当一个子进程接受的请求超过该值时，会重启。
# 默认值0，表示一个接受请求，等同于 PHP_FCGI_MAX_REQUESTS 环境变量

emergency_restart_threshold = 60
emergency_restart_interval = 60s
#表示在emergency_restart_interval所设值内出现SIGSEGV或者SIGBUS错误的php-cgi进程数如果超过 emergency_restart_threshold个，php-fpm就会优雅重启。这两个选项一般保持默认值。

daemonize = yes
#后台执行fpm,默认值为yes，如果为了调试可以改为no。在FPM中，可以使用不同的设置来运行多个进程池。 这些设置可以针对每个进程池单独设置。

listen = 127.0.0.1:9000
#fpm监听端口，即nginx中php处理的地址，一般默认值即可。可用格式为: 'ip:port', 'port', '/path/to/unix/socket'. 每个进程池都需要设置.

listen.allowed_clients = 127.0.0.1
# 允许访问FastCGI进程的ip地址，默认any为不限制ip

request_terminate_timeout = 0
# 设置单个请求的超时中止时间，php.ini的max_execution_time参数决定PHP脚本的最大执行时间，然后在PHP-FPM中max_execution_time参数无效，由request_terminate_timeout参数管理
# 设置为 '0' 表示 'Off'，表示不限制脚本执行时间，建议修改该值，避免请求队列阻塞
 
request_slowlog_timeout = 10s
#当一个请求该设置的超时时间后，就会将对应的PHP调用堆栈信息完整写入到慢日志中. 设置为 '0' 表示 'Off'
 
slowlog = log/$pool.log.slow
#慢请求的记录日志,配合request_slowlog_timeout使用
```



