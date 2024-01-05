#### 编译安装

- 依赖库安装

  ```
  yum -y install gcc automake autoconf libtool make gcc-c++ glibc libmcrypt-devel mhash-devel libxslt-devel libjpeg libjpeg-devel libpng libpng-devel freetype freetype-devel libxml2 libxml2-devel zlib zlib-devel glibc glibc-devel glib2 glib2-devel bzip2 bzip2-devel ncurses ncurses-devel curl curl-devel e2fsprogs e2fsprogs-devel krb5 krb5-devel libidn libidn-devel openssl openssl-devel

  # 注意：PHP所依赖的libmcrypt类库需要源码编译安装，并且将其安装到/usr/local目录下，但是在php7中该扩展已经被移除。
  ```

- 创建用户

  useradd phpfpm -s /sbin/nologin -M

- 编译配置

  ```
  ./configure \
  --prefix=/usr/local/php-7.3.33 \
  --with-config-file-path=/usr/local/php-7.3.33/etc \
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




#### 目录

- bin：二进制可执行文件，例如php、phpize、php-cgi
- etc：php-fpm配置文件的目录，php.ini文件也可放置在这里
- include
- lib：包含核心类库文件
- sbin：php-fpm进程管理控制器
- var：日志文件目录




#### 启动管理

在安装完毕后，需要从安装包中拷贝php.ini配置文件到etc目录中。

note：php-fpm没有提供命令来控制自身服务，因此需要通过进程信号来管理php-fpm服务

- 启动

  /sbin/php-fpm

- 关闭

  kill -INT \`cat /usr/local/php-fpm/var/run/php-fpm.pid`

- 重启

  kill -USR2 \`cat /usr/local/php-fpm/var/run/php-fpm.pid`

  HUP进行信号无法重启PHP主进程，它会杀死master进程。

  USR1进程信号会杀死所有PHP进程。

php-fpm常用参数
- -c：指定启动时的php.ini配置文件
- -y：指定启动时的php-fpm.conf配置文件
- -t：测试fpm配置文件是否出错




#### phpize

phpize用于安装php扩展，例如当前php需要安装sockets扩展：

- 进入扩展包目录

  cd /home/php7.1.2/ext/sockets

- 执行phpize

  /usr/local/php7/bin/phpize

- 指定sockets扩展的php-config二进制文件

  ./configure --with-php-config=/usr/local/php7/bin/php-config

最后在php.ini启用该扩展即可。
