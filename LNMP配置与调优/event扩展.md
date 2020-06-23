1. 安装event 支持库 libevent
   下载
   wget https://github.com/libevent/libevent/releases/download/release-2.1.11-stable/libevent-2.1.11-stable.tar.gz
   解压
   tar zxvf libevent-2.1.11-stable.tar.gz
   cd libevent-2.1.11-stable
   ./configure --prefix=/usr/local/libevent-2.1.11-stable
   make && make install
    
2. 安装 event
   下载
   wget https://pecl.php.net/get/event-2.5.3.tgz
   解压
   tar zxvf event-2.5.3.tgz
   cd event-2.5.3
   /usr/local/php/bin/phpize
   ./configure --with-php-config=/usr/local/php/bin/php-config --with-event-libevent-dir=/usr/local/libevent-2.1.11-stable
   make && make install
    
3. 最后在php.ini配置文件中开启 event.so 扩展
   extension=event.so