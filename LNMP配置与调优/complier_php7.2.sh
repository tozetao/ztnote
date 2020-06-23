# vhttps://www.linuxbabe.com/arch/compile-php7-0-php7-1-from-source-arch-linux


# 更新apt并安装php必须扩展
apt update
apt -y install libxml2-dev build-essential openssl libssl-dev make curl libcurl4-gnutls-dev libjpeg-dev libpng-dev libmcrypt-dev libreadline6 libreadline6-dev libc-client2007e libc-client2007e-dev
apt -y install libevent-dev zlib1g-dev libbz2-dev libgmp3-dev libcurl4-openssl-dev libgd2-xpm-dev libc-client-dev libkrb5-dev libxslt1-dev
apt -y install gcc make bison gawk re2c libxml2 libwebp freetype2 c-client libmcrypt libxslt

# 下载源码包
wget http://php.net/distributions/php-7.0.26.tar.gz


./configure \
--prefix=/usr/local/php7.2                   \
--with-config-file-path=/usr/local/php7.2/etc   \
--with-zlib-dir                              \
--with-freetype-dir                          \
--enable-mbstring                            \
--with-libxml-dir=/usr                       \
--enable-soap                                \
--enable-calendar                            \
--with-curl                                  \
--with-mcrypt                                \
--with-zlib                                  \
--with-gd                                    \
--disable-rpath                              \
--enable-inline-optimization                 \
--with-bz2                                   \
--with-zlib                                  \
--enable-sockets                             \
--enable-sysvsem                             \
--enable-sysvshm                             \
--enable-pcntl                               \
--enable-mbregex                             \
--enable-exif                                \
--enable-bcmath                              \
--with-mhash                                 \
--enable-zip                                 \
--with-pcre-regex                            \
--with-pdo-mysql                             \
--with-mysqli                                \
--with-jpeg-dir=/usr                         \
--with-png-dir=/usr                          \
--enable-gd-native-ttf                       \
--with-openssl                               \
--with-fpm-user=http                         \
--with-fpm-group=http                        \
--enable-ftp                                 \
--with-imap                                  \
--with-imap-ssl                              \
--with-kerberos                              \
--with-gettext                               \
--with-xmlrpc                                \
--with-xsl                                   \
--enable-opcache                             \
--enable-fpm
# 注：再7.2版本中，--with-mcrypt, --enable-gd-native-ttf俩个配置废弃了




# 初始化PHP配置
cp /opt/php-7.0.26/etc/php-fpm.conf.default /opt/php-7.0.26/etc/php-fpm.conf
cp /opt/php-7.0.26/etc/php-fpm.d/www.conf.default /opt/php-7.0.26/etc/php-fpm.d/www.conf
cp /opt/php-7.0.26/etc/php-fpm.d/www.conf



# unix socket的配置
#listen = /run/php-fpm/php7.0-fpm.sock
#listen.owner = http
#listen.group = http
#listen.mode = 0660
