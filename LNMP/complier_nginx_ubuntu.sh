# https://www.vultr.com/docs/how-to-compile-nginx-from-source-on-ubuntu-16-04

# 进入下载目录
mkdir -p /var/soft
cd /var/soft
# 下载Nginx
sudo apt update && sudo apt upgrade -y
sudo apt install build-essential -y
wget https://nginx.org/download/nginx-1.13.1.tar.gz && tar zxvf nginx-1.13.1.tar.gz



# 下载Nginx必须类库
# PCRE version 4.4 - 8.40
wget https://ftp.pcre.org/pub/pcre/pcre-8.40.tar.gz && tar xzvf pcre-8.40.tar.gz

#zlib version 1.1.3 - 1.2.11
wget http://www.zlib.net/zlib-1.2.11.tar.gz && tar xzvf zlib-1.2.11.tar.gz

#OpenSSL version 1.0.2 - 1.1.0
wget https://www.openssl.org/source/openssl-1.1.0f.tar.gz && tar xzvf openssl-1.1.0f.tar.gz

cd ./nginx-1.13.1

# 编译安装Nginx
./configure --prefix=/usr/local/nginx \
--user=www \
--group=www \
--build=Ubuntu \
--with-openssl=../openssl-1.1.0f \
--with-openssl-opt=enable-ec_nistp_64_gcc_128 \
--with-openssl-opt=no-nextprotoneg \
--with-openssl-opt=no-weak-ssl-ciphers \
--with-openssl-opt=no-ssl3 \
--with-pcre=../pcre-8.40 \
--with-pcre-jit \
--with-zlib=../zlib-1.2.11 \
--with-compat \
--with-file-aio \
--with-threads \
--with-http_addition_module \
--with-http_auth_request_module \
--with-http_dav_module \
--with-http_flv_module \
--with-http_gunzip_module \
--with-http_gzip_static_module \
--with-http_mp4_module \
--with-http_random_index_module \
--with-http_realip_module \
--with-http_slice_module \
--with-http_ssl_module \
--with-http_sub_module \
--with-http_stub_status_module \
--with-http_v2_module \
--with-http_secure_link_module \
--with-mail \
--with-mail_ssl_module \
--with-stream \
--with-stream_realip_module \
--with-stream_ssl_module \
--with-stream_ssl_preread_module

make && make install

cd /var/soft
rm -rf *.tar.gz

# create systemctl unit
#[Unit]
#Description=A high performance web server and a reverse proxy server
#After=network.target
#
#[Service]
#Type=forking
#PIDFile=/usr/local/nginx/logs/nginx.pid
#ExecStartPre=/usr/local/nginx/sbin/nginx -t -q -g 'daemon on; master_process on;'
#ExecStart=/usr/local/nginx/sbin/nginx -g 'daemon on; master_process on;'
#ExecReload=/usr/local/nginx/sbin/nginx -g 'daemon on; master_process on;' -s reload
#ExecStop=/usr/local/nginx/sbin/nginx --quiet --stop --retry QUIT/5 --pidfile /usr/local/nginx/logs/nginx.pid
#TimeoutStopSec=5
#KillMode=mixed
#
#[Install]
#WantedBy=multi-user.target