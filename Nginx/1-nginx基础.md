## nginx简介
nginx是一个高性能的http和反向代理服务器，也是一个IMAP/POP3/SMTP代理服务器。

## nginx编译安装
```
./configure --prefix=/usr/local/nginx
# prefix用于指定安装路径

yum install pcre pcre-devel
# 编译配置的时候，如果缺少依赖的包安装提示安装即可

make && make install
# 编译并进行安装

cd /usr/local/nginx/sbin
# 进入二进制执行文件目录

./nginx
# 启动nginx服务


```

### 2. nginx目录说明
在指定安装目录后，进入/usr/local/nginx目录下，可以看到4个目录
- conf：关于服务器配置相关目录
- html：网页文件
- logs：日志文件
- sbin：可执行的二进制文件，以s字母开头的估计要root用户才能执行

上述的目录是安装后未启动nginx服务器的目录情况，当你启动nginx服务后，相应的http服务目录也会创建完毕，例如http proxy temporary files所在的目录等。

```
configuration summary
	using system zlib library
	using system pcre library
	using builtin md5 code
	sha1 library is not found
	openssl library is not used

nginx path prefix: /usr/local/nginx
nginx binary file: /usr/local/nginx/sbin/nginx
nginx configuration prefix: /usr/local/nginx/conf
configuration file: /usr/local/nginx/conf/nginx.conf
pid file: /usr/local/nginx/logs/nginx.pid
error log file: /usr/local/nginx/logs/error.log
http access log file： /usr/local/nginx/logs/access.log
http client request body temporary files：
http proxy temporary files：
http fastcgi temporary files：
http uwsgi temporary files：
http scgi temporary files：

```

### 3. 关于devel包的说明
devel包主要是头文件之类的，供开发用，而且一般会依赖相应的包，安装的时候会自动装上去。

devel包含普通包，只比普通包多了头文件。动态链接库的话两种包都有。编译的时候如果需要用到这个库，那么需要安装这个库的devel，因为需要头文件