## nginx日志管理



在nginx.conf文件中server段中，可以看到一行注释信息，
> #access_log logs/host.access.log main

这说明该server的访问日志文件是logs/host.access.log，使用的格式是"main"格式。

### 1. main格式说明
main格式是一种定义好的日志格式，将其命名并方便引用。
日志的格式是可以自己定义的，main格式如下：
```
log_main main 
	'$remote_addr - $remote_user [$time_local] "$request" '
	'$status $body_bytes_sent "$http_referer" '
	'"$http_user_agent" "$http_x_forwarded_for"';

# 默认的main日志格式记录这样几项信息
# 远程IP - 远程用户 - [本地时间] 请求方法
# 请求状态码、请求体body长度、referer来源信息
# http用户代理、被转发的请求原始IP
```
http_user_agent：检查浏览页面着的操作系统、浏览器版本等信息。
http_x_forwarded_for：在经过代理时，代理将你的ip地址附加在此头信息中，传输你的原始ip。

### 2. 自定义日志格式
nginx允许对不同的server做不同的log日志处理，但是有的web服务器是不支持的例如lighttp，nginx默认会使用main格式来记录日志。

自定义日志格式是在全局配置文件中进行的：
```
# 定义日志格式
log_format mylog '格式'
				'格式'
				'格式'

# 使用日志格式
server{
	access_log logs/access_80_log mylog
}
```
定义完成后在server/location中就可以使用mylog了

### 3. 日志源说明
该文件会一直记录访问源。
linux并不是通过文件名来查找文件的，而是通过i节点来对文件进行读写。

例如/logs/access.log文件，假设nginx服务一直开启并且在记录客户访问记录，这时候通过mv命令来改access.log文件的名字，那么nginx仍然会将记录一直记录在该文件中。

因为进程一直是通过i节点来读写文件的。