## nginx配置文件说明
```
worker_processes 1;
# 有1个工作的子进程，一般设置为CPU数 * 核数（好的服务器有4个CPU，每个CPU8核心，所以可以设置为32），因为要争夺CPU资源，所以设置的值太大也无用

# 一般用于配置nginx进程与连接的特性，例如一个worker能产生多少连接
events{
	worker_connection 1024;
	# 这是指一个子进程最大允许连接1024个连接
}


# 该项用于配置http服务器的主要配置段
# 当然配置成其他服务器都行，这里只讨论http服务器
http{
	# 这是虚拟主机配置段
	# 基于域名的监听
	Server1{
		listen 80;			# 指定监听的端口
		server_name z.com;	# 指定监听的域名
		
		# 对要处理的URL路径或者是文件定位，例如php单独处理，image路径单独处理，下面的例子是对网站根目录的解析定位
		Location /{
			root test	
			# 指定root目录，当前路径相对于nginx安装目录
			index index.html index.php
		}
	}
	
	# 基于端口的监听
	server {	
		listen 2022;
		server_name z.com;
		location / {
			root /var/www/html;
			index index.html;
		}
	}
}
```