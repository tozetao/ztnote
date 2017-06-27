## Nginx与Memcache整合
nginx、memcache与php的访问流程
```
# nginx处理请求，向memcache获取数据
# 如果memcache有数据则直接返回给nginx，如果没有则调用php，由php来处理请求并将数据存储到memcache中
# nginx处理下次请求就可以直接从memcache中获取。
```

### 1. php扩展编译
在php已经安装完毕的情况下，编译一个php的扩展供php使用，这是非常常见的，我们主要使用php的bin目录下的phpize文件来进行编译，

1. 在软件官方（如memcached）或pecl.php.net寻找扩展源码并下载
2. 将该扩展包下载下来进行解压并进入该目录，该扩展包怎么编译需要依赖你的当前php配置环境
3. 使用phpize文件来根据当前php配置文件进行编译，phpize文件是有php提供的。
4. 编译完成后会生成configure文件，执行并安装（make && make install)，memcache.so扩展文件 

```
/usr/local/fastcgi_php/bin/phpize --with-php-config=/usr/local/fastcgi_php/bin/php-config
# 使用phpize指定当前php配置环境来编译memcache扩展


# ./configure --with-php-config=/usr/local/fastcgi_php/bin/php-config
# 编译完成后当前扩展目录就会多出configure文件

# make && make install
# 安装，扩展文件的路径会告诉你的

# 修改php.ini文件，重启服务。
```

### 2. nginx直连mamcached
执行过程：
1. 客户端请求nginx服务器
2. nginx请求memcached，memcached如果有缓存，直接返回，如果没有缓存则调用php程序，php执行程序，可能做业务处理或是数据查询，再将数据写入memcached

nginx要设定key，通过key去查询memcached，如果没有nginx需要去回调php，同时把key传递给php，这俩点是我们要配置的核心。

```
location / {
	set $memcached_key "$uri";
	# 设置memcached_key变量的值
	
	memcached_pass 127.0.0.1:11211;
	# 将$memcached_key传递给memcached服务
	
	error_page 404 /callback.php
	# 设置错误页面，在memcached找不到数据的时候调用php

	root /var/www/html
}
```

## ngx_http_upstream_consistent_hash模块
该模块是nginx的第三方模块，是一个负载均衡器，使用内置的一致性哈希算法来选择合适的后端节点，与php的memcached模块memcache.hash_strategy兼容。

这意味着可以使用php-memcache模块将内容存储到memcached集群中，然后通过nginx在集群中找到值。

该模块主要使用客户端信息，如ip、uri、args等参数，使用一致性哈希算法将客户端映射到后端节点，例如：
```
consistent_hash $remote_addr：可以根据客户端ip映射
consistent_hash $request_uri： 根据客户端请求的uri映射
consistent_hash $args：根据客户端携带的参数进行映射
```

### 1. 编译第三方模块
在nginx安装包目录中，configure命令编译的时候增加add-module参数，该参数指向外部模块（即nginx_hash模块）

```
./configure --prefix=/usr/local/nginx --add-module=/root/ngx_htt
# add-module参数指向第三方模块的目录
```

### 2. consistent_hash指令
```
语法：
	consistent_hash variable_name

默认值：none

上下文：upstream
```
consistent配合upstream使用一致性哈希作为负载均衡算法
并配置指定的变量作为hash值输出，用于选择后端节点。

配置参考：
```
upstream somestream {
	consistent_hash $request_uri;
	server 10.50.1.3:11211;
	server 10.50.1.4:11211;
	server 10.50.1.5:11211;
}
# 使用$request_uri变量来生成哈希值，通过该哈希值来确定后端节点。
# hash($request_uri)
# 这样php代码就可以通过该$request_uri来生成相同的哈希值，保证nginx和php操作的是同一个节点

server {
    listen       80;
    server_name  localhost;

    location / {
      default_type text/html;
      set $memcached_key $request_uri;
      memcached_pass somestream;
      error_page      500 404 405 = @fallback;
    }

    location @fallback {
      root /srv/www/whatever;
      fastcgi_intercept_errors on;
      error_page 404 = @404;

      set $script $uri;
      set $path_info "";

      include /usr/local/nginx/conf/fastcgi_params;
      fastcgi_param SCRIPT_FILENAME /srv/www/whatever/test.php;
      fastcgi_param SCRIPT_NAME $script;
      fastcgi_param REQUEST_URI $uri;
      fastcgi_pass   127.0.0.1:9000;
    }
}
```


个人测试配置：
```
upstream mcservers {
	consistent_hash $uri;
	server localhost:11211;
	server localhost:11212;
	server localhost:11213;
}

server {
	location / {
		set $memcached_key $request_uri;
		# 
		memcached_pass mcservers;
		error_page 404 /callback.php;
	}
}


# 启动三台memcached服务器
./bin/memcached -u nobody -vv -p 11212
./bin/memcached -u nobody -vv -p 11213

# 配置php中memcached的哈希策略（在php.ini中修改）
# 重启后php就正确的在多台memcache正确的获取数据了
memcache.hash_strategy=consistent

```
php配置：
```
memcache.hash_strategy string
控制key到服务器的映射（分布式）策略。值 consistent允许服务器增减而不会（大量）导致健的重新映射 （译注：参见http://tech.idv2.com/2008/07/24/memcached-004/），设置为 standard则使用余数方式进行key的映射。
```