### 4. URL重写，隐藏index.php

```
location / {
     index  index.php index.html index.htm;
     if (!-e $request_filename)
     {
        #地址作为将参数rewrite到index.php上。
        rewrite ^/(.*)$ /index.php/$1;
        #若是子目录则使用下面这句，将subdir改成目录名称即可。
        #rewrite ^/subdir/(.*)$ /subdir/index.php/$1;
     }
}

location ~ \.php {
    include fastcgi.conf;

    set $path_info "";

    set $real_script_name $fastcgi_script_name;

    if ($fastcgi_script_name ~ "^(.+?\.php)(/.+)$") {
            set $real_script_name $1;
            set $path_info $2;
    }
    fastcgi_param SCRIPT_FILENAME $document_root$real_script_name;
    fastcgi_param SCRIPT_NAME $real_script_name;
    fastcgi_param PATH_INFO $path_info;

    fastcgi_intercept_errors on;
    fastcgi_pass   127.0.0.1:9000;
}
```







## 预定义变量

Nginx预定了一些变量，这些变量定义了服务器环境信息、请求信息、url等信息；
在conf文件中定义fastcgi参数，是为了让php-fmp进程知道如何解析这些信息，例如下面定义的配置，将会被解析并封装到$_SERVER变量中。

常见的预定义变量有：
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
```

- $uri：当前请求的URI，不包括参数。
- $request_uri：从客户端请求发送过来的原生URI，包括参数
- $request_file_name：请求文件名，不包括参数，它是磁盘上的绝对路径。

- $fastcgi_script_name：当前请求的脚本文件名，它是指执行fastcgi程序的脚本文件名，如果请求被重写过那么fastcgi_name指的是重写后的脚本文件名。
- $query_string：URI参数，如果URI被重写过，参数将包括原生URI参数和重写URI参数
- $document_root：web根目录，绝对路径

注1：$uri变量在手册中说明是代表着重写后的URI，可在测试中URI被重写过，$uri仍然代表着原生URI，并不是改变后的URI。

example:
```
# nginx conf
location ~ /test/a.php
{
	rewrite /test/a.php /test/b.php?param=$uri&request_uri=$request_uri&request_filename=$request_filename last;
	break;
}

# example: url = localhost/test/a.php?age=20
# 
# uri: /test/a.php
# request_uri: /test/a.php?age=20
# request_filename: web根目录/test/a.php
# 
# 注：如果url中的uri后面有PATHINFO之类的目录，PATH也会附带在上述几个参数中，例如url是: localhost/test/a.php/user/add?age=20

# example: url = location/rewrite/test
# uri: /rewrite/test
# request_uri: /rewrite/test
# request_filename: web根目录/rewrite/test

```

## Rewrite
rewrite_module允许正则替换URI，返回页面重定向和按条件选择配置。
rewrite_module模块指令按以下规则顺序处理：
1. 处理在server级别中定义的模块指令
2. 为请求查找lcation
3. 处理在选中的location中定义的模块指令，如果有指令改变了URI，按新的URI查找Location，这个过程循环至多到10次，之后nginx返回500错误。



rewrite module模块包括以下指令：
- break
- if
- rewrite
- rewrite_log
- set
- uninitialized_variable_warn 

### break
- 默认值：-
- 上下文：server,location,if
- 作用：停止处理这一轮请求的ngx_http_rewrite_module模块指令，例如：
```
if($slow) {
	limit_rate 10k;
	break;
}
```


### rewrite
- 语法: rewrite regex replacement [flag]
- 默认值: -
- 上下文: server, location, if

regex是正则表达式，replacement是要替换URI的参数。

如果指定的正则表达式能匹配URI，此URI将会被replacement参数的字符串改写，rewrite指令按其在配置文件中出现的顺序执行。

flag指令能终止后续指令的执行。

如果replacement包含新的参数，以往的请求参数将会附带添加到新参数后面，如果不希望这样，在replacement参数后面加一个?就可以避免，例如: rewrite ^/user/(.*)$ /show?user=$1? last;

如果正则表达式包含字符}或者字符:，整个表达式应该被包含在单引号或者双引号之中。

flag
- last: 停止当前这一轮的rewrite指令执行，并使用改变后的uri做新的location匹配 
- break: 停止当前这一轮的rewrite指令执行，使用改变后的uri去查找可执行的程序文件并执行

flag example:
```
location /flag/test {
	rewrite /flag/test /flag/b.php break;
}
# 虽然uri重写成flag/b.php文件，可是该脚本文件不会执行，因为break指令终止了使用重写后的uri来做新的location匹配。
```

### if指令
语法：
```
if (express){
	# 重写模式
}

# 条件表达式有3种写法
# 1. 用"="号来判断相等，用于字符串比较
# 2. 用"~"来进行正则匹配（此处的大小写），用~*是不区分大小的正则
# 3. -f判断是否文件、-d用于判断是否目录，-e用于判断是否存在

# 注：if后的空格是不能少的
```

example：
```
if ($request_method = POST){
	return 405;
}
if ($remote_addr = 192.168.0.125 ){
	return 403;
}
# 字符串比较

if ($http_user_agent ~* msie){
	rewrite ^.*$ /ie.html;
}
# 正则表达式比较
# 匹配成功后nginx将会把正则表达式（^.*$)匹配到的URI进行重写，再做内部转发

if (!-e $document_root$fastcgi_script_name){
	rewrite ^.*$ /404.html;
	break;
}
# 判断文件或目录是否存在
# 以xxx.com/abcd.html这个不存在的页面为例，这里仍然需要增加break，因为观察访问日志可以发现重写后的URI仍然是/abcd.html
# 服务器内部的rewrite和302跳转是不一样的，302跳转的话URL会变成请求404.html，而内部rewrite的话URI上下文是不发生变化的，$fastcgi_script_name仍然是abcd.htm，所以发生了循环重定向

# 意思是rewrite后会去访问/404.html页面，但是URI是不变的，$fastcgi_script_name的值仍然是abcd.html
```

注1：一般if指令会跟break、set关键字配合使用。
注2：nginx/conf/fastcgi_params该文件夹保存了nginx服务器全局参数说明。

### 3. set关键字
set用于设置变量用的，可以用来达到多条件判断时作为标志使用，达到apache下的rewrite_condition的效果。

```
if ($http_user_agent ~* msie){
	set $flag 1;
}
# 如果访问的客户端是IE浏览器，

if ($fastcgi_script_name = ie.html){
	set $flag 0;
}

if ($flag 1){
	rewrite ^.*$ /ie.html
}
```
set关键字用于设置变量，方便后续的使用。

### 4. URI内部转发说明 
在nginx中，如果匹配到一个URI并rewrite URI，nginx会做一次内部的转发，这次内部的转发行为仍然会走一次配置文件的指令。

像location，rewrite等指令仍然会再次一次的生效，下面的例子就是因为这个原因发生了无限重定向。

### 5. rewrite重写实战

```
# 例如将xxx.com/ecshop/upload/goods-1.html
# 重写成xxx.com/ecshop/upload/goods.php?id=1

location /ecshop {
	root /var/www/html;
	rewrite goods-(\d+)\.html goods.php?id=$1;
	rewrite articles-(\d+)\.html articles.php?id=$1;
}


# http://192.168.0.125/ecshop/upload/category.php?id=3&brand=1&price_min=200&price_max=1700&filter_attr=167.229.202.186
# id，类目id
# brand：品牌id
# price_min，最低价格
# price_max，最大价格
# filter_attr，

# category-3-b1-min200-max1700-attr167.216.202.199.html

正则表达式：category-(\d+)-b(\d+)-min(\d+)-max(\d+)-attr([\d\.]+)\.html 
重写URI：category.php?id=$1&brand=$2&price_min=$3&price_max=$4&filter_attr=$5

# 中文的匹配
# -.*
```

注1：location的优先级别，精准 => 正则 => 一般
注2：rewrite命令是有匹配顺序的，先匹配的优先执行，后续的不执行

注3：在rewrite重写中，正则表达式如果有{}的话，那么这个表达式要用双引号括起来

个人感觉location命令可以给你做一起大方向的匹配，设计到更精细的，使用rewrite来正则匹配。

## nginx手册
nginx服务器的配置很多，和nginx相关的第三方模块也很多，所以在你碰到一个关于配置方面的问题时，可以在官方网的文档上去查找问题。

一般的，每个指令或模块都会单独说明清楚的。
```
指令
------
syntax：这是指令或模块的语法
default：该指令的默认值
context：该指令可生效的上下文，即包含在哪些模块当中。
```