## rewrite module

rewrite_module提供正则替换URI，返回页面重定向和按条件选择配置。

rewrite_module模块的指令按以下规则顺序处理：

1. 处理在server级别中定义的模块指令
2. 为请求查找lcation
3. 处理在选中的location中定义的模块指令，如果有指令改变了URI，按新的URI查找Location，这个过程循环至多到10次，之后nginx返回500错误。





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

regex是正则表达式，匹配的是URI，replacement是要替换URI的参数。

如果指定的正则表达式能匹配URI，此URI将会被replacement参数定义的字符串改写。rewrite指令按其在配置文件中出现的顺序执行。



flag指令能终止后续指令的执行。如果replacement的字符串是以http://或https://开头，nginx将结束执行过程，并返回客户端一个重定向。

如果正则表达式包含字符 "}" 或者字符 ";"，整个表达式应该被包含在单引号或者双引号之中。



可选的flag参数有：

- last:

  停止当前这一轮的rewrite module指令集，然后查找匹配改变后URI的新location

- break

  停止当前这一轮的rewrite module指令集

- redirect

  在replacement字符串以http://或以https://开头时，使用返回状态码为302的临时重定向。

- permanent

  返回状态码为301的永久重定向。



注：rewrite命令是有匹配顺序的，先匹配的优先执行，后续的不执行



example：

```yaml
server {
	...
	rewrite ^(/download/.*)/media/(.*)\..*$ $1/mp3/$2.mp3 last;
	rewrite ^(/download/.*)/audio/(.*)\..*$ $1/mp3/$2.ra last;
	return 403;
	...
}
```

rewrite用于server上下文中。若请求的URI匹配这俩个正则表达式，就会内部重定向到对应的文件中，没有匹配则返回403。

rewrite在location中应用时要注意重复循环匹配。比如以下配置：

```yaml
location /download {
	rewrite ^(/download/.*)/media/(.*)\..*$ $1/media/$2.mp3 last;
}
```

$1是正则表达式子匹配（第一个小括号）的内容，内部跳转的URI为/download/abc/media/1.mp3，会被location再次匹配到，因为会一直重复循环，所以flag需要使用break。

注：该例子只是为了说明这种问题的出现。



















### if

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



### set
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



### rewrite example

example：

```
location / {
     index  index.php index.html index.htm;
     if (!-e $request_filename) {
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

