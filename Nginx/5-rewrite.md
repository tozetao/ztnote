## Rewrite
所谓rewrite无论在apache或nginx，指的是将通过正则表达式去匹配URI，然后替换掉成你想要的URI，再进行重定向的事情。

### 1. rewrite指令
常用命令，最常见的是正则表达式，其次是rewrite重写命令。
```
rewrite指令
	语法：rewrite regex replacement [flag]
	默认值：无
	作用域：server、location、if

参数说明
	regex：正则表达式
	replacement：要重写的地址
	flag：一些额外选项
	作用域：指令所能生效的范围

使用说明
	如果regex（正则表达式）匹配URI，URI就按照replacement进行重写。
	在apache中.htaccess文件中的重写模块的正则表达式所匹配的URI似乎是跟.htaccess文件所处的位置有关系，不知道nginx有什么不同。

```

### 2. if指令
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