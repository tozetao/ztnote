## location解析

### 1. location语法
location有定位的意思，在nginx中主要是用于解析URI，根据URI来进行不同的定位。在虚拟主机的配置中，location是必不可少的，它可以把网站的不同部分定位到不同的处理方式上。

例如碰到.php就可以调用php的解释器，语法：
```shell
location [=|~|~*|^~] patt {
}

# patt是匹配表达式
# []的内容是修饰符
```
中括号可以不写任意参数，此时称为一般匹配，也可以有参数，大体是3种类型：
```
location = patt{}
# 精准匹配

location patt{}
# 一般匹配

location ~patt{}
# 正则匹配
```

### 2. 匹配过程
1. 先判断是否精准匹配，如果命中立即返回执行结果并结束解析过程
2. 判断普通命中，如果有多个命中，"记录"下来"最长"的命中结果（注：记录但不结束，最长的为准）
3. 继续判断正则表达式的解析结果，按配置里的正则表达式顺序为准，从上到下开始匹配，一旦匹配成功1个就立即返回结果，并结束解析过程。
4. 

example：精准匹配
```
server{
	listen 80;
	server_name localhost;
	
	location = /a.txt {
		root /var/www/html;
		index index.html index.htm
	}
}
# 在上述的配置中，= 表示精准匹配，而表达式/a.txt只会匹配URI是/a.txt的网址
# 匹配成功则会在/var/www/html目录下寻找该文件

# index，设置根目录默认匹配的文件
# root，设置网站根目录，如果一个表达式匹配成功，那么会使用URI在root指定的目录下寻找文件
```

example：精准匹配和普通匹配
```
server{
	listen 80;
	server_name localhost;
	
	location = / {
		root /var/www/html;
		index index.htm
	}
	# 
	location / {
		root html;
		index index.htm
	}
}
# 在上述配置中，第1个location是精准匹配
# 当网址是xxx.com的时候，精准匹配到"/"，因此使用第一个location的配置，得到URI"/index.htm"
# 此时URI是"/index.htm“，将会匹配到第二个location，因此会在/nginx/html目录下查找文件

# 注： "/"根目录会根据匹配到的location内容再次匹配，如果
```

example：普通匹配和正则匹配
```
server{
	location / {
		root html
		index index.html
	}

	location ~ images {
		root images
		index index.html
	}
}
# 如果普通匹配和正则匹配都成功，那么正则匹配会覆盖普通匹配的结果。
# 正则表达式是按照配置文件从上到下的顺序来进行匹配的，谁先匹配到就先返回结果。
```

example：多个普通匹配
```
server{
	location / {
		root html
		index index.html
	}

	location /foo {
		root html
		index index.html
	}

	location ~ images {
		root images
		index index.html
	}
}
# 如果有多个普通匹配一起命中，只会取表达式命中最长的结果匹配作为唯一的结果来执行。
# 在上述的例子中如果以xxx.com/foo网址进行访问，那么表达式/和/foo都将命中，但是/foo命中的结果较长，所以只想/foo表达式配置的结果。
```