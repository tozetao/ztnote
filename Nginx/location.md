## location

location用于定义uri匹配规则，根据uri来进行不同的定位。



语法：

> location [= | ~ | ~* | @ | ^~]  pattern

[]中的内容是匹配修饰符，表示不同的匹配模式，pattern则是匹配表达式。如果不使用匹配修饰符，表示普通字符匹配。

匹配修饰符：

- =

  = 修饰符表示普通字符串的精准匹配

- ~

  ~ 表示区分大小写的正则表达式匹配

- ~*

  ~* 表示不区分大小写的正则匹配

- ^~

  表示普通字符串匹配，如果该选项匹配，只匹配该选项，不匹配别的目录，一般用来匹配目录

- @

  定义一个命名的location，在内部定向时使用，例如：error_page，try_files



匹配优先级：

- = 修饰符的location是精准匹配的，如果搜索到匹配的规则，会立刻停止搜索。
- 搜索普通字符匹配、普通字符最长的匹配，如果该普通字符匹配使用^~修饰符，停止搜索。
- 继续搜索正则表达式匹配，按照配置文件定义的顺序。

location匹配优先级可以总结为：精准匹配的优先级最高，正则匹配优先级大于普通匹配，如果正则匹配（~和~*修饰符定义的location）失败，将使用普通匹配中location规则最长的匹配。



精准匹配

```
location = /demo.html {
	[configuration A]
}
# /demo.html的uri会被匹配，缺少任意一个字符都不会匹配
```



普通匹配

```conf


location / {
	[configuration A]
}
# 普通字符串匹配，该location匹配任何请求，因为所有请求都是以/开始的
# 但是更长的字符匹配或者正则表达式会优先匹配

# blog.com，匹配
# blog.com/index.html，匹配
```



多个普通匹配

```
server{
	location / {
		root /var/www
		index index.html
	}

	location /foo {
		root /var/www/html
		index index.html
	}
}

# 多个普通字符匹配，Nginx会将匹配结果存储起来
# 如果没有正则匹配，则会取最长字符的普通匹配作为结果返回
```



普通匹配和正则匹配

```
location / {
	[configuration A]
}

location /images {
	[configuration B]
}

location ~ images {
	[configuration C]
}

# 如果普通匹配和正则匹配都成功，将会执行正则匹配的配置内容
```

