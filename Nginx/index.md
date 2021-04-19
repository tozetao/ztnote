
## index指令
index是一个content阶段的命令，仅处理request_uri结尾为"/"的请求。

处理请求逻辑：
- 对于/结尾的请求，nginx会根据index指令配置的多个文件进行顺序查找，看文件是否存在
- 如果存在，会结束查找过程，把这个文件附加在request_uri结尾后面，并发起一个内部的redirect
- 如果尝试后全部不存在，那么该index指令执行结束，nginx会执行content阶段后的下一个指令的事情

example：
```
server {
	listen 80;
	server_name www.abc.com;
 
	index index.html index.php index.htm;
	
	location / {
		root /home/web/php/;
	}

	location = /index.html {
		root /home/web/html/;
	}
}
# 说明：php目录下有index.php文件，html目录下有index.html文件

# 在访问www.abc.com时，uri"/"会先被location匹配
# 接下来content阶段index指令会被执行，nginx会在/home/web/php目录下寻找index配置的文件

# 找到index.php文件，发起一次内部重定向/index.php，第一个location又被匹配到，所以是返回php目录下的index.php文件
```
注：index指令会发起一次内部重定向，因为nginx会使用index指令，将配置的文件附加到request_uri，之后再进行





### nginx执行阶段
1. 分析uri，如果uri对应web服务器目录，nginx会自动用/补全uri，如果不是uri的最后段会作为文件处理
2. 执行请求头处理
3. 执行nginx配置脚本处理
4. 进入rewrite阶段，重写阶段我已知的包括try_files阶段、content阶段，location匹配阶段
5. 如果location匹配成功会执行对应配置，在这里如果发生内部重定向会继续返回第4步，继续执行rewrite阶段。
6. 继续往下执行...

注：不要配置/的location，会影响nginx自动补全"/"


try_files{}
所以在location / {}服务器影响了

例如：/blog/test uri，在web服务器上有对应的目录访问时uri在浏览器会变成/blog/test/,在服务器会去加载/blog/test/index.html文件