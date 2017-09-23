### gzip
- Accept_Encoding：客户端所支持的内容的编码格式
- Content-Encoding：服务端说明内容体的编码格式

这俩个报头决定采用什么编码格式传输正文，原理如下：
1. 浏览器发出请求，通过Accept-Encoding请求头带上自己支持的内容的编码格式列表
2. 服务端收到请求，从Accept_Encoding头挑选一种编码格式进行编码，并通过Content-Encoding响应头说明内容的编码格式
3. 浏览器拿到相应的内容后，根据响应头Content-Encoding进行解压。

总的来说，内容编码的目的是优化传输内容大小，一般经过gzip压缩过的文本内容，只有原始大小的1/4，对于文本内容推荐开启压缩，而对于JPG/PNG这类高度压缩过的二进制文件，不推荐开启内容压缩。

注1：服务端也可以返回没有经过压缩的数据，但是这时候一定不能携带Content-Encoding响应头。
注2：在HTTP/1中，内容编码只针对传输正文，头部信息始终是ASCII文本传输，HTTP/2可以压缩头部信息




## Nginx gzip_module
### 1. gzip

```
http{
	gzip on;
	gzip_buffers 4 4k/8k;
	gzip_comp_level 1..9
}
```


如果开启了压缩会有Content-Encoding报头，同时可以用响应的内容长度跟服务器的内容大小进行对比。
