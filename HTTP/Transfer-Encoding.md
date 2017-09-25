## Transfer-Encoding
Transfer-Encoding是HTTP协议中的报头，意为传输编码；

与Content-Encoding不同，Content-Encoding是内容编码，用于对传输主体内容进行编码压缩，减少传输过程中的体积，而Transfer-Encoding传输编码是用于改变报文格式；这俩个报头是相辅相成的，对于一个请求，很可能同时进行了内容编码和传输编码。

### HTTP长连接(Connection)
HTTP协议是基于TCP/IP协议簇的，所以HTTP协议也会受到TCP三次握手、初始化等影响，因为这点引入了持久连接的机制。

HTTP长连接通过Connection: keep-alive报头来实现的，服务端和客户端都能够告诉对方在发送完数据之后不要断开TCP连接，以备后用，HTTP1.1规定所有连接都必须是持久的，除非显示的关闭连接Connection：close。

### Content-Length
Content-Length报头会计算实体内容的长度字节并告诉对方，主要解决客户端和服务器处于长连接时，如果服务器不告诉客户端相应内容的长度，会导致客户端一直处于等待状态而不解析内容。

Content-Length必须反映实体内容字节大小，如果服务端响应的内容比实际内容短，会导致解析内容时被截断，如果响应的内容比实际内容长，会造成pending。

缺点：如果实体内容由动态内容生成，在内存中会有buffer缓存起来，如果实体内容过大会导致开启过大buffer，同时让客户端等待更久

### Transfer-Encoding
Transfer-Encoding是传输编码，在最新的HTTP中值定义了chunked一个值，即分块传输。

在响应头部加入Transfer-Encoding：chunked这个报头，就代表这个报文采用分块编码。分块编码会将实体内容分成一系列块来传输，每个块包含十六进制的长度值和数据，长度值独占一行，长度值不包括块内容结尾的CRLF(\n\r)，也不包括它自身结尾的CRLF(\n\r)，最后一个块的长度必须是0，对应的分块是没有内容。

```php

//写入11字节大小的块内容
socket.write("b\n\r");
socket.write("01234567980\n\r");

//写入最后一块内容
socket.write("0\n\r");
socket.write("\n\r");

```

如果服务器使用了压缩技术，因为会将数据分段传输'transfer-encoding': 'chunked'，所以在响应头中是没有Content-Length的，

