## WebSocket

在WebSocket API中，浏览器和服务器只需要一个握手的动作，然后浏览器和服务器之间就形成了一条快速通道，倆者之间的数据可以互相传送，改变原有的B/S模式。

握手流程如下：

- 客户端向服务器发送一个http get请求
- 服务器通过客户端发送的Sec-WebSocket-Key头验证握手的请求
- 服务器握手回复，通过upgrade字段来切换到frame来传送数据

客户端发送的报文：

```
Request Url: ws://127.0.0.1:4000/
Request Method: GET
Status Code: 101 Switching Protocol

Sec-WebSocket-Key: itSwDBk/ggrRwerafjkk
Upgrade: websocket
```

重点关注Sec-WebSocket-Key请求头，服务器会按照一定规则对其进行加密再传递给客户端验证，验证通过代表握手成功。

服务器响应的报文：

```
HTTP/1.1 101 Switching Protocols
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Accept: Base64Content
\r\n
```

服务器的加密步骤如下：

- 将Sec-WebSocket-Key请求头的值解析出来
- 把该值拼接固定字符串"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"使用SHA-1加密
- 最后进行base64编码，将结果作为Sec-WebSocket-Accept响应头的值返回给客户端

客户端在收到该响应头后，验证成功就会将通信协议升级到websocket协议，然后就会在该通道下进行通信，双方是在一个全双工的状态下通信。

注：最后一行换行是必须的，参考HTTP协议。





### WebSocket通信协议

切换到WebSocket协议中的数据传输帧的格式是跟HTML协议不一样的，帧格式如下：

```erlang
<<Fin:1, Rsv:3, Opcode:4, Mask:1, PayloadLen:7>>,
<<MaskKey:4>>,
<<PayloadData:Size>>
```

- Fin

  1bit，标记位，表示信息的最后一帧

- Mask

  1bit，掩码，是否加密数据，默认必须为1

- Opcode

  4bit，帧类型

- PayloadLen

  7bit，表示携带的数据长度。PayloadLen = 126时，增加额外的2个字节表示数据长度，当它 = 127时，增加8个字节表示数据长度。

- MaskKey

  1或者4个字节，服务器要用它们来解析数据

- PayloadData

  传输的数据要用MaskKey来异或才能得到真正的数据。

服务器解析数据帧要按照上面的格式要求才能得到浏览器的数据，当要响应数据给浏览器时，也要按照这个格式来组装frame。



PayloadLen是7个bit，也就是说最多能携带128个字节的数据。因此才需要对其扩展。当 = 126时，后面2个字节就表示数据长度，能够最多携带65536个字节数据；当=127时，后面的8个字节表示携带的数据长度，最后能携带$2^{64}$个字节数据。



Opcode表示帧类型，具体有：

- 0：Continuation Frame，连续帧。
- 1：Text Frame，文本帧。
- 2：Binary Frame，二进制帧
- 8
- 9
- 10



### 解析数据帧

MaskKey是4个字节的长度，解析数据帧需要使用到MaskKey。

解析时把Data按照每4个字节来分隔，每一份中的字节从左到右依次与MaskKey对应的字节做异或运算。

比如说分隔出一份数据X，那么就会让X的第一个字节与MaskKey的第一个字节异或运算，X的第二个字节与MaskKey的第二个字节异或运算，直到异或到X的最后一个字节。

example：php代码演示

```php
// $data是payload数据，$mask是4个字节
for($i = 0; $i < count($data); $i++) {
    $data += $data[i] ^ mask[$i % 4];
}
```

example：erlang代码

```erlang
%% Len表示数据的字节长度，Bin是由4个字节的MaskKey和Len个字节的Data组成。
unpacket(Bin, Len) ->
    <<Mask:4/binary, Payload:Len/binary>> = Bin,
    unpacket(Payload, Mask, <<>>).

unpacket(Payload, <<Ma:8, Mb:8, Mc:8, Md:8>> = Mask, Acc) ->
    case size(Payload) of
        0 ->
            Acc;
        1 ->
            <<A:8>> = Payload,
            <<Acc/binary, (Ma bxor A)>>;
        2 ->
            <<A:8, B:8>> = Payload,
            <<Acc/binary, (Ma bxor A), (Mb bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = Payload,
            <<Acc/binary, (Ma bxor A), (Mb bxor B), (Mc bxor C)>>;
        _Other ->
            <<A:8, B:8, C:8, D:8, Data/binary>> = Payload,
            Acc1 = <<Acc/binary, (Ma bxor A), (Mb bxor B), (Mc bxor C), (Md bxor D)>>,
            unpack(Data, Mask, Acc1)
    end.
```





