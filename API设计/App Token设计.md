
## session和token
**Session机制**
session是基于cookie实现的，用户第一次访问网站的时候会生成一个唯一的session_id，当相应完用户的请求时会以PHPSESSID为cookie键，session_id作为cookie的值相应给客户端，同时服务器会创建一个session_id为名的文件，文件内容即是session对象序列化的内容。

当用户第二次请求是将携带该cookie，服务器会通过该PHPSESSID Cookie的值

**Session和Token**
Session 是一种HTTP存储机制，目的是为无状态的HTTP提供的持久机制。所谓 Session 认证只是简单的把 User 信息存储到 Session 里，因为 SID 的不可预测性，暂且认为是安全的。这是一种认证手段。 

而 Token ，如果指的是 OAuth Token 或类似的机制的话，提供的是 认证 和 授权 ，认证是针对用户，授权是针对 App 。其目的是让 某App 有权利访问 某用户 的信息。这里的 Token 是唯一的。不可以转移到其它 App 上，也不可以转到其它 用户 上。 

转过来说 Session 。Session 只提供一种简单的认证，即有此 SID ，即认为有此 User 的全部权利。是需要严格保密的，这个数据应该只保存在站方，不应该共享给其它网站或者第三方App。 

所以简单来说，如果你的用户数据可能需要和第三方共享，或者允许第三方调用 API 接口，用 Token 。 
如果永远只是自己的网站，自己的 App ，用什么就无所谓了。


## url签名
1. 用户发送请求，服务器验证密码和用户名，成功将token字符串"qwer1234"和uid"5"返回给客户端
2. 假设api请求为"test.com/user/info"，通过token字符串'qwer1234'、uid5、当前时间戳time()生成签名，sign: md5('test.com/user/info')
3. 客户端请求加上签名和用户标识后就是，test.com/user/info?userid=5&timestamp=time()&sign=xxx

这种签名方式主要是用过双方保存一个共同token来生成签名进行验证，timestamp用于验证请求是否被重用。
缺点：token由于是保存在客户端，被人获取到可以被别人用于伪造签名。

## JWT

### 1.jwt组成
jwt主要由header（头部）、payload（载荷）、sign（签名）组成。

**header**
header是描述jwt的基本信息，例如jwt签名算法和加密类型，可以以json对象来表现，
```
{
	'typ' : 'jwt',
	'alg' : 'HS256'
}
```

**payload**
载荷可以理解为内容主体，包括jwt定义的标准，自定义字段内容。

**sign**
签名的实现是jwt的加密最重要的部分，使用base64编码后的header、payload和一个密钥并用指定的算法来进行生成。

一个完整的jwt：
base64_encode(header).base64_encode(payload).base64_encode(sign)


### 2. 身份验证流程
流程：
- 在用户登录成功后生成一个密钥，或者由服务器分发给用户一个密钥，将这个密钥与uid关联起来
- 在载荷中payload设置主体信息，包括uid和一些其他信息
- 将header、payload进行base64编码，将编码后的header+payload+secret组成字符串并进行指定算法的加密，这样就得到了签名，
- 将编码后的header、payload和签名返回给客户端，客户端将使用jwt作为凭证，表示已经登录过了。
- 后续的操作中，判断用户是否登录过，是否有权限调用接口，只需要传递jwt即可。


jwt实现的时候有俩个过期时间：
- token过期时间，即token自身的使用时间
- token过期后的再次刷新的有效期时间，也就是说把过期的token发给服务器后，服务器判断刷新token的时间是否过期，如果没过期则重新生成一个token



问题1：让jwt强制失效
本地删除jwt，服务器删除jwt对应用户的密钥，因为密钥是存储在服务器的，验证token需要密钥，没有密钥则会验证失败，一种让jwt强制失效的方式。


问题2：给jwt添加过期时间
在payload载荷中添加签证时间、过期时间和刷新token过期时间，这样就可以给token做一个时间戳过期的判断了。


问题3：解决用户长时间不使用APP，token过期问题，场景是在X天以外。
为了避免用户多次登录，减轻客户端主动请求刷新token的操作，可以在客户端每次发起请求验证token的请求时，对旧的token刷新，新的token以 Authorization: Bearer <token> 插入到响应头当中，因为开启了黑名单，旧的token在被刷新完成加入黑名单后就再也无法使用了。

问题4：如何解决重放式攻击




### 3. 签名的作用

签名的目的是将头部信息和payload载荷信息进行加密，
加密的信息 + 密钥 = sign（签名），由于密钥是保存在服务器中，并且使用密钥对不同的载荷进行加密得出的签名基本上是不同的，因为要加密的信息是可变的。

如果有人对加密的头部和载荷进行base64解码后进行修改，那么新的头部和载荷跟之前的签名是不一样的，因为你不知道服务器的密钥所以也无法仿造签名。




## OAuth2.0的实现
- 客户端在第一次登录后，服务端会同时返回一个access token和refresh token，
- access token有失效期，相对较短，用于每次和服务端通信的身份校验。
- refresh token用于access token失效后换取新的access token时的校验，成功后返回新的access token和新的refresh token。


