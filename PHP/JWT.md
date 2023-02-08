jwt

json web token，其实就是一个token，可以理解为签证的意思。



header.payload.signature



header

头部信息，指定jwt使用的签名算法

经过base64编码过的header就是要传递的头部信息。



payload

​    消息体，这是一个JSON对象，包含需要传递的数据。

payload由公有字段和私有字段组成，公有字段是jwt定义的字段，私有字段则是业务字段。



经过base64编码后的payload就是要传递的消息体。



signature

通过头部信息指定的签名算法，使用密钥对字符串（base64编码的header + "." + base64编码的payload）

进行加密，得到的加密字符串就是签名。



经过base64编码后的签名就是要传递的签名。



jwt使用流程

- 客户端发送用户名和密码，服务端进行验证，如果用户名和密码正确则发放token，否则提示相关错误信息。
- 在后续的请求中，客户端会在发送请求时会携带该签名，而服务端会验证该签名是否合法。





什么是jwt

优点

缺陷

CSRF攻击

token的过期设计



如果一个token过期了，允许在过期后的一定时间内对其进行刷新，这个值可以根据需求自定义。

因此我们会有俩个token，一个用于鉴权，一个用于刷新过期的token。

refresh token时一个一次性token，在刷新完鉴权token后会再次发放。















jwt的缺陷

jwt本身是无状态的，并且一个已经签发的jwt，在过期之前它是一直有效的。

因此它无法做到统计在线用户的人数、强行踢掉一个在线用户或者让一个未过期的jwt失效。对于一些有状态的需求，仍然需要Session的支持。



认证信息的存储







jwt有状态的实现

通过Session的配置来实现有状态的jwt，jwt的payload中保存SessionId，这样jwt就是有状态的。如果jwt的SessionId所对应服务器的Session数据不存在时，该jwt即使未过期也是无效的。



https://www.jianshu.com/p/805dc2a0f49e



web认证：

无论是使用Session或Token做认证，总要保证服务器可以管理Session，通过Session的存在来确认认证的有效性。



https://www.jianshu.com/p/805dc2a0f49e

https://blog.csdn.net/guyan0319/article/details/84879110

https://blog.csdn.net/lzy_zhi_yuan/article/details/73127601

https://www.cnblogs.com/chevin/p/5669940.html





实现一个基于token的session机制。

超时：在用户一段时候没有访问后，服务端session将会过期。



```go
//定义Session的操作接口
type Session interface {
    Get(key string) interface{}, error
    
    Set(key string, interface{}) error
    
    Delete(key string) error
    
    SessionId() string, error
}

//Session服务提供者，只要符合接口规范，可以以任意形式来实现Session的存储
type SessionProvider interface {
    //启动或复用一个请求的Session对象
    SessionStart(r *http.Request, w http.ResponseWriter) *Session, error
    
    //销毁一个请求的Session对象
    SessionDestory(r *http.Request, w http.ResponseWriter) error
    
    //对过期的Session进行垃圾回收
    GC()
}

var provider SessionProvider

func Init() {
    provider = MemeryStore()
}

func Start(r *http.Request) (*Session, error) {
    return provider.SessionStart()
}

func Destory(r *http.Request) {
    return provider.SessionDestory()
}

func GC() {
}

/*
Provider是全局可访问的，在Handler中也可以调用。

需要一个map容器，以键值对存储Session对象。
需要一个List，存储已经创建的Session对象，用于gc

SessionId -> map

session应该在哪里启动?
- 有一个全局初始化Session容器的地方

session如何获取?
- 每个请求都能够获取自己的Session对象。

*/
```





























