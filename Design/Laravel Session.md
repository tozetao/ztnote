Session原理

- 服务器会为客户端生成一个Session Id，通过该Session Id来标识用户。

- 当生成Session Id后，服务器会设置响应头，Set-Cookie=Session Id，让客户端使用cookie来保存Session Id。
- 客户端会使用cookie保存Session Id，在之后的每次请求如果有cookie都会发送该cookie，以此来区分用户。







```php
Illuminate\Session\SessionServiceProvider
```

负责在容器中注册Session服务。



```php
Illuminate\Session\Middleware\StartSession
```

Session中间件，提供Session服务的启动和关闭时的处理工作。StartSession实例化时会注入SessionManager对象。



```php
Illuminate\Session\SessionManager
```

管理Session驱动对象。



```php
namespace Illuminate\Contracts\Session;

interface Session
{
    
}
```

由框架定义的Session对象接口。



```php
namespace Illuminate\Session;

class Store implements Session{}
```

以文件存储数据的Session驱动对象，实现了Session接口。



```php
SessionHandlerInterface
{
    // 
    public function open($savePath, $sessionName);
    
    public function close();
    
    // 读取$sessionId相关的数据
    public function read($sessionId);
    
    // 将与$sessionId关联的数据写入到存储系统中
    public function write($sessionId, $data);
    
    // 从持久化存储中移除$sessionId关联的数据
    public function destory($sessionId);
    
    // 销毁过期的session数据
    public function gc($lifetime);
}
```

PHP原生Session操作接口。



```php
namespace Illuminate\Session;

class FileSessionHandler implements SessionHandlerInterface {}
```

FileSessionHandler实现了SessionHandlerInterface接口，Store对象将会用该对象来存取Session数据。





开始处理请求时做了什么处理？

- 根据Session配置，获取Session驱动对象。
- Session驱动对象根据Session Id，初始化Session数据并启动Session服务



如何创建驱动对象?

SessionManager管理驱动对象的实例化。框架内置的驱动SessionManager会以create + 驱动名 + driver组成的方法名来生成对象。如果是扩展的驱动也可以自定义生成驱动的方法名。



响应请求时做了什么处理？

对Session数据进行了存储。



session start：启动Session时，会根据Session ID来加载Session数据。如果Session ID不合法则会重新生成ID。

session save：将session ID关联的数据存储起来。

session gc：以一定的概率来进行gc，gc时会找出所有过期的session 文件，然后清空掉。









客户端cookie所存储的session id为什么每次都发生变化？

vendor/laravel/framework/src/Illuminate/Cookie/Middleware/EncryptCookies.php，因为该中间件每次都会对session ID进行加密，体现在客户端就是cookie存储的session ID一直在发生变化。



