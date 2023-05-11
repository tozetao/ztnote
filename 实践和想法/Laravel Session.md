Session原理

- 服务器会为客户端生成一个Session Id，通过该Session Id来标识用户。

- 当生成Session Id后，服务器会设置响应头，Set-Cookie=Session Id，让客户端使用cookie来保存Session Id。
- 客户端会使用cookie保存Session Id，在之后的每次请求如果有cookie都会发送该cookie，以此来区分用户。





### SessionServiceProvider

```php
Illuminate\Session\SessionServiceProvider
```

负责在容器中注册Session服务。



```php
Illuminate\Session\Middleware\StartSession
```

Session中间件，提供Session服务的启动和关闭时的处理工作。StartSession实例化时会注入SessionManager对象。



```php
public function register()
{
    $this->registerSessionManager();
    $this->registerSessionDriver();
    $this->app->singleton(StartSession::class);
}

protected function registerSessionManager()
{
    $this->app->singleton('session', function ($app) {
        return new SessionManager($app);
    });
}

protected function registerSessionDriver()
{
    // 使用之前注册的SessionManager来初始化驱动。
    $this->app->singleton('session.store', function ($app) {
        return $app->make('session')->driver();
    });
}
```

这里注册了SessionManager对象。sesion driver也是SessionManager进行初始化的。







### SessionManager

```php
Illuminate\Session\SessionManager
```

管理Session驱动对象，继承自Manager抽象类。



```php
abstract class Manager
{
    // 依赖注入的时候传递进来的app实例
    protected $app;
    
    public function driver($driver = null)
    {
        $driver = $driver ?: $this->getDefaultDriver();
            
        // 未配置驱动则抛出异常
        if (is_null($driver)) {
            throw new InvalidArgumentException(sprintf(
                'Unable to resolve NULL driver for [%s].', static::class
            ));
        }

        // 如果给定的驱动程序之前没有被创建，在这里创建实例，并缓存起来。
        // 如果该实例已经创建，就可以直接返回。
        if (! isset($this->drivers[$driver])) {
            $this->drivers[$driver] = $this->createDriver($driver);
        }

        return $this->drivers[$driver];
    }
	
    protected function createDriver($driver){...}
}
```

driver()方法会初始化配置文件指定的driver，并返回该driver对象。



```php
protected function createDriver($driver)
{
    // 首先，我们将确定给定的驱动是否存在自定义驱动创建者，如果不存在，我们将检查该驱动程序是否存在创建者方法。自定义创建者回调允许开发人员使用Closures自己的驱动程序。
    if (isset($this->customCreators[$driver])) {
        return $this->callCustomCreator($driver);
    } else {
        $method = 'create'.Str::studly($driver).'Driver';

        if (method_exists($this, $method)) {
            return $this->$method();
        }
    }
    throw new InvalidArgumentException("Driver [$driver] not supported.");
}
```

这里注意下createDriver()方法，在这里我们可以自定义自己的驱动程序，customCreators()用于创建我们自定义的驱动，当然要使用对应的extend方法注册驱动对应的实例化回调函数。

如果没有自定义驱动，那么会判断是否有该驱动的创建方法，如果有就实例化否则抛出异常。





#### FileDriver

FileDriver是文件驱动，也是默认的原生驱动。

SessionManager的方法：

```php
protected function createNativeDriver()
{
    $lifetime = $this->app['config']['session.lifetime'];

    return $this->buildSession(new FileSessionHandler(
        $this->app['files'], $this->app['config']['session.files'], $lifetime
    ));
}
```

文件驱动就是使用文件来存储数据。在这里是获取session相关配置然后实例化对象，这个实例对象最终由Manager的deiver()方法返回的。



SessionManager的方法：

```php
protected function buildSession($handler)
{
    // 是否有配置session加密，如果有则构建一个加密的session，否则新建一个Store对象。
    return $this->app['config']['session.encrypt']
        ? $this->buildEncryptedSession($handler)
        : new Store($this->app['config']['session.cookie'], $handler);
}
```

构建一个Session实例对象。然后根据配置会返回俩种类型的Session，一种是加密的Session，一个是基于Cookie的Session。这俩种Session我们放到后面来看，先看看handler对象。

$handler是实现了SessionHandlerInterface接口的类，SessionHandlerInterface定义了session数据打开、关闭、存储、读取、销毁等接口，简单的说定义了处理Session数据的接口。

FileDriver的handler是FileSessionHandler实现的。我们先来看看SessionHandlerInterface定义了哪些方法，再来看具体的实现。

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



#### FileSessionHandler

```php
public function __construct(Filesystem $files, $path, $minutes)
{
    $this->path = $path;
    $this->files = $files;
    $this->minutes = $minutes;
}
```

path是存储的目录，files是Filessystem系统实例对象，$minutes是存活时间，单位是分钟。



```php
// 读取数据的实现
public function read($sessionId)
{
    if ($this->files->isFile($path = $this->path.'/'.$sessionId)) {
        if ($this->files->lastModified($path) >= Carbon::now()->subMinutes($this->minutes)->getTimestamp()) {
            return $this->files->sharedGet($path);
        }
    }

    return '';
}
```

subMinutes()是减去若干分钟，在这里是当前时间减去有效期时间后的时间戳。

文件的最后一次修改时间 >= （当前时间 - 存活时间），那么该文件认为就是有效的。



```php
// 写入数据的实现。
public function write($sessionId, $data)
{
    $this->files->put($this->path.'/'.$sessionId, $data, true);
    return true;
}
```

将数据写入到session id文件中。



```php
// 销毁数据的实现
public function destroy($sessionId)
{
    $this->files->delete($this->path.'/'.$sessionId);
    return true;
}
```



```php
// gc处理
public function gc($lifetime)
{
    $files = Finder::create()
        ->in($this->path)
        ->files()
        ->ignoreDotFiles(true)
        ->date('<= now - '.$lifetime.' seconds');

    foreach ($files as $file) {
        $this->files->delete($file->getRealPath());
    }
}
```









#### Filesystem

以下分析Filesystem类的实现。



```php
public function sharedGet($path)
{
    $contents = '';

    $handle = fopen($path, 'rb');

    if ($handle) {
        try {
            if (flock($handle, LOCK_SH)) {
                clearstatcache(true, $path);

                $contents = fread($handle, $this->size($path) ?: 1);

                flock($handle, LOCK_UN);
            }
        } finally {
            fclose($handle);
        }
    }

    return $contents;
}
```

用于获取可共享访问的文件内容，可以看到这里加了共享锁，为了保证数据的一致性。



```php
// 写入内容
public function put($path, $contents, $lock = false)
{
    return file_put_contents($path, $contents, $lock ? LOCK_EX : 0);
}
```

put方法用户写入数据。$lock参数标识是否加锁。



```php
// 删除文件
public function delete($paths)
{
    $paths = is_array($paths) ? $paths : func_get_args();

    $success = true;

    foreach ($paths as $path) {
        try {
            if (! @unlink($path)) {
                $success = false;
            }
        } catch (ErrorException $e) {
            $success = false;
        }
    }

    return $success;
}
```







### StartSession

StartSession在实例化的时候就被注入了SessionManager。

```php
public function getSession(Request $request)
{
    return tap($this->manager->driver(), function ($session) use ($request) {
        $session->setId($request->header($session->getName()));
    });
}
```

首先驱动管理对象创建驱动，也就是上面buildSession()方法返回来的Session对象，然后设置Session的id。

```php
protected function startSession(Request $request)
{
    return tap($this->getSession($request), function ($session) use ($request) {
        $session->setRequestOnHandler($request);
        $session->start();
    });
}
```

接着将Request对象注入到session中，同时初始化session，最后将session对象返回。



```php
$request->setLaravelSession($session);
```

这里是request对象绑定session对象。



```php
// session gc
protected function collectGarbage(Session $session)
{
    $config = $this->manager->getSessionConfig();

    if ($this->configHitsLottery($config)) {
        $session->getHandler()->gc($this->getSessionLifetimeInSeconds());
    }
}

protected function configHitsLottery(array $config)
{
    return random_int(1, $config['lottery'][1]) <= $config['lottery'][0];
}
```

session的gc，这里是1/50的几率去清理session文件。







### Store

#### Session接口

Store是实现了Session接口的类，实现了Session操作接口。我们先来看看Session定义的接口。

```php
namespace Illuminate\Contracts\Session;

interface Session
{
    // 获取session的名字
    public function getName();
    // 获取当前session的id
    public function getId();
    
    // 设置Session的id
    public function setId($id);
    
    // 启动session，使用handler读取数据
    public function start();
    
    // 存储session的数据到储存空间
    public function save();
    
    // 获取session所有数据
    public function all();
    // 清空session所有数据
    public function flush();
    
    // 判断某个key存在
    public function exists($key);
    // 检查一个key是否存在且不为空
    public function has($key);
    
    // 从session中获取一项数据。
    public function get($key, $default = null);
    // 在session中放置键值对或键值对的数组
    public function put($key, $value = null);
    
    // 从session中移除指定项的数据，并返回它。
    public function remove($key);
    // 从session中移除一项或多项数据
    public function forget($keys);
    
    // 为session生成一个新的session id
    public function migrate($destory = false);
    
    //...
}
```

可以看到定义了Session操作的基本接口。接下来看看Store是怎么为一个session id存储和设置数据。



#### Store实现

```php
namespace Illuminate\Session;

public function __construct($name, SessionHandlerInterface $handler, $id = null){}
```

控制器方法主要初始化了session name和SessionHandler。



```php
public function setId($id)
{
    $this->id = $this->isValidId($id) ? $id : $this->generateSessionId();
}

public function isValidId($id)
{
    return is_string($id) && ctype_alnum($id) && strlen($id) === 40;
}
```

该方法用于设置session id。如果id不是有效的，那么重新生成一个id。



```php
public function start()
{
    // 加载session
    $this->loadSession();
    // 如果没有token则重新生成
    if (! $this->has('_token')) {
        $this->regenerateToken();
    }
    // 将启动状态改为true
    return $this->started = true;
}

protected function loadSession()
{
    $this->attributes = array_merge($this->attributes, $this->readFromHandler());
}

protected function readFromHandler()
{
    // 通过handler读取数据
    if ($data = $this->handler->read($this->getId())) {
        // 反序列化数据并返回。
        $data = @unserialize($this->prepareForUnserialize($data));
        if ($data !== false && ! is_null($data) && is_array($data)) {
            return $data;
        }
    }

    return [];
}
```

这里是启动session的方法。



```php
public function put($key, $value = null)
{
    if (! is_array($key)) {
        $key = [$key => $value];
    }
    // 如果key是一个键值对数据，则会遍历并存储到$attributes中。
    foreach ($key as $arrayKey => $arrayValue) {
        Arr::set($this->attributes, $arrayKey, $arrayValue);
    }
}
```

put方法实现了向session中放置数据的逻辑。



```php
public function save()
{
    $this->ageFlashData();

    $this->handler->write($this->getId(), $this->prepareForStorage(
        serialize($this->attributes)
    ));

    $this->started = false;
}
```

save()方法是session中的数据更新到存储介质中，在这一步数据才会被存储起来。这里的实现逻辑是将attributes序列化，然后使用handler写入到session id对应的存储空间中。

注意：调用方法后，session的启动状态就变为false了。



ageFlashData()是老化闪存数据，暂时不看具体实现。





StartSession构造方法依赖注入的SessionManager对象，与app对象make出来的SessionManager是同个实例对象。这意味着我不能直接获取应用中注册的SessionManager对象的deiver来去操作别人的Session数据了，必须新创建一个对象来进行操作。

