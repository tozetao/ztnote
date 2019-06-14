### Facade (外观模式)

外观模式由外观角色(Facade)、子系统角色(Subsystem)和客户角色(Client)组成。

- 子系统角色(Subsystem)

​      子系统对外提供服务，对子系统来说，Facade和Client角色是未知的。也就是说子系统没有指向Facade角色的应用。

- 外观角色(Facade)

  外观模式的核心，它由客户(Client)角色调用，知道各个子系统的功能，它会根据客户角色的需求预定了几种功能的组合。

- 客户角色(Client)

  调用Facade角色来完成相应的功能。

```php
// 需求模拟：
//     一个电源总开关，可以控制四盏灯、一个风扇、一个空调和一台电视机的启动和关闭。该电源总开关可以控制上述所有电器设备，
//     那么电源总开关即为该系统的外观模式设计。

/**
 * 灯泡
 */
class Light {
    private $isOpen = 0;

    public function on()
    {
        $this->isOpen = 1;
        echo 'Light is open <br/>';
    }

    public function off()
    {
        $this->isOpen = 0;
        echo 'Light is off <br/>';
    }
}

/**
 * 风扇
 */
class Fan {
    private $isOpen = 0;

    public function on()
    {
        $this->isOpen = 1;
        echo 'Fan is open <br/>';
    }

    public function off()
    {
        $this->isOpen = 0;
        echo 'Fan is off <br/>';
    }
}

/**
 * 外观模式
 */
class Facade {
    private $fan;
    private $light;

    public function __construct()
    {
        $this->fan = new Fan();
        $this->light = new Light();
    }

    public function method1($status)
    {
        if ($status === 1) {
            $this->fan->on();
            $this->light->on();
        } else {
            $this->fan->off();
            $this->light->off();
        }
    }
}

class Client {
    public static function open()
    {
        $f = new Facade();
        $f->method1(1);
    }

    public static function close()
    {
        $f = new Facade();
        $f->method1(0);
    }
}

Client::open();
```

简单的说，外观角色隐藏了调用子系统的复杂度。





### Laravel的Facade

Laravel中的Facade是一个容器中的对象的静态代理，通过Facade能够以静态的方式来调用存放在容器中对应对象的方法。



### 使用

要实现一个Facade对象，需要继承Illuminate\Support\Facades\Facade类并实现getFacadeAccessor()方法，getFacadeAccessor()方法是注册在容器中对象的key。

```php
use Illuminate\Support\Facades\Facade;
use Cache;

class Cache extends Facade
{
    // 获取组件注册名称
    protected static function getFacadeAccessor() { 
        return new Cache; 
    }
}
```





### 实现原理

Illuminate\Support\Facades\Facade源码：

```php
abstract class Facade
{
    /**
     * The application instance being facaded.
     *
     * @var \Illuminate\Contracts\Foundation\Application
     */
    protected static $app;

    /**
     * The resolved object instances.
     *
     * @var array
     */
    protected static $resolvedInstance;


    // 获取Facade对应的对象
    public static function getFacadeRoot()
    {
        return static::resolveFacadeInstance(static::getFacadeAccessor());
    }

    // 返回已注册的组件名
    protected static function getFacadeAccessor()
    {
        throw new RuntimeException('Facade does not implement getFacadeAccessor method.');
    }

	// 从容器中解析Facade对应的对象
    protected static function resolveFacadeInstance($name)
    {
        if (is_object($name)) {
            return $name;
        }

        if (isset(static::$resolvedInstance[$name])) {
            return static::$resolvedInstance[$name];
        }

        return static::$resolvedInstance[$name] = static::$app[$name];
    }
    
    // 处理对象的动态static调用
    public static function __callStatic($method, $args)
    {
        $instance = static::getFacadeRoot();

        if (! $instance) {
            throw new RuntimeException('A facade root has not been set.');
        }

        return $instance->$method(...$args);
    }
}
```

Laravel的Facade主要通过PHP的__callStatic魔术方法，实现以静态的方式来调用存放在容器中对应对象的方法。

```php
    // 处理对象的动态static调用
    public static function __callStatic($method, $args)
    {
        $instance = static::getFacadeRoot();

        if (! $instance) {
            throw new RuntimeException('A facade root has not been set.');
        }

        return $instance->$method(...$args);
    }
```

__callStatic是PHP中一个魔术方法，当调用一个不存在的静态方法时该方法就会被调用。Laravel的实现很简单，即解析Facade对象的实例对象，然后调用该对象的方法。

```php
// 返回已注册的组件名
protected static function getFacadeAccessor()
{
    throw new RuntimeException('Facade does not implement getFacadeAccessor method.');
}
```

getFacadeAccessor()方法默认实现是抛出异常，因此继承自Facade类的子类都需要实现该方法，否则会抛出异常。