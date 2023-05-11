### 依赖与耦合

一个对象实现某个功能时需要其他对象相关功能的支持就叫做依赖，简单的说在一个类中使用new关键字实例化时就引入了一个依赖。

一个对象的功能依赖于其他对象时，它们之间就产生了耦合，耦合的代码难以维护和移植，为了解耦我们会定义对象之间交互的接口，以达到解耦的目的。

```php
// 定义约束接口，解耦依赖关系
interface CacheInterface {
    public function cache($key, $value);
    public function get($key);
}

class RedisCache() implements CacheInterface {
    public function cache($key, $value) {}
    public function get($key) {}
}

class Consumer {
    private $instance;
    
    public function __construct() {
        $this->instance = new RedisCache();
    }
    
    // $instance可以是任意实现了CacheInterface接口的对象
    public function handle() {
        $instance->cache('total', 1000);
    }
}
```



### IoC概念

IoC意为控制反转，指的是获得依赖对象的过程被反转了。

在没有引入IoC概念时，如果对象A依赖于对象B，那么对象A在运行到某个点的时候会主动的创建和使用对象B，无论是创建对象和使用对象，控制权是在自己手里的，也就是说依赖关系是在代码编译期间就存在了。

就像在上面的代码中，Consumer对象是对RedisCache具体类型对象的强依赖，而IoC希望的是解除在代码编译期间对象之间的依赖关系，能够在程序运行期间为消费者对象注入依赖对象。



### 实现

实现IoC的方法称为依赖注入，一般会实现一个容器来负责管理组件之间的依赖关系。容器完成俩件事情：

- 创建服务对象（ServiceUser）
- 解析服务对象所要依赖的服务提供者（ServiceProvider），自动注入服务对象所依赖的服务提供者，完成运行时的依赖绑定关系。



```php
<?php

/**
 * 容器的核心是解析一个类的依赖关系，在实例化的时候提供自动注入依赖对象的功能。
 *
 * binad()
 *      容器支持依赖关系的绑定
 *
 *      绑定具体实例
 *      $container = new Container();
 *      $car = new Car();
 *      $container->bind('instance', $car);
 *
 *      绑定匿名函数
 *      $container->bind('instance', function($c){ return $c->make('Car'); });
 *
 *      绑定接口的实现：当解析类的依赖关系时，如果某个依赖关系是接口类型，且有绑定具体实现，那么就会采用绑定的关系来去实例化依赖对象。
 *      $container->bind('Visit', 'Car');
 *      $container->bind('', '');
 *
 * make($abstract)
 *      根据绑定的依赖关系，来生成实例对象，并自动注入依赖关系。
 *
 */
class Container {
    protected $bindings = [];

    public function bind($abstract, $concrete = null, $shared = false)
    {
        if (!$concrete instanceof Closure) {
            $concrete = $this->getClosure($abstract, $concrete);
        }
        $this->bindings[$abstract] = compact('concrete', 'shared');
    }

    /**
     * 默认生成实例的回调函数
     * @param $abstract
     * @param $concrete
     * @return Closure
     */
    protected function getClosure($abstract, $concrete) 
    {
        return function($c) use($abstract, $concrete) {
            $method = ($abstract == $concrete) ? 'build' : 'make';
            return $c->$method($concrete);
        };
    }

    /**
     * 生成对象，注入对象的依赖关系。
     * make的逻辑是：
     *     如果在容器中找到$abstract的实现($concrete)，就去make实现($concrete)。
     *     直到在容器中找不到$abstract的具体实现才会build它。
     * @param $abstract     标识，可以是接口名、类名或实例的标识。
     * @return object       返回已经注入依赖关系的对象。
     * @throws Exception
     */
    public function make($abstract)
    {
        $concrete = $this->getConcrete($abstract);

        if ($this->isBuildable($concrete, $abstract)) {
            $object = $this->build($concrete);
        } else {
            $object = $this->make($concrete);
        }

        return $object;
    }

    /**
     * 如果标识和实现是否相等，或者标识是否回调函数。
     * 如果条件为true则标识可以构建该实现（$concrete），否则会继续make该实现（$concrete）
     * @param $concrete
     * @param $abstract
     * @return bool
     */
    public function isBuildable($concrete, $abstract)
    {
        return $concrete === $abstract || $abstract instanceof Closure;
    }

    /**
     * 在容器中寻找$abstract的实现，如果找不到则返回参数本身。
     * @param $abstract
     * @return mixed
     */
    public function getConcrete($abstract)
    {
        if (@!isset($this->bindings[$abstract])) {
            return $abstract;
        }
        return $this->bindings[$abstract]['concrete'];
    }

    /**
     * 构建实例并注入的依赖对象，因为注入的对象也能恩依赖其他对象，所以注入的过程是递归的。
     * @param $concrete
     * @return object
     * @throws Exception
     */
    public function build($concrete)
    {
        // 如果是回调函数，则调用该回调函数
        if ($concrete instanceof Closure) {
            return $concrete($this);
        }

        // 反射类名，解析它
        $reflector = new ReflectionClass($concrete);

        // 如果类名不可实例化，则抛出异常
        if (!$reflector->isInstantiable()) {
            throw new Exception("Target [$concrete] is not instantiable");
        }

        // 获取类的构造函数，如果没有构造函数则直接实例化
        $constructor = $reflector->getConstructor();

        if (is_null($constructor)) {
            return new $concrete();
        }

        // 解析函数，获取一组用ReflectionParameter对象表示的参数列表。
        // 这些参数也就是类所依赖的对象，需要注入它们。
        $dependencies = $constructor->getParameters();

        // 生成所依赖的实例对象
        $instances = $this->getDependencies($dependencies);

        // 根据给出的参数，创建一个新的类实例对象
        return $reflector->newInstanceArgs($instances);
    }

    public function getDependencies($parameters)
    {
        $dependencies = [];
        foreach($parameters as $parameter) {
            // 获取参数的类型提示类，如果参数类型不是一个类则返回null
            $dependency = $parameter->getClass();

            if (is_null($dependency)) {
                // 如果参数不是一个类，则填充null
                $dependencies[] = null;
            } else {
                // 如果参数是一个类，则继续有容器来负责实例它。
                $dependencies[] = $this->resolveClass($parameter);
            }
        }
        return $dependencies;
    }

    protected function resolveClass(ReflectionParameter $parameter)
    {
        return $this->make($parameter->getClass()->name);
    }
}

interface Visit 
{
    public function go();
}

class Leg implements Visit
{
    public function go()
    {
        echo 'walt to Tibet!<br/>';
    }
}

class Car implements Visit
{
    public function go()
    {
        echo 'this is car<br/>';
    }
}

class Traveller
{
    protected $trafficTool;

    private $array;
    private $num;

    public function __construct(Visit $trafficTool, $array, $num)
    {
        $this->trafficTool = $trafficTool;
        $this->num = $num;
        $this->array = $array;
    }

    public function visitTibet()
    {
        $this->trafficTool->go();
    }
}
$app = new Container();

// 绑定接口的实现
$app->bind('Visit', 'Car');

// 绑定具体的类名
//$app->bind('traveller', 'Traveller');

// 绑定匿名函数
$app->bind('traveller', function($c) {
    return $c->make('Traveller');
});

// 通过容器完成依赖注入
$tra = $app->make('traveller');
$tra->visitTibet();
```







1. 中间件是怎么被执行的
2. 在容器中绑定的单例对象，与自动注入的是同一个对象吗（构造方法、普通方法注入）？
    比如StartSession构造方法中自动注入的SessionManager与SessionProvider中绑定的对象是否一致。
3. 容器的绑定与解析之间的关系

别名：
    在自动注入时，容器可以反射出构造方法或setter方法的参数，根据参数的类型提示来自动的自动注入对象。
    别名就是为参数的类型起一个名字，当容器在进行自动注入时发现参数类型有对应的别名，它就会去寻找该别名的绑定实现，根据该别名的绑定实现来实例化对象。