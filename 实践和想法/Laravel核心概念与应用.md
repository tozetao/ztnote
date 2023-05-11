Laravel对服务对象，服务容器、服务提供者这3个概念的定义有些模糊，我们重新定义下。

- 服务对象：一个普通的对象，封装了一些功能的实现。比如一个Cache对象，它封装了缓存处理的相关逻辑。
- 服务容器：依赖注入容器
- 服务提供者：使用服务容器注册服务的地方。



### 服务容器

服务容器就是依赖注入容器，依赖注入是指类的依赖通过构造方法或者setter方法注入到类中。也就是说我们将对象的初始化过程交给了容器处理，当需要一个对象时去找容器取就可以了，我们不关系初始化对象的过程。



服务绑定：绑定一个服务对象的初始化过程。比如Laravel容器提供的简单绑定。

```php
$this->app->bind('HelpSpot\API', function($app) {
    return new HelpSpot\API($app->make('HttpClient'));
});
// 这是在服务提供者中绑定服务实例的代码
// Closure的$app参数是容器实例对象，在构造API对象时，我们通过容器出HttpClient对象，然后注入到构造方法中
```



服务解析：服务解析就是去服务容器中获取服务对象

```php
$obj = resolve('HelpSpot');
```

通过app辅助函数可以快速去容器中获取服务对象。



容器使用技巧：可以在控制器、队列任务、中间件、事件监听器等，通过类型提示在类的构造函数中注入依赖的服务对象。





### 服务提供者

服务提供者是配置应用程序的中心，比如服务对象的注册，事件监听器、中间件，甚至是路由的注册都可以在服务提供者这里进行配置。

- 注册服务提供者：在config/app.php文件中配置即可。

- 无论是register还是boot方法，都是在请求处理之前执行的。



> register

register是注册方法，只允许在该方法中注册服务对象，不允许在这里注册监听器、路由或者其他功能，因为注册的这些服务其所对应的服务提供者尚未被加载。



> boot

引导方法。boot方法会在所有服务提供者被注册之后才被调用，也就是所有服务提供者的register方法执行完毕后再来执行boot方法，所以在boot方法中可以访问已经注册过的服务。



### Facade

Facade为服务对象提供了一个静态接口。它其实就是一个代理类，对服务对象提供的服务做了一个静态方法的包装。

要实现一个Facade，需要将其与服务容器中已注册的服务对象的别名绑定起来，也就是实现getFacadeAccessor方法，比如Cache Facade：

```php
class Cache extends Facade
{
    /**
     * 获取组件的注册名称。
     *
     * @return string
     */
    protected static function getFacadeAccessor() { return 'cache'; }
}
```





### 授权

授权，字面意义上是指给与用户某个操作的权限。

授权分为俩步，先定义授权规则，授权规则指的是用户具有什么样的资质才会给与操作的权限。然后是授权操作（动作），如果用户满足该定义的授权规则，授权就成功，否则失败。

Laravel提供了Gate和Policy俩种授权方式。Gate提供了一种简单的基于闭包的授权方法，而Policy是围绕着模型或资源对授权动作进行分组的一种授权方式。



> 注册策略

为什么要注册策略？在使用策略去授权动作时，一般会传递一个类名或者参数，Laravel要根据这个类名和参数去选择对应的策略对象来执行授权动作。这也是在注册时，Key是资源类（模型类或其他类）的名字，而Value是策略类名字。

```php
namespace App\Providers;

use App\Post;
use App\Policies\PostPolicy;
use Illuminate\Support\Facades\Gate;
use Illuminate\Foundation\Support\Providers\AuthServiceProvider as ServiceProvider;

class AuthServiceProvider extends ServiceProvider
{
    /**
     * 应用的策略映射。key是资源类名，值是策略类名
     *
     * @var array
     */
    protected $policies = [
        Post::class => PostPolicy::class,
    ];

    /**
     * 注册任意应用认证、应用授权服务
     * @return void
     */
    public function boot()
    {
        $this->registerPolicies();
    }
}
```



> 策略自动发现

在授权动作时，通过什么方式去寻找一个策略。Laravel允许你提供寻找策略的实现。



> 对于访客

对于访客，Gate和Policy的授权默认都返回失败。





> 关于后台权限的控制

使用RBAC控制权限，后台的管理员表可以与前台的用户表共用，同时为后台所有路由划分一个组，访问这些路由必须拥有管理员权限，或者是管理员角色。