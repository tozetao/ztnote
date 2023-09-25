基础的应用程序可以在一个配置文件中定义所有的路由--通常是config/routes.yaml（see [Routing](https://symfony.com/doc/5.4/routing.html#routing-creating-routes)）。然而，在大多数应用程序中，从不同的资源导入路由定义是很常见的：在控制器文件中的PHP注解、YAML、存储在某个目录中的XML或PHP文件等。



### Built-in Route Loaders

Symfony为最常见的需求提供了几个路由加载器：

```yaml
# config/routes.yaml
app_file:
    # loads routes from the given routing file stored in some bundle
	# 从存储在某个bundle中的给定routing file加载路由
    resource: '@AcmeBundle/Resources/config/routing.yaml'

app_annotations:
    # loads routes from the PHP annotations of the controllers found in that directory
    # 通过从目录中找到的控制器的PHP注解来加载路由
    resource: '../src/Controller/'
    type:     annotation

app_directory:
    # loads routes from the YAML, XML or PHP files found in that directory
    resource: '../legacy/routing/'
    type:     directory

app_bundle:
    # loads routes from the YAML, XML or PHP files found in some bundle directory
    resource: '@AcmeOtherBundle/Resources/config/routing/'
    type:     directory
```

提示：当导入资源时，key（例如app_file）是集合名。只要确保每个文件都是唯一的，这样就不会有其他行覆盖它。

如果你的应用需求不同，你可以创建你自己的自定义路由加载器，在下一节解释。



### What is a Custom Route Loader

自定义路由加载器使你能够根据一些惯例、模式或集成来生成路由。这个用例是[OpenAPI-Symfony-Routing](https://github.com/Tobion/OpenAPI-Symfony-Routing)库，其中路由是基于OpenAPI/Swagger注释生成的。另一个例子是 [SonataAdminBundle](https://github.com/sonata-project/SonataAdminBundle)，它根据CRUD惯例来创建路由。



### Loading Routes

Symfony应用程序中的路由是由 [DelegatingLoader](https://github.com/symfony/symfony/blob/5.4/src/Symfony/Bundle/FrameworkBundle/Routing/DelegatingLoader.php)加载的。这个加载器使用其他几个加载器(委托)来加载不同类型的资源，例如YAML文件或控制器文件中的@Route注解。特定的加载器需要实现LoaderInterface，该接口有两个重要的方法:supports()和load()。

我们看下routes.yaml配置文件中的这几行配置：

```yaml
# config/routes.yaml
controllers:
    resource: ../src/Controller/
    type: annotation
```

当主加载器解析时，它会尝试所有注册的委托加载器，并以给定的资源（.../src/Controller/）和类型（注解）为参数，调用它们的support()方法。当其中一个加载器返回true时，它的load()方法将被调用，它应该返回一个包含Route对象的RouteCollection。

提示：以这种方式加载的路由将被Router缓存，与以默认格式之一（如XML、YAML、PHP文件）定义的路由相同。



### Loading Routes with a Custom Service

使用常规的Symfony服务是自定义加载路由的最简单方法，这比创建一个自定义的路由加载器要容易的多，所以你应该总是先考虑使用这种方式。

要做到这一点，请将type: service定义为加载路由资源的类型，并配置要调用的服务和方法：

```yaml
# config/routes.yaml
admin_routes:
    resource: 'admin_route_loader::loadRoutes'
    type: service
```

在这个例子中，路由是通过调用ID为admin_route_loader的服务的loadRoutes()方法加载的。你的服务不需要集成或实现任何特殊的类，但被调用的方法必须返回一个RouteCollection对象。

如果你使用的是 [autoconfigure](https://symfony.com/doc/5.4/service_container.html#services-autoconfigure),，你的类应该实现RouteLoaderInterface接口，以便被自动标记。如果你不使用自动配置，就用routing.route_loader来手动标记它。

提示：使用服务路由加载器定义的路由将被框架自动缓存。因此，每当你的服务需要加载新的路由时，不要忘记清除缓存。

注：如果你的服务是可调用的，你不需要精确的方法来使用。If your service is invokable, you don't need to precise the method to use.



### Creating a custom Loader

要从一些自定义的来源（即从注解、YAML或XML文件以外的东西）加载路由，你需要创建一个自定义路由加载器。这个加载器必须要实现LoaderInterface。

在大多数情况下，从Loader集成而不是自己实现LoaderInterface更容易。

下面的示例加载器支持加载额外类型的路由资源。这个类型的名字不应该与其他可能支持相同类型资源的加载器冲突。你可以根据你所做的事情来制定任何名称。资源名称本身在这个例子中并没有实际使用。

```php
// src/Routing/ExtraLoader.php
namespace App\Routing;

use Symfony\Component\Config\Loader\Loader;
use Symfony\Component\Routing\Route;
use Symfony\Component\Routing\RouteCollection;

class ExtraLoader extends Loader
{
    private $isLoaded = false;

    public function load($resource, string $type = null)
    {
        if (true === $this->isLoaded) {
            throw new \RuntimeException('Do not add the "extra" loader twice');
        }

        $routes = new RouteCollection();

        // prepare a new route
        $path = '/extra/{parameter}';
        $defaults = [
            '_controller' => 'App\Controller\ExtraController::extra',
        ];
        $requirements = [
            'parameter' => '\d+',
        ];
        $route = new Route($path, $defaults, $requirements);

        // add the new route to the route collection
        $routeName = 'extraRoute';
        $routes->add($routeName, $route);

        $this->isLoaded = true;

        return $routes;
    }

    public function supports($resource, string $type = null)
    {
        return 'extra' === $type;
    }
}
```

请确保你指定的控制器真的存在。在这种情况下，你必须在ExtraController中创建一个extra()方法。

```php
// src/Controller/ExtraController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Response;

class ExtraController extends AbstractController
{
    public function extra($parameter)
    {
        return new Response($parameter);
    }
}
```

现在为ExtraLoader定义一个service：

```yaml
# config/services.yaml
services:
    # ...
    App\Routing\ExtraLoader:
        tags: [routing.loader]
```

注意标签routing.loader。所有带有这个标签的服务都将被标记为潜在的路由加载器，并作为专门的路由加载器添加到routing.loader服务中，后者是DelegatingLoader的一个实例。



#### Using the Custom Loader

如果你不做别的，你的自定义路由加载器就不会被调用。剩下要做的就是在路由配置中添加几行。

```yaml
# config/routes.yaml
app_extra:
    resource: .
    type: extra
```

这里重要的部分是type key。它的值应该是 extra，因为这是 ExtraLoader 支持的类型，这将确保其 load() 方法被调用。resource key对ExtraLoader来说是不重要的，所以它被设置为.（一个点）。

提示：使用自定义路由加载器定义的路由将被框架自动缓存。因此，每当你改变加载器类本身的内容时，不要忘记清除缓存。



### More Advanced Loaders

如果你的自定义路由加载器如上所示继承自[Loader](https://github.com/symfony/symfony/blob/5.4/src/Symfony/Component/Config/Loader/Loader.php)，你也可以利用提供的解析器（LoaderResolver的实例）来加载二级路由资源。

你仍然需要实现support()和load()。每当你想加载另一个资源 - 例如YAML路由配置文件 - 你可以调用import()方法：

```php
// src/Routing/AdvancedLoader.php
namespace App\Routing;

use Symfony\Component\Config\Loader\Loader;
use Symfony\Component\Routing\RouteCollection;

class AdvancedLoader extends Loader
{
    public function load($resource, string $type = null)
    {
        $routes = new RouteCollection();

        $resource = '@ThirdPartyBundle/Resources/config/routes.yaml';
        $type = 'yaml';

        $importedRoutes = $this->import($resource, $type);

        $routes->addCollection($importedRoutes);

        return $routes;
    }

    public function supports($resource, string $type = null)
    {
        return 'advanced_extra' === $type;
    }
}
```

注1：导入的路由配置的资源名称和类型通常可以是任何由路由配置加载器支持的东西（YAML、XML、PHP、注释等）。

注2：对于更高级的使用，可以看看Symfony CMF项目提供的[ChainRouter](https://symfony.com/doc/current/cmf/components/routing/chain.html)。这个路由器允许应用程序结合使用两个或更多的路由器，例如在编写一个自定义路由器时，继续使用默认的Symfony路由系统。

