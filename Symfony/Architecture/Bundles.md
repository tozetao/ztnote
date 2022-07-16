警告：在4.0之前的Symfony版本中，建议使用bundles来组织你自己的应用程序代码。现在已经不推荐这样做了，bundles只应该用来在多个应用程序之间共享代码和功能。

bundle包类似于其他软件中的插件，但甚至更好。Symfony框架的核心功能是用bundle实现的（FrameworkBundle、SecurityBundle、DebugBundle等），它们还用于通过第三方捆绑包在应用程序中添加新功能。

在你的应用程序中使用的bundles必须在config/bundles.php文件中按环境启用。

```php
// config/bundles.php
return [
    // 'all' means that the bundle is enabled for any Symfony environment
    Symfony\Bundle\FrameworkBundle\FrameworkBundle::class => ['all' => true],
    Symfony\Bundle\SecurityBundle\SecurityBundle::class => ['all' => true],
    Symfony\Bundle\TwigBundle\TwigBundle::class => ['all' => true],
    Symfony\Bundle\MonologBundle\MonologBundle::class => ['all' => true],
    Doctrine\Bundle\DoctrineBundle\DoctrineBundle::class => ['all' => true],
    Sensio\Bundle\FrameworkExtraBundle\SensioFrameworkExtraBundle::class => ['all' => true],
    // this bundle is enabled only in 'dev' and 'test', so you can't use it in 'prod'
    Symfony\Bundle\WebProfilerBundle\WebProfilerBundle::class => ['dev' => true, 'test' => true],
];
```

提示：在一个使用Symfony Flex的默认Symfony应用程序中，bundles在安装/删除时自动为你启用/禁用，所以你不需要看或编辑这个bundles.php文件。



### Creating a Bundle

本节会创建并启用一个新的bundle，以显示只需要几个必要步骤。这个新的捆绑包被称为AcmeTestBundle，其中Acme部分是一个示例名称，应该用一些代表你或你的组织的 "供应商 "名称来代替（例如，ABCTestBundle代表一些名为ABC的公司）。

首先创建一个 src/Acme/TestBundle/目录，并添加一个名为 AcmeTestBundle.php 的新文件：

```php
// src/Acme/TestBundle/AcmeTestBundle.php
namespace App\Acme\TestBundle;

use Symfony\Component\HttpKernel\Bundle\Bundle;

class AcmeTestBundle extends Bundle
{
}
```

注：AcmeTestBundle这个名字遵循标准的Bundle命名惯例。你也可以选择通过将这个类命名为TestBundle（并将文件命名为TestBundle.php），将Bundle的名称缩短为简单的TestBundle。

这个空类是创建新bundle所需的唯一部分。虽然这个类通常是空的，但它功能强大，可以用于自定义bundle的行为。现在您已经创建了bundle，请启用它：

```php
// config/bundles.php
return [
    // ...
    App\Acme\TestBundle\AcmeTestBundle::class => ['all' => true],
];
```

虽然它还没有做任何事情，但AcmeTestBundle现在已经可以使用了。





### Bundle Directory Structure

bundle 的目录结构是为了帮助保持所有Symfony bundle 之间的代码一致。它遵循一套惯例，但在需要时可以灵活调整。

- Controller/

  包含bundle的控制器（例如RandomController.php）

- DependencyInjection/

  持有某些依赖注入扩展类，这些类可以导入服务配置，register compiler passes或其他（该目录不是必须的）。

- Resources/config/

  Houses配置，包括路由配置（比如routing.yaml）。

- Resources/views/

  持有按控制器名称组织的模板（例如，`Random/index.html.twig`）。

- Resources/public/

  包含网络资源（图片、样式表等），通过`assets:install`控制台命令复制或象征性地链接到项目`public/`目录。

- Tests/

  Holds all tests for the bundle.



### Learn more

- [How to Override any Part of a Bundle](https://symfony.com/doc/5.4/bundles/override.html)
- [Best Practices for Reusable Bundles](https://symfony.com/doc/5.4/bundles/best_practices.html)
- [How to Create Friendly Configuration for a Bundle](https://symfony.com/doc/5.4/bundles/configuration.html)
- [How to Load Service Configuration inside a Bundle](https://symfony.com/doc/5.4/bundles/extension.html)
- [How to Simplify Configuration of Multiple Bundles](https://symfony.com/doc/5.4/bundles/prepend_extension.html)



