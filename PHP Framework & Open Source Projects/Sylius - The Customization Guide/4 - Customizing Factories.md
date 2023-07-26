> Warning: Some factories may already be decorated in the **Sylius** Core. You need to check before decorating which factory (Component or Core) is your resource using.



## Why would you customize a Factory?

Differently configured versions of resources may be needed in various scenarios in your application. You may need for instance to:

在你的应用程序中的各种情况下，可能需要不同的Resources配置版本。例如，你可能需要：

- create a Product with a Supplier (which is your own custom entity)

  创建一个带有供应商的产品（这是您自己的自定义实体）。

- create a disabled Product (for further modifications)

  创建一个disabled 产品（用于进一步修改）。

- create a ProductReview with predefined description

  创建一个具有预定义描述的产品评论

and many, many more.

以及更多。





## How to customize a Factory?

Let’s assume that you would want to have a possibility to create disabled products.

让我们假设你希望有可能创建Disabled产品。

**1.** Create your own factory class in the `AppBundle\Factory` namespace. Remember that it has to implement a proper interface. How can you check that? For the `ProductFactory` run:

在`AppBundle/Factory`命名空间中创建你自己的工厂类。记住，它必须实现一个合适的接口。如何检查呢？比如对于 "ProductFactory "的检查，可以运行：

```
$ php bin/console debug:container sylius.factory.product
```

As a result you will get the `Sylius\Component\Product\Factory\ProductFactory` - this is the class that you need to decorate. Take its interface (`Sylius\Component\Product\Factory\ProductFactoryInterface`) and implement it.

结果你会得到`Sylius\Component\Product\Factory\ProductFactory`--这是你需要装饰的类。拿出它的接口（`Sylius\Component\ProductFactory\ProductFactoryInterface`）并实现它。

```php
<?php

namespace AppBundle\Factory;

use Sylius\Component\Product\Model\ProductInterface;
use Sylius\Component\Product\Factory\ProductFactoryInterface;

class ProductFactory implements ProductFactoryInterface
{
    /**
     * @var ProductFactoryInterface
     */
    private $decoratedFactory;

    /**
     * @param ProductFactoryInterface $factory
     */
    public function __construct(ProductFactoryInterface $factory)
    {
        $this->decoratedFactory = $factory;
    }

    /**
     * {@inheritdoc}
     */
    public function createNew(): ProductInterface
    {
        return $this->decoratedFactory->createNew();
    }

    /**
     * {@inheritdoc}
     */
    public function createWithVariant(): ProductInterface
    {
        return $this->decoratedFactory->createWithVariant();
    }

    /**
     * @return ProductInterface
     */
    public function createDisabled(): ProductInterface
    {
        /** @var ProductInterface $product */
        $product = $this->decoratedFactory->createWithVariant();

        $product->setEnabled(false);

        return $product;
    }
}
```

**2.** In order to decorate the base ProductFactory with your implementation you need to configure it as a decorating service in the `app/Resources/config/services.yml`.

为了用你的实现来装饰基本的ProductFactory，你需要在`app/Resources/config/services.yml`中把它配置为一个装饰服务。

```yaml
services:
    app.factory.product:
        class: AppBundle\Factory\ProductFactory
        decorates: sylius.factory.product
        arguments: ['@app.factory.product.inner']
        public: false
```

**3.** You can use the new method of the factory in routing.

你可以在路由中使用工厂的新方法。

After the `sylius.factory.product` has been decorated it has got the new `createDisabled()` method. To actually use it overwrite `sylius_admin_product_create_simple` route like below in `app/config/routing/admin/product.yml`

在对sylius.factory.product进行装饰后，它就有了新的createDisabled()方法。要实际使用它，请在app/config/routing/admin/product.yml中覆盖sylius_admin_product_create_simple路由，如下所示：

```yaml
# app/config/routing/admin/product.yml
sylius_admin_product_create_simple:
    path: /products/new/simple
    methods: [GET, POST]
    defaults:
        _controller: sylius.controller.product:createAction
        _sylius:
            section: admin
            factory:
                method: createDisabled # like here for example
            template: SyliusAdminBundle:Crud:create.html.twig
            redirect: sylius_admin_product_update
            vars:
                subheader: sylius.ui.manage_your_product_catalog
                templates:
                    form: SyliusAdminBundle:Product:_form.html.twig
                route:
                    name: sylius_admin_product_create_simple
```

Create a new yaml file located at `app/config/routing/admin.yml`, if it does not exist yet.

创建一个新的yaml文件，位于app/config/routing/admin.yml，如果它还不存在。

```yaml
# app/config/routing/admin.yml
app_admin_product:
    resource: 'admin/product.yml'
```

Remember to import the `app/config/routing/admin.yml` into the `app/config/routing.yml`.

记得在app/config/routing.yml文件中导入/app/config/routing/admin.yml文件。

```yaml
# app/config/routing.yml
app_admin:
    resource: 'routing/admin.yml'
    prefix: /admin
```

### Good to know

See also：All the customizations can be done either in your application directly or in [Plugins](https://docs.sylius.com/en/1.1/plugins/index.html)!