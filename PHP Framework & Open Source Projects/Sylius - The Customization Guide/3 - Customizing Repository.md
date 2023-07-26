Warning: In **Sylius** we are using both default Doctrine repositories and the custom ones. Often you will be needing to add your very own methods to them. You need to check before which repository is your resource using.

在Sylius中，我们同时使用默认的Doctrine Repositories和自定义的Repositories。通常当你需要向它们添加你自己的方法。你需要事先检查你的资源是使用哪一个Repository。



## Why would you customize a Repository?

Different sets of different resources can be obtained in various scenarios in your application. You may need for instance:

在你的应用中的各种情况下，可以获得不同的资源集。例如，你可能需要

> - finding Orders by a Customer and a chosen Product
> - finding Products by a Taxon
> - finding Comments by a Customer



## How to customize a Repository?

Let’s assume that you would want to find products that you are running out of in the inventory.

让我们假设，你想在库存中找到你即将用完的产品。

**1.** Create your own repository class under the `AppBundle\Repository` namespace. Remember that it has to extend a proper base class. How can you check that?

在AppBundle/Repository命名空间下创建你自己的仓库类。记住，它必须扩展一个合适的基类。怎么检查呢？

For the `ProductRepository` run:

对于ProductRepository的运行：

```
$ php bin/console debug:container sylius.repository.product
```

As a result you will get the `Sylius\Bundle\CoreBundle\Doctrine\ORM\ProductRepository` - this is the class that you need to be extending.

结果你会得到Sylius\Bundle\CoreBundle\Doctrine\ORM\ProductRepository - 这是你需要扩展的类。

```php
<?php

namespace AppBundle\Repository;

use Sylius\Bundle\CoreBundle\Doctrine\ORM\ProductRepository as BaseProductRepository;

class ProductRepository extends BaseProductRepository
{
    /**
     * @param int $limit
     *
     * @return array
     */
    public function findByOnHand(int $limit = 8): array
    {
        return $this->createQueryBuilder('o')
            ->addSelect('variant')
            ->addSelect('translation')
            ->leftJoin('o.variants', 'variant')
            ->leftJoin('o.translations', 'translation')
            ->addOrderBy('variant.onHand', 'ASC')
            ->setMaxResults($limit)
            ->getQuery()
            ->getResult()
        ;
    }
}
```

We are using the [Query Builder](http://doctrine-orm.readthedocs.io/projects/doctrine-orm/en/latest/reference/query-builder.html) in the Repositories. As we are selecting Products we need to have a join to translations, because they are a translatable resource. Without it in the query results we wouldn’t have a name to be displayed.

我们正在使用Repositories中的Query Builder。由于我们正在选择产品，我们需要连接到translations（翻译），因为它们是可翻译的资源。如果在查询结果中没有它，我们就不会有要显示的名字。

We are sorting the results by the count of how many products are still available on hand, which is saved on the `onHand` field on the specific `variant` of each product. Then we are limiting the query to 8 by default, to get only 8 products that are low in stock.

我们通过手头还有多少产品的计数来对结果进行排序，这些计数被保存在每个产品的特定变体的onHand字段上。然后，我们将查询限制在默认的8个，以获得只有8个库存低的产品。

**2.** In order to use your repository you need to configure it in the `app/config/config.yml`.

2.为了使用你的资源库，你需要在app/config/config.yml中进行配置。

```yaml
sylius_product:
    resources:
        product:
            classes:
                repository: AppBundle\Repository\ProductRepository
```

**3.** After configuring the `sylius.repository.product` service has your `findByOnHand()` method available. You can form now on use your method in any **Controller**.

```php
<?php

public function lowInStockAction()
{
    $productRepository = $this->container->get('sylius.repository.product');

    $lowInStock = $productRepository->findByOnHand();
}
```



## What happens while overriding Repositories?

- The parameter `sylius.repository.product.class` contains `AppBundle\Repository\ProductRepository`.

  `sylius.repository.product.class`参数包含了`AppBundle\Repository\ProductRepository`

- The repository service `sylius.repository.product` is using your new class.

- Under the `sylius.repository.product` service you have got all methods from the base repository available plus the one you have added.

### Good to know

See also：All the customizations can be done either in your application directly or in [Plugins](https://docs.sylius.com/en/1.1/plugins/index.html)!