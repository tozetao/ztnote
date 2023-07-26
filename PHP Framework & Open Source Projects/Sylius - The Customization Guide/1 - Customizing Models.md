All models in Sylius are placed in the `Sylius\Component\*ComponentName*\Model` namespaces alongside with their interfaces.

Sylius中的所有模型都与它们的接口一起放在Sylius/Component/ComponentName/Model命名空间中。

> Warning: Many models in Sylius are **extended in the Core component**. If the model you are willing to override exists in the `Core` you should be extending the `Core` one, not the base model from the component.
>
> Sylius中的许多模型是在Core组件中扩展的。如果你想要覆盖的模型存在于Core组件中，你应该扩展Core组件，而不是组件中的基本模型。

> Note: Note that there are **translatable models** in Sylius also. The guide to translatable entities can be found below the regular one.
>
> 注意：请注意，Sylius中也有可翻译的模型。可翻译实体的指南可以在常规指南下面找到。



## Why would you customize a Model?

To give you an idea of some purposes of models customizing have a look at a few examples:

为了让你了解模型定制的一些目的，请看几个例子：

- Add `flag` field to the `Country`

  在Country中添加flag字段

- Add `secondNumber` to the `Customer`

- Change the `reviewSubject` of a `Review` (in Sylius we have `ProductReviews` but you can imagine for instance a `CustomerReview`)

  改变评论（Review）的主题（reviewSubject），在Sylius中我们有产品评论（ProductReivews），但是你可以想象一个实例，比如客户评论（CustomerReview）

- Add `icon` to the `PaymentMethod`

  为支付方式（PaymentMethod）添加一个icon

And of course many similar operations limited only by your imagination. Let’s now see how you should perform such customizations.
当然还有许多类似的操作，只受限于你的想象力。现在让我们看看你应该如何执行这样的自定义操作。



## How to customize a Model?

Let’s take the `Sylius\Component\Addressing\Country` as an example. This one is not extended in Core. How can you check that?

让我们以Sylius/Component/Addressing/Country为例。这个Model没有继承自Core组件，如何检查一个Model是否继承自Core组件呢？

For the `Country` run:

比如Country模型，可以运行：

```
$ php bin/console debug:container --parameter=sylius.model.country.class
```

As a result you will get the `Sylius\Component\Addressing\Model\Country` - this is the class that you need to be extending.

结果你会得到Sylius\Component\Addressing\Model\Country - 这是你需要扩展的类。

Assuming that you would want to add another field on the model - for instance a `flag`.

假设你想在模型上添加另一个字段--比如说一个flag字段。

**1.** The first thing to do is to write your own class which will extend the base `Country` class.

首先要做的是编写你自己的类，它将继承基础Country类。

```php
<?php

namespace AppBundle\Entity;

use Sylius\Component\Addressing\Model\Country as BaseCountry;

class Country extends BaseCountry
{
    /**
     * @var bool
     */
    private $flag;

    /**
     * @return bool|null
     */
    public function getFlag(): ?bool
    {
        return $this->flag;
    }

    /**
     * @param bool $flag
     */
    public function setFlag(bool $flag): void
    {
        $this->flag = $flag;
    }
}
```

**2.** Next define your entity’s mapping.

下一步是定义实体映射。

The file should be placed in `AppBundle/Resources/config/doctrine/Country.orm.yml`

该文件应该位于`AppBundle/Resources/config/doctrine/Country.orm.yml`

```yaml
AppBundle\Entity\Country:
    type: entity
    table: sylius_country
    fields:
        flag:
            type: boolean
            nullable: true
```

**3.** Finally you’ll need to override the model’s class in the `app/config/config.yml`.

最后你需要在app/config/config.yml中覆盖模型类。

Under the `sylius_*` where `*` is the name of the bundle of the model you are customizing, in our case it will be the `SyliusAddressingBundle` -> `sylius_addressing`.

在sylius_\*下，其中*是你要定制的模型的包的名称，在我们的例子中，它将是SyliusAddressingBundle -> sylius_addressing。

```yaml
sylius_addressing:
    resources:
        country:
            classes:
                model: AppBundle\Entity\Country
```

**4.** Update the database. There are two ways to do it.

- via direct database schema update:

```
$ php bin/console doctrine:schema:update --force
```

- via migrations:

Which we strongly recommend over updating the schema.

```
$ php bin/console doctrine:migrations:diff
$ php bin/console doctrine:migrations:migrate
```

> Tip: Read more about the database modifications and migrations in the [Symfony documentation here](http://symfony.com/doc/current/book/doctrine.html#creating-the-database-tables-schema).

**5.** Additionally if you want to give the administrator an ability to add the `flag` to any of countries, you’ll need to update its form type. Check how to do it [here](https://docs.sylius.com/en/1.1/customization/form.html).

此外，如果你想让管理员有能力将flag添加到任何一个国家，你需要更新其表单类型（Form Type）。在这里查看如何做。



### What happens while overriding Models?

- Parameter `sylius.model.country.class` contains `AppBundle\Entity\Country`.

  参数sylius.model.country.class包含AppBundle/Entity/Country。

- `sylius.repository.country` represents Doctrine repository for your new class.

  sylius.repository.country代表你的新类的Doctrine资源库。

- `sylius.manager.country` represents Doctrine object manager for your new class.

  sylius.manager.country代表你的新类的Doctrine对象管理器。

- `sylius.controller.country` represents the controller for your new class.

  sylius.controller.country代表你新类的控制器。

- All Doctrine relations to `Sylius\Component\Addressing\Model\Country` are using your new class as *target-entity*, you do not need to update any mappings.

  所有与SyliusComponent/Addressing/Model/Country的Doctrine关系都使用你的新类作为目标实体，你不需要更新任何映射。

- `CountryType` form type is using your model as `data_class`.

  CountryType表单类型使用你的模型作为data_class。

- `Sylius\Component\Addressing\Model\Country` is automatically turned into Doctrine Mapped Superclass.
  Sylius/Component/Addressing/Model/Country会自动变成Doctrine Mapped Superclass。



## How to customize a translatable Model?

One of translatable entities in Sylius is the Shipping Method. Let’s try to extend it with a new field. Shipping methods may have different delivery time, let’s save it on the `estimatedDeliveryTime` field.

Just like for regular models you can also check the class of translatable models like that:

```
$ php bin/console debug:container --parameter=sylius.model.shipping_method.class
```

**1.** The first thing to do is to write your own class which will extend the base `ShippingMethod` class.

```php
<?php

namespace AppBundle\Entity;

use Sylius\Component\Core\Model\ShippingMethod as BaseShippingMethod;
use Sylius\Component\Shipping\Model\ShippingMethodTranslation;

class ShippingMethod extends BaseShippingMethod
{
    /**
     * @var string
     */
    private $estimatedDeliveryTime;

    /**
     * @return string
     */
    public function getEstimatedDeliveryTime(): string
    {
        return $this->estimatedDeliveryTime;
    }

    /**
     * @param string $estimatedDeliveryTime
     */
    public function setEstimatedDeliveryTime(string $estimatedDeliveryTime): void
    {
        $this->estimatedDeliveryTime = $estimatedDeliveryTime;
    }

    /**
     * {@inheritdoc}
     */
    protected function createTranslation(): ShippingMethodTranslation
    {
        return new ShippingMethodTranslation();
    }
}
```

> Note: Remember to set the translation class properly, just like above in the `createTranslation()` method.



**2.** Next define your entity’s mapping.

The file should be placed in `AppBundle/Resources/config/doctrine/ShippingMethod.orm.yml`

```yaml
AppBundle\Entity\ShippingMethod:
    type: entity
    table: sylius_shipping_method
    fields:
        estimatedDeliveryTime:
            type: string
            nullable: true
```

**3.** Finally you’ll need to override the model’s class in the `app/config/config.yml`.

Under the `sylius_*` where `*` is the name of the bundle of the model you are customizing, in our case it will be the `SyliusShippingBundle` -> `sylius_shipping`.

```yaml
sylius_shipping:
    resources:
        shipping_method:
            classes:
                model: AppBundle\Entity\ShippingMethod
```

**4.** Update the database. There are two ways to do it.

- via direct database schema update:

```
$ php bin/console doctrine:schema:update --force
```

- via migrations:

Which we strongly recommend over updating the schema.

```
$ php bin/console doctrine:migrations:diff
$ php bin/console doctrine:migrations:migrate
```

> Tip: Read more about the database modifications and migrations in the [Symfony documentation here](http://symfony.com/doc/current/book/doctrine.html#creating-the-database-tables-schema).

**5.** Additionally if you need to add the `estimatedDeliveryTime` to any of your shipping methods in the admin panel, you’ll need to update its form type. Check how to do it [here](https://docs.sylius.com/en/1.1/customization/form.html).

> Warning: If you want the new field of your entity to be translatable, you need to extend the Translation class of your entity. In case of the ShippingMethod it would be the `Sylius\Component\Shipping\Model\ShippingMethodTranslation`. Also the form on which you will add the new field should be the TranslationType.



## How to customize translatable fields of a translatable Model?

Suppose you want to add a translatable property to a translatable entity, for example to the Shipping Method. Let’s assume that you would like the Shipping method to include a message with the delivery conditions. Let’s save it on the `deliveryConditions` field.

Just like for regular models you can also check the class of translatable models like that:

```
$ php bin/console debug:container --parameter=sylius.model.shipping_method_translation.class
```

**1.** In order to add a translatable property to your entity you need to define it on the `AppBundle\Entity\ShippingMethodTranslation` class of your bundle, that will extend the base `Sylius\Component\Shipping\Model\ShippingMethodTranslation`.

```php
<?php

namespace AppBundle\Entity;

use Sylius\Component\Shipping\Model\ShippingMethodTranslation as BaseShippingMethodTranslation;

class ShippingMethodTranslation extends BaseShippingMethodTranslation
{
    /**
     * @var string
     */
    private $deliveryConditions;

    /**
     * @return string
     */
    public function getDeliveryConditions(): string
    {
        return $this->deliveryConditions;
    }

    /**
     * @param string $deliveryConditions
     */
    public function setDeliveryConditions(string $deliveryConditions): void
    {
        $this->deliveryConditions = $deliveryConditions;
    }
}
```

**2.** Next define your translation entity’s mapping.

The translation’s entity file should be placed in `AppBundle/Resources/config/doctrine/ShippingMethodTranslation.orm.yml`

```php
AppBundle\Entity\ShippingMethodTranslation:
    type: entity
    table: sylius_shipping_method_translation
    fields:
        deliveryConditions:
            type: string
            nullable: true
```

**3.** You’ll need to provide access to the new fields in the `ShippingMethod` class by extending the base ShippingMethod class.

```php
<?php

namespace AppBundle\Entity;

use Sylius\Component\Core\Model\ShippingMethod as BaseShippingMethod;

class ShippingMethod extends BaseShippingMethod
{
   /**
     * @return string
     */
    public function getDeliveryConditions(): string
    {
        return $this->getTranslation()->getDeliveryConditions();
    }

   /**
     * @param string $deliveryConditions
     */
    public function setDeliveryConditions(string $deliveryConditions): void
    {
        $this->getTranslation()->setDeliveryConditions($deliveryConditions);
    }
}
```

> Note: Remember that if the original entity is not translatable you will need to initialize the translations collection in the constructor, and use the TranslatableTrait. Take a careful look at the Sylius translatable entities.

**4.** As we are overriding not only the translation class but also the base class, we need to create an emty mapping also for this base class.

The mapping file should be placed in `AppBundle/Resources/config/doctrine/ShippingMethod.orm.yml`

```yaml
AppBundle\Entity\ShippingMethod:
    type: entity
    table: sylius_shipping_method
```

**5.** Finally you’ll need to override the model’s classes in the `app/config/config.yml`.

Under the `sylius_*` where `*` is the name of the bundle of the model you are customizing, in our case it will be the `SyliusShippingBundle` -> `sylius_shipping`.

```yaml
sylius_shipping:
    resources:
        shipping_method:
            classes:
                model: AppBundle\Entity\ShippingMethod
            translation:
                classes:
                    model: AppBundle\Entity\ShippingMethodTranslation
```

**6.** Update the database. There are two ways to do it.

- via direct database schema update:

```
$ php bin/console doctrine:schema:update --force
```

- via migrations:

Which we strongly recommend over updating the schema.

```
$ php bin/console doctrine:migrations:diff
$ php bin/console doctrine:migrations:migrate
```

> Tip: Read more about the database modifications and migrations in the [Symfony documentation here](http://symfony.com/doc/current/book/doctrine.html#creating-the-database-tables-schema).



**6.** Additionally if you need to add the `deliveryConditions` to any of your shipping methods in the admin panel, you’ll need to update its form type. Check how to do it [here](https://docs.sylius.com/en/1.1/customization/form.html).