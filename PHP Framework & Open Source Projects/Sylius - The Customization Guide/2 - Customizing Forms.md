The forms in Sylius are placed in the `Sylius\Bundle\*BundleName*\Form\Type` namespaces and the extensions will be placed in AppBundleFormExtension.

Sylius中的表单被放置在Sylius\Bundle\BundleName\FormType命名空间中，扩展将被放置在AppBundleFormExtension中。



## Why would you customize a Form?

There are plenty of reasons to modify forms that have already been defined in Sylius. Your business needs may sometimes slightly differ from our internal assumptions.

有很多理由可以修改已经在Sylius中定义好的表单。你的业务需求有时可能与我们的内部假设略有不同。

You can:

- add completely **new fields**,

  添加全新的字段、

- **modify** existing fields, make them required, change their HTML class, change labels etc.,

  修改现有的字段，使其成为必填项，改变其HTML class，改变label等、

- **remove** fields that are not used.

  删除不使用的字段。



## How to customize a Form?

If you want to modify the form for the `Customer Profile` in your system there are a few steps that you should take. Assuming that you would like to (for example):

如果你想在你的系统中修改客户资料的表单，有几个步骤是你应该做的。假设你想（比如说）：

- Add a `contactHours` field,

  增加一个contactHours字段、

- Remove the `gender` field,

  删除性别字段、

- Change the label for the `lastName` from `sylius.form.customer.last_name` to `app.form.customer.surname`

  将lastName的标签（label）从sylius.form.customer.last_name改为app.form.customer.surname。

These will be the steps that you will have to take to achieve that:
你必须采用以下步骤来实现：

**1.** If you are planning to add new fields remember that beforehand they need to be added on the model that the form type is based on.

如果你打算添加新的字段，请记住，在此之前，它们需要被添加到表单类型所对应的模型上。

> In case of our example if you need to have the `contactHours` on the model and the entity mapping for the `Customer` resource. To get to know how to prepare that go [there](https://docs.sylius.com/en/1.1/customization/model.html).

在我们的例子中，如果你需要在模型上添加contactHours和Customer资源的实体映射。要知道如何准备，请看这里。

**2.** Create a **Form Extension**.

创建一个Form扩展。

Your form has to extend a proper base class. How can you check that?

你的表单必须扩展一个合适的基类。怎么检查呢？

For the `CustomerProfileType` run:

```
$ php bin/console debug:container sylius.form.type.customer_profile
```

As a result you will get the `Sylius\Bundle\CustomerBundle\Form\Type\CustomerProfileType` - this is the class that you need to be extending.

你将会得到Sylius\Bundle\CustomerBundle\Form\Type\CustomerProfileType结果，这个类就是你需要去继承的。

```php
<?php

namespace AppBundle\Form\Extension;

use Sylius\Bundle\CustomerBundle\Form\Type\CustomerProfileType;
use Symfony\Component\Form\AbstractTypeExtension;
use Symfony\Component\Form\Extension\Core\Type\TextType;
use Symfony\Component\Form\FormBuilderInterface;

final class CustomerProfileTypeExtension extends AbstractTypeExtension
{
    /**
     * {@inheritdoc}
     */
    public function buildForm(FormBuilderInterface $builder, array $options): void
    {
        // Adding new fields works just like in the parent form type.
        $builder->add('contactHours', TextType::class, [
            'required' => false,
            'label' => 'app.form.customer.contact_hours',
        ]);

        // To remove a field from a form simply call ->remove(`fieldName`).
        $builder->remove('gender');

        // You can change the label by adding again the same field with a changed `label` parameter.
        $builder->add('lastName', TextType::class, [
            'label' => 'app.form.customer.surname',
        ]);
    }

    /**
     * {@inheritdoc}
     */
    public function getExtendedType(): string
    {
        return CustomerProfileType::class;
    }
}
```

> Note: Of course remember that you need to define new labels for your fields in the `app\Resources\translations\messages.en.yml` for english contents of your messages.
>
> 当然你需要记住在app/Resources/translations/messages.en.yml中为你的信息的英文内容定义新的label。



**3.** After creating your class, register this extension as a service in the `app/config/services.yml`:

扩展类创建好后，需要将app/config/services.yaml其注册为服务。

```yaml
services:
    app.form.extension.type.customer_profile:
        class: AppBundle\Form\Extension\CustomerProfileTypeExtension
        tags:
            - { name: form.type_extension, extended_type: Sylius\Bundle\CustomerBundle\Form\Type\CustomerProfileType }
```

> Note: Of course remember that you need to render the new fields you have created, and remove the rendering of the fields that you have removed **in your views**.
>
> 注意事项：要记住，你需要在视图中创建要渲染的新字段，并删除你在视图中删除的不要的字段。
>

In our case you will need a new template: app/Resources/SyliusShopBundle/views/Account/profileUpdate.html.twig.

在这个例子中，我们需要一个新的模板文件：app/Resources/SyliusShopBundle/views/Account/profileUpdate.html.twig

In **Twig** for example you can render your modified form in such a way:

```twig
<div class="two fields">
    <div class="field">{{ form_row(form.birthday) }}</div>
    <div class="field">{{ form_row(form.contactHours) }}</div>
</div>
```



### Need more information?

> Warning：Some of the forms already have extensions in Sylius. Learn more about Extensions [here](http://symfony.com/doc/current/form/create_form_type_extension.html).
>
> 有些表单在Sylius中已经有了扩展。在这里了解更多关于扩展的信息。

For instance the `ProductVariant` admin form is defined under `Sylius/Bundle/ProductBundle/Form/Type/ProductVariantType.php` and later extended in `Sylius/Bundle/CoreBundle/Form/Extension/ProductVariantTypeExtension.php`. If you again extend the base type form like this:

例如，ProductVariant管理表单是在Sylius/Bundle/ProductBundle/Form/Type/ProductVariantType.php中定义的，之后继承自Sylius/Bundle/CoreBundle/Form/Extension/ProductVariantTypeExtension.php。如果你还想要再次继承基本表单类型，可以这样做：

```yaml
services:
    app.form.extension.type.product_variant:
        class: AppBundle\Form\Extension\ProductVariantTypeMyExtension
        tags:
            - { name: form.type_extension, extended_type: Sylius\Bundle\ProductBundle\Form\Type\ProductVariantType, priority: -5 }
```

your form extension will also be executed. Whether before or after the other extensions depends on priority tag set.

你的表单扩展也将被执行。是在其他扩展之前还是之后，取决于优先级标签的设置。

Having a look at the extensions and possible additionally defined event handlers can also be useful when form elements are embedded dynamically, as is done in the `ProductVariantTypeExtension` by the `CoreBundle`:

当表单元素被动态嵌入时，看一下扩展和可能额外定义的事件处理程序也很有用，就像CoreBundle的ProductVariantType扩展那样：

```php
<?php

...

final class ProductVariantTypeExtension extends AbstractTypeExtension
{
    /**
     * {@inheritdoc}
     */
    public function buildForm(FormBuilderInterface $builder, array $options): void
    {
        ...

        $builder->addEventListener(FormEvents::PRE_SET_DATA, function (FormEvent $event) {
            $productVariant = $event->getData();

            $event->getForm()->add('channelPricings', ChannelCollectionType::class, [
                'entry_type' => ChannelPricingType::class,
                'entry_options' => function (ChannelInterface $channel) use ($productVariant) {
                    return [
                        'channel' => $channel,
                        'product_variant' => $productVariant,
                        'required' => false,
                    ];
                },
                'label' => 'sylius.form.variant.price',
            ]);
        });
    }

    ...
}
```

The `channelPricings` get added on `FormEvents::PRE_SET_DATA`, so when you wish to remove or alter this form definition, you will also have to set up an event listener and then remove the field:

channelPricings在FormEvents::PRE_SET_DATA上被添加，所以当你想删除或改变这个表单定义时，你也必须设置一个事件监听器，然后删除该字段：

```php
<?php

...

final class ProductVariantTypeMyExtension extends AbstractTypeExtension
{
    ...

    public function buildForm(FormBuilderInterface $builder, array $options): void
    {
        ...

        $builder
            ->addEventListener(FormEvents::PRE_SET_DATA, function (FormEvent $event) {
                $event->getForm()->remove('channelPricings');
            })
            ->addEventSubscriber(new AddCodeFormSubscriber(NULL, ['label' => 'app.form.my_other_code_label']))
        ;

        ...

    }
}
```

### Adding constraints inside a form extension

> Warning: When adding your constraints dynamically from inside a form extension, be aware to add the correct validation groups.

Although it is advised to follow the [Validation Customization Guide](https://docs.sylius.com/en/1.1/customization/validation.html), it might happen that you want to define the form constraints from inside the form extension. They will not be used unless the correct validation group(s) has been added. The example below shows how to add the default sylius group to a constraint.

```php
<?php

...

final class CustomerProfileTypeExtension extends AbstractTypeExtension
{
    ...

    public function buildForm(FormBuilderInterface $builder, array $options): void
    {
        ...

        // Adding new fields works just like in the parent form type.
        $builder->add('contactHours', TextType::class, [
            'required' => false,
            'label' => 'app.form.customer.contact_hours',
            'constraints' => [
                new Range([
                    'min' => 8,
                    'max' => 17,
                    'groups' => ['sylius'],
                ]),
            ],
        ]);

        ...
    }

    ...
}
```

### Overriding forms completely

> Tip: If you need to create a new form type on top of an existing one - create this new alternative form type and define getParent() to the old one. [See details in the Symfony docs](http://symfony.com/doc/current/form/create_custom_field_type.html).



#### Good to know

See also

All the customizations can be done either in your application directly or in [Plugins](https://docs.sylius.com/en/1.1/plugins/index.html)!