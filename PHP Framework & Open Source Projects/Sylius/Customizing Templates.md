

> Note: There are two kinds of templates in Sylius. **Shop** and **Admin** ones, plus you can create your own to satisfy your needs.
>
> 注意：在Sylius中，有两种模板。一个是Shop模板，一个是Admin模板。你还可以创建自己的模板来满足你的需求。



## Why would you customize a template?

The most important case for modifying the existing templates is of course **integrating your own layout of the system**. Sometimes even if you have decided to stay with the default layout provided by Sylius, you need to **slightly modify it to meet your business requirements**. You may just need to **add your logo anywhere**.

修改现有模板的最重要的情况当然是整合你自己的系统布局。有时，即使你已经决定继续使用Sylius提供的默认布局，你也需要轻微地修改它以满足你的业务需求。比如你可能需要在任何地方添加你的Logo。



## Methods of templates customizing

Warning: There are three ways of customizing templates of Sylius:

警告： 有三种方法可以定制Sylius的模板：

The first one is simple **templates overriding** inside of the `templates/bundles` directory of your project. Using this method you can completely change the content of templates.

第一种是在项目的`templates/bundles`目录下进行简单的模板覆盖。使用这种方法，你可以完全改变模板的内容。

The second method is **templates customization via events**. You are able to listen on these template events, and by that add your own blocks without copying and pasting the whole templates. This feature is really useful when [creating Sylius Plugins](https://docs.sylius.com/en/1.12/book/plugins/creating-plugin.html).

第二种方法是通过事件定制模板。你能够监听这些模板事件，并通过这些事件添加你自己的block，而无需复制和粘贴整个模板。这个功能在[创建Sylius插件]（https://docs.sylius.com/en/1.12/book/plugins/creating-plugin.html）时非常有用。

The third method is **using Sylius themes**. Creating a Sylius theme requires a few more steps than basic template overriding, but allows you to have a different design on multiple channels of the same Sylius instance. [Learn more about themes here](https://docs.sylius.com/en/1.12/book/themes/themes.html).

第三种方法是**使用Sylius主题**。创建一个Sylius主题需要比基本的模板覆盖多一些步骤，但允许你在同一个Sylius实例的多个频道上有不同的设计。[在这里了解更多关于主题的信息](https://docs.sylius.com/en/1.12/book/themes/themes.html)。

Tip: You can browse the full implementation of these examples on [this GitHub Pull Request.](https://github.com/Sylius/Customizations/pull/16)

提示：你可以在[这个GitHub Pull Request.](https://github.com/Sylius/Customizations/pull/16)上浏览这些例子的完整实现。



## How to customize templates by overriding?

> Note: How do you know which template you should be overriding? Go to the page that you are going to modify, at the bottom in the Symfony toolbar click on the route, which will redirect you to the profiler. In the Request Attributes section under `_sylius [ template => ...]` you can check the path to the current template.
>
> 注意：你怎么知道你应该覆盖哪个模板？进入你要修改的页面，在Symfony工具栏的底部点击路由，这将使你重定向到分析器。在"_sylius [ template => ...]"下的请求属性部分，你可以看到当前模板的路径。

- **Shop** templates: customizing Login Page template:

  Shop模板：定制登录页面模板。

The default login template is: `@SyliusShopBundle/login.html.twig`. In order to override it you need to create your own: `templates/bundles/SyliusShopBundle/login.html.twig`.

默认的登录模板是：`@SyliusShopBundle/login.html.twig`。为了覆盖它，你需要创建你自己的模板： `templates/bundles/SyliusShopBundle/login.html.twig`。

Copy the contents of the original template to make your work easier. And then modify it to your needs.

复制原始模板的内容，使你的工作更容易。然后根据你的需要修改它。

```twig
{% extends '@SyliusShop/layout.html.twig' %}

{% import '@SyliusUi/Macro/messages.html.twig' as messages %}

{% block content %}
<div class="ui column stackable center page grid">
    {% if last_error %}
        {{ messages.error(last_error.messageKey|trans(last_error.messageData, 'security')) }}
    {% endif %}

    {# You can add a headline for instance to see if you are changing things in the correct place. #}
    <h1>
        This Is My Headline
    </h1>

    <div class="five wide column"></div>
    <form class="ui six wide column form segment" action="{{ path('sylius_shop_login_check') }}" method="post" novalidate>
        <div class="one field">
            {{ form_row(form._username, {'value': last_username|default('')}) }}
        </div>
        <div class="one field">
            {{ form_row(form._password) }}
        </div>
        <div class="one field">
            <button type="submit" class="ui fluid large primary submit button">{{ 'sylius.ui.login_button'|trans }}</button>
        </div>
    </form>
</div>
{% endblock %}
```

Done! If you do not see any changes on the `/shop/login` url, clear your cache:

如果在shop/login URL上看不到任何改变，请清空缓存：

```
php bin/console cache:clear
```



- **Admin** templates: Customization of the Country form view.

  Admin模板：订制Country表单视图。

The default template for the Country form is: `SyliusAdminBundle:Country:_form.html.twig`. In order to override it you need to create your own: `templates/bundles/SyliusAdminBundle/Country/_form.html.twig`.

Country表单的默认模板是：`SyliusAdminBundle:Country:_form.html.twig`。为了覆盖它，你需要创建你自己的模板： `templates/bundles/SyliusAdminBundle/Country/_form.html.twig`。

Copy the contents of the original template to make your work easier. And then modify it to your needs.

复制原始模板的内容，然后根据你的需要修改它。

```twig
<div class="ui segment">
    {{ form_errors(form) }}
    {{ form_row(form.code) }}
    {{ form_row(form.enabled) }}
</div>
<div class="ui segment">

    {# You can add a headline for instance to see if you are changing things in the correct place. #}
    <h1>My Custom Headline</h1>

    <h4 class="ui dividing header">{{ 'sylius.ui.provinces'|trans }}</h4>
    {{ form_row(form.provinces, {'label': false}) }}
</div>
```

Done! If you do not see any changes on the `/admin/countries/new` url, clear your cache:

```
php bin/console cache:clear
```



## How to customize templates via events?

Sylius uses its own event mechanism called Sylius Template Events which implementation is based purely on Twig. This (compared to the legacy way of using SonataBlockBundle) leads to:

Sylius使用自己的事件机制，称为Sylius Template Events，其实现完全基于Twig。这（与使用SonataBlockBundle的传统方式相比）导致：

- better performance - as it is no longer based on EventListeners

  更好的性能--因为它不再基于EventListeners了

- less boilerplate code - no need to register more Listeners

  更少的模板代码--不需要注册更多的监听器

- easier variable pass - now you just need to add it to configuration file

  更简单的变量传递--现在你只需要将其添加到配置文件中

- extended configuration - now you can change if block is enabled, change its template, or even priority

  扩展配置--现在你可以改变块是否被启用，改变它的模板，甚至是优先级。

> Note：If you want to read more about the Sylius Template Events from developers/architectural perspective check the [Github Issue](https://github.com/Sylius/Sylius/issues/10997) referring this feature.
>
> 注意：如果你想从开发者/架构的角度阅读更多关于Sylius模板事件的信息，请查看提及该功能的[Github Issue](https://github.com/Sylius/Sylius/issues/10997)。
>
> We will now guide you through a simple way of customizing your template with Sylius Template Events.
>
> 我们现在将引导你通过一个简单的方法，用Sylius Template Events来定制你的模板。



### How to locate template events?

The events naming convention uses the routing to the place where we are adding it, but instead of `_` we are using `.`, followed by a slot name (like `sylius_admin_customer_show` route results in the `sylius.admin.customer.show.slot_name` events). The slot name describes where exactly in the template’s structure should the event occur, it will be `before` or `after` certain elements.

事件命名约定：使用我们要添加事件所在的地方的路由名，但我们使用`.`，而不是`_`，后面是slot名（如`sylius_admin_customer_show`路由会有对应的`sylius.admin.customer.show.slot_name`事件）。slot名描述了事件发生在模板结构中的确切位置，它某些元素之前或者之后。

Although when the resource name is not just one word (like `product_variant`) then the underscore stays in the event prefix string. Then `sylius_admin_product_variant_create` route will have the `sylius.admin.product_variant.create.slot_name` events.

当资源名（resource name）不只是一个单词时（如`product_variant`），那么下划线将保留在事件前缀字符串中。那么`sylius_admin_product_variant_create`路由将有`sylius.admin.product_variant.create.slot_name`事件。

Let’s see how the event is rendered in a default Sylius Admin template. This is the rendering of the event that occurs on the create action of Resources, at the bottom of the page (after the content of the create form):

让我们看看该事件在默认的Sylius Admin模板中是如何渲染的。这是发生在创建资源动作（create action of Resources）上的事件渲染，在页面的底部（创建表单的内容之后）：

```twig
{# First we are setting the event_prefix based on route as it was mentioned before #}
{# 首先，我们要根据前面提到的路由来设置事件前缀。 #}
{% set event_prefix = metadata.applicationName ~ '.admin.' ~ metadata.name ~ '.create' %}

{# And then the slot name is appended to the event_prefix #}
{# 然后slot name会被附加到event_prefix中 #}。
{{ sylius_template_event([event_prefix, 'sylius.admin.create'], _context) }}
```



> Note：Besides the events that are named based on routing, Sylius also has some other general events: those that will appear on every Sylius admin or shop. Examples: `sylius.shop.layout.slot_name` or `sylius.admin.layout.slot_name`. They are rendered in the `layout.html.twig` views for both Admin and Shop.
>
> 注：除了根据路由命名的事件外，Sylius还有一些其他的一般事件：那些会出现在每个Sylius管理员或商店的事件。例子： `sylius.shop.layout.slot_name`或`sylius.admin.layout.slot_name`。它们会在Admin和Shop的`layout.html.twig`视图中渲染。

> Tip：In order to find events in Sylius templates you can simply search for the `sylius_template_event` phrase in your project’s directory.
>
> 提示：为了在Sylius模板中找到事件，你可以简单地在你的项目目录中搜索`sylius_template_event`缩写。



### How to locate rendered template event?

#### With DevTools in your browser

If you want to search easier for the event name you want to modify, the Sylius Template Events can be easily found in your browser with the debug tools it provides. Just use the `explore` (in Chrome browser) or its equivalent in other browsers to check the HTML code of your webpage. Here you will be able to see commented blocks where the name of the template as well as the event name will be shown:

如果你想更容易地搜索你想修改的事件名称，Sylius模板事件可以通过它提供的调试工具在你的浏览器中轻松找到。只要使用 "explore"（在Chrome浏览器中）或其他浏览器中的类似工具，就可以检查你的网页的HTML代码。在这里，你将能够看到注释块，其中将显示模板的名称以及事件名称：

![./sylius_event_debug.png](D:\study\ztnote\PHP Framework & Open Source Projects\Sylius\sylius_event_debug.png)

In the example above we were looking for the HTML responsible for rendering of the Sylius Logo. Mentioned markup is surrounded by statements of where the event, as well as block, started. What is more, we can see which twig template is responsible for rendering this block and what the priority of this rendering is.

在上面的例子中，我们正在寻找负责渲染Sylius Logo的HTML。提到的Logo被event以及block的begin声明语句所包裹。此外，我们可以看到哪个twig模板负责渲染这个块，以及渲染的优先级是什么。

![./sylius_logo_locate.png](D:\study\ztnote\PHP Framework & Open Source Projects\Sylius\sylius_logo_locate.png)

This will have all the necessary information that you need for further customization.

这将有你进一步定制所需的所有必要信息。



#### With Symfony Profiler

The `Template events` section in Symfony Profiler gives you the list of events used to render the page with their blocks. Besides all information about blocks mentioned in the above section, you will see one more especially beneficial when it comes to optimization which is `Duration`.

Symfony Profiler中的 "Template evetns"部分为你提供了用于渲染页面的事件及其block的列表。除了上面提到的关于block的所有信息，你还会看到一个特别有利于优化的信息，那就是 "Duration"。

![./sylius_template_events_metrics.png](D:\study\ztnote\PHP Framework & Open Source Projects\Sylius\sylius_template_events_metrics.png)



### How to use template events for customizations?

When you have found an event in the place where you want to add some content, here’s what you have to do.

当你在想添加一些内容的地方找到了一个事件，下面就是你要做的。

Let’s assume that you would like to add some content after the header in the Sylius shop views. You will need to look at the `SyliusShopBundle/Resources/views/layout.html.twig` template, which is the basic layout of Sylius shop, and then in it find the appropriate event.

让我们假设你想在Sylius商店视图的头部（header）后面添加一些内容。你需要查看`SyliusShopBundle/Resources/views/layout.html.twig`模板，它是Sylius shop的基本布局，然后在其中找到适当的事件。

For the space below the header it will be `sylius.shop.layout.after_header`.

对于header下面的空间，它将是`sylius.shop.layout.after_header'。

- Create a Twig template file that will contain what you want to add.

  新建一个Twig模板，其中包含你想要添加的内容。

```twig
{# templates/block.html.twig #}

<h1> Test Block Title </h1>
```

- And configure Sylius UI to display it for the chosen event:

  为选定的事件添加Sylius UI配置，以便展示该模板。

```yaml
# config/packages/sylius_ui.yaml

sylius_ui:
    events:
        sylius.shop.layout.after_header:
            blocks:
                my_block_name: 'block.html.twig'
```

That’s it. Your new block should appear in the view.

这边完成了。新的block应该出现在视图中。

> Tip：Learn more about adding custom JS & CSS in the cookbook [here](https://docs.sylius.com/en/1.12/book/frontend/managing-assets.html).





### Passing variables to the template events

By default, Sylius Template Events provide all variables from the template. If you want to pass some additional variables, you can do it with the `context` key in the configuration. Let’s greet our customers at the top of the homepage:

默认情况下，Sylius模板事件提供了模板中的所有变量。如果你想传递一些额外的变量，你可以通过配置中的`context`键来实现。让我们在主页的顶部问候我们的客户：

```twig
{# templates/greeting.html.twig #}

<h2>{{ message }}</h2>
```

```yaml
# config/packages/sylius_ui.yaml
sylius_ui:
    events:
        sylius.shop.homepage:
            blocks:
                greeting:
                    template: 'greeting.html.twig'
                    priority: 70
                    context:
                        message: 'Hello!'
```

However, this simple way of passing variables may not be sufficient when you want to pass some complex data that comes as a result of application logic. Perhaps you would like to greet customers with their names. In such cases, you need to define your own `Context Provider`.

然而，当你想传递一些作为应用逻辑结果的复杂数据时，这种简单的传递变量的方式可能是不够的。也许你想用客户的名字来问候他们。在这种情况下，你需要定义你自己的 "Context Provider"。



#### Context Providers

Context Providers are responsible for providing context to the template events. The default one is the `DefaultContextProvider` which provides all variables from the template and from the context in the block’s configuration. You can have multiple Context Providers and they will provide their context to the template events with the given priority with the `sylius.ui.template_event.context_provider` tag.

Context Providers负责为模板事件提供上下文。默认的是`DefaultContextProvider`，它提供所有来自模板和block配置中的上下文的变量。你可以有多个Context Provider，它们将以`sylius.ui.template_event.context_provider'标签给定的优先级为模板事件提供上下文。

Let’s do something fancier than just greeting customers with a name. Say happy birthday to the customer! To do so, create a `GreetingContextProvider` that will provide the `message` variable from the example above but this time depending on the customer’s birthday:

让我们做一些更高级的事情，而不仅仅是用一个名字来问候客户。向客户说生日快乐 为此，创建一个`GreetingContextProvider`，它将提供上面例子中的`message`变量，但这次是根据客户的生日：

```php
<?php

declare(strict_types=1);

namespace App\ContextProvider;

use Sylius\Bundle\UiBundle\ContextProvider\ContextProviderInterface;
use Sylius\Bundle\UiBundle\Registry\TemplateBlock;
use Sylius\Component\Customer\Context\CustomerContextInterface;

final class GreetingContextProvider implements ContextProviderInterface
{
    public function __construct(private CustomerContextInterface $customerContext)
    {
    }

    public function provide(array $templateContext, TemplateBlock $templateBlock): array
    {
        $customer = $this->customerContext->getCustomer();

        if (null === $customer) {
            return $templateContext;
        }

        $customerName = $customer->getFirstName() ?? $customer->getFullName();

        if (
            null === $customer->getBirthday() ||
            $customer->getBirthday()->format("m-d") !== (new \DateTime())->format("m-d")
        ) {
            $templateContext['message'] = sprintf('Hello %s!', $customerName);
        } else {
            $templateContext['message'] = sprintf('Happy Birthday %s!', $customerName);
        }

        return $templateContext;
    }

    public function supports(TemplateBlock $templateBlock): bool
    {
        return 'sylius.shop.homepage' === $templateBlock->getEventName()
            && 'greeting' === $templateBlock->getName();
    }
}
```



Register the new Context Provider as a service in the `config/services.yaml`:

在config/services.yaml文件中，将新的Context Provider注册为一个服务。

```yaml
services:
    # ...

    App\ContextProvider\GreetingContextProvider:
        arguments:
            - '@sylius.context.customer'
        tags:
            - { name: sylius.ui.template_event.context_provider }
```

Now if the customer’s birthday is today, they will be greeted with a happy birthday message.

现在，如果客户的生日是今天，他们将收到一条生日快乐的信息。

![./sylius_template_events_greeting.png](D:\study\ztnote\PHP Framework & Open Source Projects\Sylius\sylius_template_events_greeting.png)

### What more can I do with the Sylius Template Events?

You might think that this is the only way of customisation with the events, but you can also do more.

你可能认为这是对事件进行定制的唯一方式，但你还可以做更多。

1. Disabling blocks:

   You can now disable some blocks that do not fit your usage, just put in config:

    你现在可以禁用一些不适合你使用的区块，只需在配置中放入：

   ```yaml
   sylius_ui:
       events:
           sylius.shop.layout.event_with_ugly_block:
               blocks:
                   the_block_i_dont_like:
                       enabled: false
   ```

   

2. Change the priority of blocks:

   In order to override the templates from vendor, or maybe you are developing plugin you can change the priority of a block:

   为了覆盖vendor（供应商）提供的模板，或者你正在开发插件，你可以改变一个block的优先级：

   ```yaml
   sylius_ui:
       events:
           sylius.shop.layout.vendor_block:
               blocks:
                   my_important_block:
                       priority: 1
   ```

   

3. Access variables:

   You can access variables by using the function:

   你可以通过函数来访问变量：

   ```twig
   {{ dump() }}
   ```

   You can also access the resources and entities (in the correct views) variables:

   你也可以访问资源和实体（在正确的视图中）的变量：

   ```twig
   # for example in products show view
   {{ dump(product) }}
   ```

   Or you can pass any variable from the template to the block and access it with function:

   或者你可以将模板中的任何变量传递给block，用函数访问它：

   ```twig
   # Parent html
   ...
       {{ sylius_template_event('sylius.shop.product.show', {'customVariable': variable}) }}
   ...
   ```

   ```twig
   # Template html
   ...
       {{ dump(customVariable) }}
   ...
   ```

   

4. Override block templates:

   You can override the existing blocks by changing the config:

   你可以通过改变配置覆盖已存在的block：

   ```yaml
   # config.yaml
   sylius_ui:
       events:
           sylius.shop.layout.header.grid:
               blocks:
                   logo: 'logo.html.twig'
   ```

   And adding your own template into templates/logo.html.twig folder.

   并在templates/logo.html.twig文件夹中加入你自己的模板。

> Note: Check out the full example of overriding the template in [Shop Customizations](https://docs.sylius.com/en/1.12/getting-started-with-sylius/shop-customizations.html)
>
> 注意：请查看[商店定制](https://docs.sylius.com/en/1.12/getting-started-with-sylius/shop-customizations.html)中覆盖模板的完整例子。



### How to use themes for customizations?

You can refer to the theme documentation available here: - [Themes (The book)](https://docs.sylius.com/en/1.12/book/themes/themes.html) - [SyliusThemeBundle (Bundle documentation)](https://github.com/Sylius/SyliusThemeBundle/blob/master/docs/index.md)





## Global Twig variables

Each of the Twig templates in Sylius is provided with the `sylius` variable, that comes from the [ShopperContext](https://github.com/Sylius/Sylius/blob/master/src/Sylius/Component/Core/Context/ShopperContext.php).

Sylius中的每个Twig模板都提供了`sylius`变量，它来自于[ShopperContext](https://github.com/Sylius/Sylius/blob/master/src/Sylius/Component/Core/Context/ShopperContext.php)。

The **ShopperContext** is composed of `ChannelContext`, `CurrencyContext`, `LocaleContext` and `CustomerContext`. Therefore it has access to the current channel, currency, locale and customer.

**ShopperContext**是由`ChannelContext`、`CurrencyContext`、`LocaleContext`和`CustomerContext`组成。因此它可以访问当前的渠道、货币、地域和客户。

The variables available in Twig are:

Twig中可用的变量有：

| Twig variable       | ShopperContext method name |
| ------------------- | -------------------------- |
| sylius.channel      | getChannel()               |
| sylius.currencyCode | getCurrencyCode()          |
| sylius.localeCode   | getLocaleCode()            |
| sylius.customer     | getCustomer()              |



### How to use these Twig variables?

You can check for example what is the current channel by dumping the `sylius.channel` variable.

```twig
{{ dump(sylius.channel) }}
```

That’s it, this will dump the content of the current Channel object.



### Good to know

See also：All the customizations can be done either in your application directly or in [Plugins](https://docs.sylius.com/en/1.12/book/plugins/index.html)!





## 理解

模板事件系统：Sylius将事件集成到模板中，事件像一个slot嵌入到模板结构中。当一个事件触发时，我们就可以向这个事件对应的所在slot嵌入我们想添加的block。

在Sylius Bundle中，事件名配置在Bundle的/Resource/config/app目录的config.yml或者events.yaml文件中，每个事件名就是一个slot，它包含要渲染的block等配置信息。

事件命名：事件名一般是由路由名 + slot组成。路由名表示事件触发在哪个请求，slot表示该事件在模板结构中的位置。

