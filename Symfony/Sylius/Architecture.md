这里是理解Sylius内部设计原则的关键。在这里你将了解资源层、状态机、事件，以及平台中使用的与一般电子商务不同的概念，比如E-mail或Fixtures。



### Overview

在我们深入了解Sylius每个概念之前，你需要对我们的应用程序的结构有个了解。Sylius是由Symfony Bundles和组件构建的，它们是框架的集成层（integration layers）。

所有的bundles在命名和数据持久化的方式上都有相同的约定。Sylius默认使用Doctrine ORM来管理所有实体。要深入了解Doctrine的工作原理，请参考其官方网站上的优秀文档。



### Fullstack Symfony

Sylius是基于Symfony，这是一个用于创建Web应用程序的的PHP框架。使用Symfony可以让开发人员更好更快地工作，因为他们有把握开发出与业务规则完全兼容的、结构化的、可维护的和可升级的应用程序，而且它还可以通过提供通用的可重用模块来节省时间。



### Doctrine

Doctrine是一个专注于提供数据持久层的PHP库系列。其中最重要的是对象关系映射器（ORM）和数据库抽象层（DBAL）。Doctrine的主要特点之一是可以用Doctrine查询语言（DQL）编写数据库查询，这是一种面向对象的SQL方言。



### API Platform

API Platform是一个现代的解决方案，用于开发高质量的API。API Platform默认与Symfony一起工作，并依赖于其组件。



### Components

Sylius的每个组件都可以单独使用。以Taxation组件为例，它的职责是计算税收，不管是产品的税收或是其他东西的税收，它都是完全解耦的。为了让Taxation组件对你的对象进行操作，这些对象需要实现TaxableInterface。这样它们就可以计算出税收。

这种方法对Sylius的每一个组件都是如此。除了与电子商务需求严格相关的组件外，我们还有很多更通用的组件。例如Attribute, Mailer, Locale等。

所有组件都是Packagegist提供的软件包。

更多组件内容：https://docs.sylius.com/en/latest/components_and_bundles/components/index.html



### Bundles

Bundles是Symfony Bundles，如果你是Symfony开发者，并且你想在你的系统中使用Taxation组件，但是不想在配置表单和服务容器上花费时间。你可以包含TaxationBundle，只需最少配置甚至不需要配置就可以访问所有服务、模型、配置税率，税收类别，并且用于你想要的任何税收。

https://docs.sylius.com/en/latest/components_and_bundles/bundles/index.html



### Platforms

这是一个全栈式的Symfony应用程序，基于Symfony标准。Sylius平台给予了你经典的、功能丰富的Web商店。

在你开始使用Sylius之前，你需要决定你是否需要一个具有我们提供的所有功能的完整平台，或者也许你会使用解耦的Bundles和compontens来建立一些定制的东西，也许是小型的，具有不同功能的商店。当然平台本身是高度灵活的，可以很容易地进行定制，以满足你可能有的所有业务要求。



### Core

Core是另一个集成了所有其他组件的组件。

例如，ProductVariant最终了解到它有一个TaxCategory。核心组件是ProductVariant实现TaxableInterface和其他对其操作有用的接口的地方。Sylius在这里有一个完全集成的概念，它涵盖了运营WebShop所需的一切。



### Admin

在每一个具有安全层的系统中，系统管理的功能需要被限制在只有一些具有特定角色的用户--管理员。这是AdminBundle的责任，如果你不需要它可以把它关掉。视图是使用SemanticUI构建的。



### Shop

我们的ShopBundle基本上是一个标准的B2C接口，用于系统中发生的一切。它主要由yaml配置和模板组成。此外，这里的视图也是使用SemanticUI构建的。



### API

我们在API Platform上创建API时，我们尽可能的提供简单且易于使用的API给开发者使用。API最重要的功能是：

- 所有的操作都按照shop和admin上下文进行分组（俩个前缀）
- 开发者可以通过改变单个参数来启用或禁用整个API（查看本章）。

- 基于REST创建了所有终端，使用了HTTP动词（GET、POST、PUT、PATCH、DELETE）。
- 返回的响应包含最小的信息（如果需要更多的数据，开发者应该扩展序列化）。

- 整个业务逻辑与API分离--如果有必要，我们会派发命令，而不是将API逻辑与业务逻辑混合在一起。

Tip：如果你正在寻找Shop API，这是一个以Customer运作的API，那么你将需要官方的Shop API插件。



Third-Party Library

Sylius使用大量的库来完成各种任务。

- Payum用于支付
- KnpMenu - 用于商店和管理菜单
- Gaufrette用于文件系统的抽象（在本地、Amazon S3或外部服务器上存储图片）。
- Imagine用于图像处理，生成缩略图和裁剪
- 用于分页的Pagerfanta
- Winzou状态机 - 用于处理状态机