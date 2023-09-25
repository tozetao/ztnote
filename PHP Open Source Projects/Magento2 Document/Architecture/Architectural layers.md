## Domain layer

领域层保存模块的业务逻辑层，它通常不包含特定资源或特定数据库的信息。其主要功能包括：

- 定义包含业务逻辑的数据对象或模型。这个逻辑定义了在特定数据类型上可以执行哪些操作，比如Customer对象。而这些模型仅包含普通信息。应用也可以使用SOAP或RESTful从模型中请求数据。
- （可选的）包含服务契约的实现，尽管不是服务契约不属于它们的定义。

最佳实践：使用服务契约通过传递强类型对象去跟领域层交互。这可以帮助你避免在需要替换业务逻辑层代码时去替换表示层代码。



### Models

每个领域层模型都包含对一个资源模型的引用，它使用资源模型通过MySQL调用去数据库中检索数据。该资源模型包含连接到底层数据库（通常是MySQL）的逻辑。只有当模型数据必须持久化时，模型对象才需要资源模型。



### Accessing the domain layer

有三种主要的方式去访问模型的领域层代码：

- 服务契约是一个模块去访问另外一个模块的领域层代码的推荐方式。这种松耦合的解决方案是大多数模块去访问另外一个模块的最佳方式。
- 一个模块可以直接调用另一个模块。这种紧密耦合的解决方案不建议用于大多数情况，但有时是不可避免的。
- 一个模块中的代码也可以通过以下方式把自身插入到另外一个模块：
  - 事件钩子（event hooks）
  - 插件
  - di.xml文件（使用SPI契约）

调用其他模块的领域层代码的策略在很大程度上是取决于系统的独特配置和需求。





## Persistence layer

Magento框架使用活动记录模式策略来进行持久化。在这个系统中，模型对象包含映射到数据库一行或多行记录的资源对象。资源模型负责执行以下功能：

- 执行所有CRUD请求（create, read, update, delete）。资源模型包含完成这些请求的SQL代码。
- 执行其他业务逻辑。比如一个资源对象可能执行数据验证，start processes before or after data is saved，或者其他其他数据库操作。

如果你期望从一个数据库查询中返回多项数据，那么需要实现一个被称为Collection的特定类型的资源对象。Collection是一个类，它会基于一组规则加载多个模型到到一个类似于array的结构体中。这有点类似于SQL的where子句。

一个简单的资源模型定义一个表并与之交互。

然而一些对象可能包含很多属性，或者它们可能拥有一组对象，这些对象包含不同不同数据的属性。对于这种情况，对象是使用Entity-Attribute-Value（EAV）模型构建的。

任何使用EAV资源的模型，它的属性会分布在MySQL的多张表上。Customer，Catalog和Order资源模型都使用到EAV属性。



XML Declarative Schema

在Magento2.3框架中，我们引入了声明式的XML Schema。这些XML文件用于指定数据库的最终状态。这些文件会替代升级一个模块所需的PHP更新脚本。这些文件允许您跳过渐进式升级脚本并直接跳转到数据库的最终状态。



## Persentation layer

当你与Magento的web界面交互时，你是在与表示层代码进行交互。表示层位于四层模型中的最顶层。

表示层包含视图元素（layouts, blocks, templates）和控制器，处于来自用户界面的命令。表示层代码控制web用户与产品和界面的交互。

你可以通过使用HTML、CSS和PHTML文件来修改表现层的元素以去定制用户界面。基本上，表示层代表了HTML、CSS、JavaScript、UI、PHTML文件和块文件的定制。



Who uses the Presentation layer?

Magento2框架使用areas来搞笑的进行web服务调用，仅加载特定类型用户所需的依赖代码。三种类型的用户与表示层代码交互：

- 与店面交互的web用户。在那里他们可以看到由框架显示的数据的视图模型，并与产品UI元素互动，请求查看和操作数据。这些用户在frontend area（前端区域）内工作。
- 定义页面的系统管理员。它可以间接的操作表示层。例如在前端添加主题或小部件。
- web API调用可以像浏览器请求一样通过HTTP进行，也可以通过用户界面的AJAX调用进行。



Presentation layer components

了解表示层组件的一个有效方式是通过examining主题。

主题组织了店面视觉和产品某些方面的行为。每个主题都放在一个唯一的目录中，并包含自定义页面布局、模板、皮肤和语言文件，这些文件一起工作以创建独特的用户体验。

有关主题元素的详细介绍以及如何扩展和覆盖默认主题的概述，请参阅《前端开发人员指南》。



GraphQL

GraphQL是一种数据查询语言，由Facebook在2012年内部开发，然后在2015年公开发布。商务框架实现了GraphQL，为前端开发提供了REST和SOAP网络API的替代品。

GraphQL允许你定义你需要的数据结构，而服务器只返回你请求的数据。每个支持GraphQL的模块都包含一个声明性的模式（declarative schema），schema定义了模块支持的查询语法，以及可以返回的属性。如果你在一个简单的产品上运行GET /V1/products/:sku这样的REST调用，系统可能会获取超过100行的数据。如果你只需要当前的价格，那么这个调用所返回的信息就大大超过了你的需要。使用GraphQL，对同一SKU的查询可以只返回价格。

更多信息可以在GraphQL开发者指南中找到。



Progressive Web Apps

Progressive Web App（PWA）Studio项目是一组开发人员工具，允许您在Commerce框架上开发、部署和维护PWA店面。PWA是一种使用一组React JavaScript组件呈现店面的方法。使用Commerce框架作为后端，您可以使用PWA组件创建移动友好的前端。



View model

Commerce框架为一个页面生成HTML，从视图元素的树状结构中显示给用户。

视图元素分为俩大类：blocks和containers。

Blocks可以生成动态内容，并且可以包含与传入参数类似的命名子视图元素。（as属性保存父块的子视图元素名称以引用它们）。

Containers收集一组有序的子视图元素。

浏览器是请求视图元素树，视图元素树render为HTML来渲染产品页面。Blocks和Containers emit HTML，将其子对象适当的封装起来。Blocks可以使用静态HTML、Knockout JS scripts和PHTML。





How Presentation code calls other layers#
Presentation code typically calls service contracts, particularly for a storefront. However, presentation code is occasionally dependent on a specific implementation that requires the presentation code to directly call the domain layer. For example, the Admin UI screens are often tightly linked to a specific implementation and are not generic across implementations.

The View layer calls code from the Model to get information about the state of the application (for example, the price of a product). Typically, the way it accesses the Model is through service contracts.





Presentation layer flow#
Web users interact with components of the presentation layer to select actions that initiate calls to the underlying layers. Presentation layer components make calls to the service layer, which in turn sends requests to the domain layer.





## Service layer

服务层是表示层和领域层以及特定资源数据之间的中间层。这是用服务契约实现的，使用PHP interfaces来定义。

一般来说服务层有以下特点：

- 位于表现层之下，领域层之上。
- 包含服务契约，它定义了实现的行为。
- 提供一种简单的方式去访问REST/SOAP API框架代码（也位于服务契约之上）。你可以在配置文件中绑定服务契约到web API服务调用中而不需要编码。
- 为其他模块的调用提供一个稳定的API。



Who accesses the service layer?

所有来自web service接口或者使用店铺的用户的所有调用（即控制器启动的请求），一般通过服务层路由。我们强烈建议使用服务契约来调用业务逻辑。

外部应用程序可以通过简单的SOAP和REST调用对业务逻辑进行请求。通过一些简单的XML和JSON，你可以暴漏服务层的PHP API，并使其可以被SOAP和REST web服务访问。一旦实现，web服务就可以进行单个API调用并返回丰富的数据结构。

服务契约（service contract）客户端包括：

- 控制器（由店面用户的操作启动）
- Web Service（SOAP或者REST API调用）
- 通过服务契约的其他模块



Service contract anatomy

一个模块的服务契约是由模块的/api目录的一组接口定义的。

该目录包含：

- 模块的/Api命名空间中的服务接口。

- Api/Data目录中的Data或entity接口。

  数据实体是服务接口要传递或返回的数据。在数据目录中的文件包含实体表中的条目和扩展属性的get()和set()方法。

通常服务契约提供三种不同类型的接口：

- Repository interfaces
- Management interfaces
- Metadata interfaces

然而并不要求服务契约必须符合这三种模式。



Advantages of services contracts

服务契约允许你去添加一个新的自定义扩展，同时添加或更改业务逻辑级别的资源模型而不会破坏系统。

这是使用自定义模块的依赖注入配置文件（di.xml）的<preference>元素完成的。di.xml文件指定了哪些PHP类去使用Magento\Customer\Api\CustomerRepositoryInterface接口。

另一个模块可以通过指定不同的类名来修改该接口文件。但是如果客户端仅使用接口定义，则无需更改类名。



