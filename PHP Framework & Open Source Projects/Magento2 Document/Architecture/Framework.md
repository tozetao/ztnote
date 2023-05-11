## Framework

Commerce框架控制应用组件的交互方式，包含请求流、索引、缓存和异常处理。它提供的服务减少了创建包含业务逻辑的模块的工作量，有助于实现更加模块化且减少依赖性的商业代码。

这个主要是PHP的软件组件被组织为称为库的逻辑组，所有模块都可以调用这些库。大部分的框架代码位于领域层之下，或者包含着表现层、服务层或领域层。该框架不包含业务逻辑。（虽然Commerce框架不包含资源模型，但是它确实包含一个代码库来帮助实现资源模型）。

你永远都不应该去修改框架文件，如果你正在扩展Commerce，你必须知道如何去调用框架库。你创建的模块通常会继承框架目录中定义的类和接口。



### Responsibilities

Commerce框架提供的库有助于减少创建包含商业逻辑模块的工作量。 它负责对所有模块有用的操作包括：

- 处理http协议
- 与数据库和文件系统进行交互
- 呈现内容



### Organization

以下是Commerce框架的目录结构：

- /vendor/magento/framework仅包含PHP代码。这些是代码库以及请求路由到模块（模块反过来调用framework库）的应用程序入口点。例如，框架中的库帮助实现资源模型，但不是模型本身。某些库还支持CSS呈现。
- lib/internal 包含一些非PHP代码和PHP组件，非PHP代码包含JavaScript和LESS/CSS。
- /lib/web 包含JavaScript和CSS/less文件。这些文件位于web目录而不是internal目录是因为它们可以被浏览器访问。而internal目录下的PHP代码则不能。（任何浏览器可以访问的代码都应该放在web目录下，而其他的应该放在internal目录下）

注：vendor/magento/framework目录映射到Magento\Framework命名空间。



### Framework highlights

框架重点

Commerce框架（lib/internal/Magento/Framework/）提供了一系列强大的功能。如果你是一个扩展开发者，你可能对整个框架命名空间的子集感兴趣。



作为扩展开发者，你可能对其他命名空间感兴趣。

- Magento\Framework\ObjectManager

  提供依赖注入

- Magento\Framework\App

  包含关于Commerce应用的框架代码。这段代码将引导引用并读取初始化配置。它也是命令行工具、web应用和cron job的入口。最后它在提供部署环境时（比如读取数据库配置、语言和缓存系统）路由请求。

