## Introduction

开发手册包含了开发人员想要了解更新有关框架组件的开发或修改的信息。有了这些知识你就可以扩展或者自定义任何现有组件。你还可以创建新功能的组件并发布给商家。



### Components

Magento开源应用程序由模块、主题和语言包组成。

- 模块（Modules）

  模块与应用程序的其他部分交互以完成特定的业务功能或提供某些特性。

  一个模块可以包含显示用户信息或交互的用户界面，它也可以包含另一个模块或代码块可能调用的应用接口。

- 主题（Themes）

  通过改变店铺或后台的外观，为每个框架的安装提供了个性化的触感。在默认的代码结构中已经有俩个主题。Blank主题和Luma主题。在创建自定义主题时，请参考这些默认主题。

- 语言包

  通过为显示在店铺和后台上的字符串提供翻译来进行国际化和本地化。

注：在构建一个模块时你必须遵循PSR-4标准。



## Developer roadmap

本主题介绍了想要创建或定制Magento开源应用程序的开发人员的工作流程。开发人员也可以将他们的定制产品打包并分发给商家。

为了满足创建或定义应用程序的最低要求：

- 在composer.json中声明组件的依赖性。
- 虽然你可以自己管理依赖关系，但建议并强烈鼓励使用composer.json文件，这是最佳实践。
- 使用registration.php文件注册组件。
- 使用组件专用的XML定义文件：
  - 模块：module.xml
  - 主题：theme.xml
  - 语言包：language.xml
- 发布你的组件：
  - 使用.zip格式打包你的组件
  - 如果你要上传组件到Commerce Marketplace，打包后的组件应小于30M。



## Common Trems

以下主要介绍Component和Metapackage俩个术语。

### Component

我们把你编写的代码称为组件（composer称为包，组件和包这俩个词是等价的）。一个组件可以分为以下几种类型：

- Module：扩展应用功能
- 主题：更改店铺和后台的外观
- 语言包：本地化店面和后台

你可以按照以下方式打包你的组件：

- 独立包

- 作为一个Metapackage，如果你开发的产品具有一个以上的包就需要打包成Metapackage，这是Commerce Marketplace的要求。

  一个Metapackage包含共享的包。比如一个Metapackage包含一个主题和模块、俩个主题、俩个模块等等。

### Metapackage

Commerce Marketplace需要将多个组件打包为Metapackage，Metapackage仅包含一个composer.json文件及其依赖的组件（Commerce Marketplace也将元包称为扩展）。

。。。。







