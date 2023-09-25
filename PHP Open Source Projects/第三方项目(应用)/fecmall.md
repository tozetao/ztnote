### 配置文件分析

- common config

  /common/config/main.php，common公用配置。

  定义Yii2框架基本组件，包括UrlManager、Session、DB、Cache、Redis组件

- appfront config

  /appfront/config/main.php，/appfront/config/main-local.php。

  appfront配置定义了当前应用主体配置，包括主体id、时区、基础目录、控制器命名空间、以及日志、错误处理、参数等组件。



- fecshop扩展包公用配置

  /fecshop/config/fecshop.php，配置公共服务和组件。

- fecshop扩展包appfront入口配置

  /fecshop/app/appfront/config/appfront.php，配置appfront应用的模块，同时配置所需要的业务逻辑组件和服务。比如i18n、request、user、errorHandler组件和page service。



- fecshop第三方配置

  暂时不去了解第三方。



- fecshop本地公用配置

  /common/config/fecshop_local.php，配置当前项目的公用services和compontens。

  在这里重写系统编写自己的业务逻辑？



- fecshop本地入口配置

  /appfront/config/fecshop_local.php，配置当前项目appfront主体的公用services和compontens。







### Yii.php

Yii不仅实现了符合PSR4的类自动加载，同时也挂载了整个应用的全局静态对象。比如：\$app、\$container、\$classMap都是初始化后挂载在Yii类上的，Yii提供了一个全局访问对象的方式。



fecmall重写了该类，新增全局\$services对象，\$rewriteMap对象。

$services是由\fecshop\services\Application初始化再挂载到Yii类上的，它提供了整个应用中的各种服务对象；\$rewriteMap对象提供了重写fecmall应用Block层和Model层对象的配置。





### Yii ClassMap

类映射表是Yii2类自动加载器支持的功能，可以建立起类名字到类文件路径的映射关系。当系统在实例化一个类的时候快速的去加载类文件。

添加类映射：

```php
Yii::$classMap['foo\bar\MyClass'] = 'path/to/MyClass.php';
```





### RewriteMap

fecmall的重写机制，通过rewriteMap可以重写Block层和模型层。

比如重写appfront应用的某个Block，只需要打开@appfront/config/fecshop_local.php文件：

```php
$fecRewriteMap = [
    '\fecshop\app\appfront\modules\Cms\block\home\Index'  => '\fectfurnilife\appfront\modules\Cms\block\home\Index',

    '\fecshop\models\mongodb\Category'  => '\appfront\local\local_models\mongodb\Category',
];
```

在fecRewriteMap数组中重新配置即可。





### Services分析

Services是在整个系统中可使用的服务，通过Yii::$services来调用。

$services对象是fecshop\services\Application对象实现的，它本身就是个容器对象，不仅保存了services对象的配置，还保存了已经初始过的service对象。

实现很简单，看下fecshop\services\Application代码实现就ok了。





### 执行流程

初始化配置

- Yii2框架配置
- fecmall服务配置、模块配置
- fecmall本地服务配置、本地模块配置，这部分的配置是客户重写的配置



初始化$services对象、\$rewriteMap对象

- 初始化Services对象，加载到Yii类上。





### init.php

init.php脚本用于初始化环境。

我们可以在environments目录中放置local配置，init脚本会根据环境的不同，将environments目录的配置文件拷贝到各个应用目录下。



默认有dev和prod环境：

- dev

  开发环境，显示所有debug错误并打开所有调试工具。

- prod

  生产环境，关闭所有开发工具，关闭错误显示。



xxx-local.php

local是本地配置的意思，一般是个人配置文件或服务器配置文件。

例如生产环境的DB、Redis配置可以放在main-local.php中，local配置文件是私有的，这些本地配置不推送到仓库。

说明：在应用的config目录下通过.gitignore文件可以看到要忽略版本控制的文件。

















### todo

fecmall请求的执行流程？











想想平时项目中出现的问题？项目代码的改进。

- service层

  控制器与模型层之间的缓冲，service会为controller提供统一的接口来负责处理请求。

- model层

  实体数据，一个模型对象代表一条对象，一个模型类对应一张表。

  模型对象不仅负责实体数据的操作，例如存储、更新和访问实体字段，也负责实体数据的验证。不要将过多的逻辑放到model里面，而是放到service层中。