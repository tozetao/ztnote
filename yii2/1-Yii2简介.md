## Yii目录结构

Yii框架默认有2个归档文件，除了自身包含的类库外，还提供了Bisic和Advance俩个级别的模板。

### 1. basic模板目录结构

- /config：项目配置文件目录
- /web：web应用根目录，包含index.php入口文件和assets资源文件目录(js/css)
- runtime：运行时目录，包含日志文件，缓存文件
- vendor：Yii框架自身类库以及第三方扩展
- /commands，控制台命令泪
- /tests：测试文件类
- /assets：资源文件

### 2. advance模板目录结构
advance模板在basic基础进行了扩展，基础目录不变，只不过将mvc封装成模块来进行处理，例如有：
- /backend：该目录是后台系统目录，包含了mvc模板
- /frontend：该目录是前台系统目录
- /common：公共目录，包含配置文件，公共模型目录文件等其他公有的模块。










## 入口脚本
应用能分为web应用和控制台应用俩种，一个应用只能有一个入口脚本，一般入口脚本名为index.php。

yii控制台应用的入口脚本在根目录下，命名为yii，该文件需要有执行权限，这样就能通过命令./yii <route> [arguments] [options]来运行控制台应用。

入口脚本一般做以下几件事情：
- 定义应用常量
- 注册composer自动加载器
- 包含Yii类文件
- 加载配置文件
- 创建应用对象并配置，使用应用对象来处理请求

### web应用
以下是web应用入口脚本代码：
```php
<?php
	defined('YII_DEBUG')  or define('YII_DEBUG', true);
	defined('YII_ENV') or define('YII_ENV', 'dev');
	
	#注册composer加载器
	require(__DIR__. '/../vendor/autoload.php');
	#加载yii类我呢见
	require(__DIR__ . '/../vendor/yiisoft/yii2/Yii.php');
	
	# 加载应用配置
	$config = require(__DIR__ . '/../config/web.php');
	
	# 实例应用对象并运行
	(new yii/web/Application($config))->run();
```

### 定义常量
index.php入口脚本是定义常量的好地方，在该脚本页面定义的常量，在整个项目中都可以用到，Yii支持3个常量：
- YII_DEBUG，标识应用的调试模式
- YII_ENV，标识应用的运行环境
- YII_ENABLE_ERROR_HANDLER，标识是否启用yii提供的错误处理，默认true。












## 应用主体（Application）
Yii有一个应用主体对象的概念，负责管理整个应用的整体结构和生命周期，一个应用只会有一个主体对象，通过\Yii::$app表达式能在全局范围内访问。

Yii有俩种应用主体对象，网页应用主体和控制台应用主体。

### 1. 应用主体配置
```php
# 加载配置文件
$config = require(__DIR__ . '/../../web.php');

# 使用配置文件实例化Application对象
(new yii\web\Application($config)->run());
``` 

### 2. 应用主体对象的生命周期
当入口脚本处理请求的时候，应用主体对象会经历以下生命周期：
1. 入口脚本加载配置并实例化应用主体对象

2. 应用主体调用preInit()配置几个高级别应用主体属性，比如yii\base\Application:basePath.
3. 注册yii\base\Application:errorHandler错误处理方法
4. 配置应用主体属性
5. 调用init()初始化，该函数会调用bootstrap()运行引导启动组件

6. 入口脚本调用yii\base\Application::run()
7. run()方法内将会处理请求：解析请求路由和相关参数，创建路由指定的控制器，执行对应的动作，在动作内会调用模型，做业务处理，最后发送相应到客户端。


### 3. 应用主体对象的属性
应用实体对象的属性指定了应用主题对象的运行环境，比如应用主体对象如何加载控制器、临时文件应该保存到哪里等等。

应用主体对象的属性是通过config配置文件来进行配置的。

### 必要属性
- yii\base\Application::id，应用的唯一标识。
- yii\base\Application::basePath，指定应用的根目录，跟目录是mvc模块，包含应用所有源代码

### 重要属性
- controllerNameSpace，指定控制器的命名空间
- modules，指定应用所要包含的模型
- compontents，最重要的属性，允许定义在多个地方使用的应用组件

- charset，指定应用使用的字符集
- defaultRoute，默认的路由
- layout，指定视图使用的布局名，
- layoutPath，该属性指定视图查找布局文件使用的路径

- language，指定应用终端显示的语言包
- params，该属性是一个数组，指定可以被全局访问的参数。

- yii\base\Application::aliases，别名属性，是一个数组，允许你定义多个别名
- bootstrap，该属性指定处理请求前要初始化的组件
- catchAll，该属性仅web应用支持，指定了一个处理所有用户请求的控制器方法

### 应用事件
应用主体在处理请求的时候会触发一些事件，具体看手册






## Gii
Gii是一个基于web的代码生成器，用来生成模型、控制器、表单、增删改查等功能的代码。Gii也叫做脚手架。
/config/main-local.php查看是否开启Gii，输入网址：
?r=gii，便可以在该页面进行操作了。

**Gii的使用步骤**
- 生成模型类，yii将会根据数据库表结构来做规则验证
- 生成CRUD，这一步是生成控制器的增删改查的业务代码，包括视图模板文件。









## YII模型验证规则
在CI中，如果模型的验证规则不是required必须的，在属性有值的情况下会做验证，如果属性没有值不会做验证。

场景决定了模型的活动属性，即在某个场景下允许模型哪些属性能做块赋值和验证规则的验证。

Model的scenaris属性定义当前的场景，
Model的scenarios()方法返回一个数组，key是场景名，值是对应的活动属性，该方法默认返回rules()中申明的验证规则中的场景，scenarios()方法定义哪些属性应被验证，哪些属性是安全属性（允许被块赋值）。

在rules()方法中，设置属性验证规则时，可以通过on属性设定该属性的验证场景，如果未定义on属性，默认在所有场景生效。