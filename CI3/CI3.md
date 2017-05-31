## 简介
轻量级框架，基于mvc构建的框架。除了实现mvc模式之外，拥有系统类库和辅助函数。

## CI3生命周期
1. index.php作为入口文件，定义系统常量、加载CI核心文件
2. 由Routing组件处理请求，如果有缓存使用缓存相应，如果没有执行下一步
3. 进行请求数据安全检查，这一步做数据验证
4. 由Router解析创建Application Controller对象，负责处理请求
5. Application Controller加载模型、核心类库、辅助函数以及其他所有处理请求所需的资源；
6. 渲染视图进行相应。

## URL路由
默认CI以段的形式来组织url，即：/class/function/params，分别对应控制器、方法、参数，也能通过定义配置文件使用古老的mvc url。
但是有时候我们想要改变这种URI的映射方式，这时候就需要使用路由。

路由配置文件位于/config/route.php中，在该文件中，将路由规则定义在$route数组中，数组的key表示要匹配的URI，值是要映射的控制器方法。

CI3中的路由支持正则、HTTP动词、回调函数，详细看手册


## 控制器(CI_Controller)，CI超级对象
在ci3中，控制器是整个应用的核心对象，控制器决定了HTTP请求被如何处理。
CI_Controller控制器对象通过CI_Loader对象来加载和调度其他资源对象，例如模型对象、CI系统类型、CI系统辅助函数。

CI_Loader对象提供接口加载不同资源对象，再通过CI_Controller控制器对象来实例化访问，
可以将CI_Controller理解为CI超级对象，它所加载的资源可以在Model层和View层使用，如果要在项目其他地方调度这些资源对象，可以使用get_instance()全局方法来获取。

之所以能在View层和Model层调用CI超级对象的资源，是因为CI重写了View和Model对象的__get()方法。

注1：控制器命名、方法命名、变量和常量命名不要跟CI保留字冲突，因为CI3没有命名空间。
注2：ci3的控制器有俩个概念要注意下，分别是重映射方法与数据输出。

## 模型
- 模型加载：CI_Controller::load::model($model_name, $alisa)，$alias是别名
- 模型调用：CI_Controller::$model_name


## 视图
- 加载：CI_Controller::load::view($view, $data)，$view是视图路径，$data是视图数据


## 类库
类库是CI框架内置提供的，可以认为是各种功能组件，当然CI允许你自定义类库、扩展和替换系统类库。

目录说明：
- 核心类库：位于system/libraries目录下
- 自定义类库：位于application/libraries目录下
- 扩展核心类型：同自定义类库一样的路径，扩展的核心类型文件名和类名都是以MY_作为前缀来命名，该前缀可修改。

调度说明
- 加载：CI_Controller::load::library($library_name);
- 调用：CI_Controller::$library_name

说明：CI的类库提供了各种场景的解决方案，每个解决方案对应一个类，同时配合对应的辅助函数的时候，能够解决大部分的应用问题。


## 辅助函数
CI内置了大量的辅助函数，辅助函数主要用于处理特定的功能，同样也可以扩展和替换CI中内置的辅助函数。

目录说明：
- 系统辅助函数：位于system/helpers目录下
- 自定义辅助函数：位于application/helpers目录下，以_helper结尾命名
- 扩展辅助函数：以MY_为前缀，辅助函数文件名字来命名，前缀可以修改。



## 全局函数
无须加载便可以使用的函数。


## Form_Validation类
### 1. Form_Validation类库
Form_validation是一个数据验证类，提供各种数据验证规则，它支持：

- 验证数据源设置，默认$_POST
- 灵活的验证规则设置：包括自定义、使用配置文件分组配置
- 自定义错误提示信息
- 灵活的获取错误信息
- 预处理
- 数组作为数据域处理

与Form_Validation类对应的是Form_Validation辅助函数，具体看手册





## CI语言文件
CI_lang提供了一套用于获取语言文件和不同语言的文本来实现国际化，默认CI框架提供了english语言文件，如果要汉化需要去扩展它。

目录说明：
- system/language：CI框架系统语言类的存放路径
- application/language：应用语言文件的存放路径

配置：
application/config/config.php 配置文件中指定默认语言，如果想让你的项目支持多语言，可以在system或application目录下的language目录下创建不同语言文件，再通过config.php文件配置

### 1. 创建语言文件
语言文件必须以_lang.php结尾，在此文件中，是把一个字符串赋值给名为 $lang 的数组，例如：$lang['language_key'] = 'str';

### 2. 加载
### 3. 读取


## 错误处理
对于错误信息分为如下俩种情况处理：
- PHP程序报错：默认CI会显示所有的PHP错误，在index.php脚本的error_reporting()函数控制
- 错误日志：记录程序执行的错误

ci中将错误日志信息分为3个级别，这3个级别与php错误级别是不同概念的，ci的错误类型是：
- error：错误信息，将会记录用户的错误信息和PHP错误信息（各种级别的）
- debug：调试信息，一些对开发有些的信息
- info：一般信息