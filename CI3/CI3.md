## CI3生命周期
1. index.php作为入口文件，定义系统常量、加载CI核心文件
2. 由Routing组件处理请求，如果有缓存使用缓存相应，如果没有执行下一步
3. 进行请求数据安全检查，这一步做数据验证
4. 由Router解析创建Application Controller对象，负责处理请求
5. Application Controller加载模型、核心类库、辅助函数以及其他所有处理请求所需的资源；
6. 渲染视图进行相应。


## 控制器
在ci3中，控制器是整个应用的核心对象，它是一个Application容器对象，负责调度模型资源、视图资源，加载一系列其他组件资源来进行请求的处理。

ci3的控制器继承自CI_Controller类。
ci3的控制器有俩个概念要注意下，分别是重映射方法与数据输出。

Application Controller对象可以在控制器中访问，也可以通过get_instance()全局方法来获取，因为这个AC对象控制着CI中的所有资源，所以如果要引入类库，就需要创建自己的类库，加载到AC对象，由AC对象调用操作。

注：控制器命名、方法命名、变量和常量命名不要跟CI保留字冲突，因为CI3没有命名空间。


## URL路由
默认CI以段的形式来组织url，即：/class/function/params，分别对应控制器、方法、参数，也能通过定义配置文件使用古老的mvc url。
但是有时候我们想要改变这种URI的映射方式，这时候就需要使用路由。

路由配置文件位于/config/route.php中，在该文件中，将路由规则定义在$route数组中，数组的key表示要匹配的URI，值是要映射的控制器方法。

CI3中的路由支持正则、HTTP动词、回调函数，详细看手册

## 模型


## 视图
ci3并没有自己的模板引擎，需要你自己去引进。


## 类库
CI系统类库位于system/libraries目录下，大多数情况下我们要在控制器中初始化它，例如：
```php
$this->load->library('class_name');

$this->load->library('form_validation');
```

CI的类库提供了各种场景的解决方案，每个解决方案对应一个类，同时配合对应的辅助函数的时候，能够解决大部分的应用问题。


## 辅助函数
ci3中提供了大量的辅助函数，这些函数只是处理特定的功能，辅助函数使用需要去手动加载它，当然你也可以自动加载。

辅助函数位于system/helpers或者application/helpers目录下，常用的功能大体有：
**URL辅助函数**
**表单负数函数**
**文本辅助函数**
**cookie辅助函数**

## 全局函数
无须加载便可以使用的函数。


## Form类库与Form辅助函数
### 1. Form类库
Form_validation类提供对form表单的一系列验证，默认该类是对$_POST数组进行验证，大体功能有：
- 提供默认的验证规则和对应的错误信息
- 允许自定义验证规则，自定义错误信息
- 允许将多个验证规则组合配置，定义成一个规则集，在指定的时候调用。

- 支持表单字段填充
- 支持指定数组验证
- 支持错误定界符的更改

**自定义错误提示信息**
- system/langeuage/english，CI默认使用english包，我们可以自己自定义错误提示信息文件，目录位于：application/language/english/form_validation_lang.php

- 使用form_validation::set_message()方法定义验证规则的错误信息

- 使用form_validation::set_rules()方法的时候定义错误信息

注：变量替代，{fidld}用于替代field label，{param}替换某些验证规则自带参数，例如minlength[5]

**规则集**

对于第三点的说明，例如你有个登陆的form，可以将该form的多个字段的验证规则配置成一个数组，在调用Form_validation对象的时候指定调用。


### 2. Form辅助函数
CI的表单辅助函数能够自动化的创建表单，根据功能分类有：
- 生成表单标签
- 生成表单字段标签
- 根据Form类生成错误信息

**表单填充**
set_*()系列方法用于在视图界面上的form表单域赋值，默认的会从$_POST对象中查找数据，如果查找不到则使用默认值。

如果有使用到from_validation类的时候，set系列的方法将会去使用form_validation对象中的表单字段的值，而不是执行函数本身，如果from_validation对象找不到则使用默认值。

调用update()方法，无法填充模型对象表示从编辑入口进来，这时候set_*()方法只能使用默认值。成功填充对象但是无法通过验证仍然会返回_form页面，这时候将显示

**错误提示**
- form_error()
- validation_errors()
这俩个函数用于跟form_validation对象来进行错误的提示，函数将会输出form_validation对象的错误提示信息，可以全部输出或者输出指定字段的错误信息。



## 安全
1. 验证数据类型是否正确，包括长度、大小等等
2. 数据过滤：转义

隐藏重要程序文件，在web根目录站点放置index.php入口文件与资源目录即可


## CI的语言文件
CI_lang提供了一套用于获取语言文件和不同语言的文本来实现国际化，默认CI下载包提供了english语言文件，如果要自定义，需要重新去创建和加载它。

### 1. 加载原理
默认的ci会先加载system/language目录下的语言文件，之后会去加载application/language目录下的语言文件，语言类别的加载取决于config.php配置文件，当然你可以在使用的时候去切换它。

### 2. 使用
