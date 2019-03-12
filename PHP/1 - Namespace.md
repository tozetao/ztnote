### namespace

命名空间组织代码的一种机制，主要用于解决类、函数重名的一种机制。它可以将类（抽象类和traits）、常量、函数和接口等代码划分出不同的空间。



### 声明

命名空间在PHP文件的顶部，在<?php标签之后的第一行进行声明。每个PHP类、接口、函数和常量都在命名空间中。

```php
namespace MonderPHP\Compontent;

//code
class Request {
}
```



### 导入与别名

导入是指告诉PHP要使用哪个命名空间中的类、接口、函数或常量，当导入后就不需要输入全部名字来引用代码了。而别名是使用简单的名称来引入导入的类、接口、函数或常量。

```php
<?php
//不导入类来实例对象
$obj = new \MonderPHP\Compontent\Request();

//导入要使用的类，可以使用默认的别名来实例对象
use \MonderPHP\Compontent\Request;
$obj = new Foo();

//使用别名
use  \MonderPHP\Compontent\Request as Req;
$obj = new Req();
```





### 全局命名空间

有些代码没有在命名空间中，例如PHP的内置异常类Exception，这些代码在全局的命名空间中，应用全局命名空间的代码需要使用 \ 符号。

```php
<?php
namespace My\App;

class Foo {
    public function doSomething() {
        $exception = new \Exception();
    }
}
```