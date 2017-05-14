## PHP自动加载
在php中，我们需要使用某个类的时候，必须先include/require包含该文件，然后才能访问，自动加载就是为了避免我们频繁的加载文件而产生的。

原理：当一个脚本文件在创建对象的时候，它会在当前脚本文件中寻找类代码，如果找不到会自动执行__autoload()函数。

### spl_autoload_register()
编写自己的加载类函数，并将其注册到autoload队列中，解决了在项目中导入不同类库覆盖autoload的问题。

spl_autoload_register()函数会将zend engine的__autoload()函数替换为spl_autoload()或spl_autoload_call()，

所以在使用这个函数的情况下实现了__autoload()函数，它必须显示的注册到__autoload()队列中。

### __autoload()
该函数也是一个魔术方法，基本弃用了。
