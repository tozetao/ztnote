### 模块

模块可以将相同功能的JS代码进行封装，一个模块就是一个JS文件。在requireJS中，通过define方法来定义模块：

```javascript
function define([id, deps], callback);
```

- id

  模块id，默认是文件名。

- deps

  当前模块所依赖的模块数组，数组中每个元素都是模块id。

- callback

  一个匿名函数，在这里编写模块的功能代码。该匿名函数可以有参数，它的参数从左到右依次对应模块数组中的每个具体模块的执行结果。



### 数据模块

即定义键值对的对象，用于保存数据或配置参数。

```javascript
//作为当前模块使用，当然也可以定义到一个文件中。
define('Config', {
    success: 1,
    failed: 0
});

require(['Config'], function(Config) {
    console.log(Config.success);
});
```



### 非依赖性模块

所谓的非依赖性模块指的是该模块不依赖其他模块。对于这种模块，在匿名函数中引用requireJS提供的三个对象：require、exports和modules。

```javascript
define(function(require, exports, modules) {
    //do something.
});
```

require是一个函数，加载模块时使用；exports用于导出模块的返回值；modules定义模块的相关信息以及参数。



### 依赖模块

如果有依赖模块，通过deps参数进行定义，同时所依赖的模块如果有返回值，可以在callback匿名函数的参数中，引用模块数组中对应模块的执行结果。

```javascript
define(['jquery'], function($) {
    console.log($);
});
```



### 载入

可以通过require()方法载入一个模块，并执行模块中的回调函数。

```javascript
function require(deps[, callback])
```

- deps

  所要载入的模块数组

- callback

  载入模块后执行的匿名函数。该匿名函数的参数对应模块数组中的具体模块。



require()可以主动载入模块，如果载入的模块也依赖了其他模块，会间接的载入依赖的模块，并且可以将依赖模块返回的方法或对象保存到一个变量中，以供使用。

```javascript
//index.js
define(['jquery'], function($) {
    $(function() {
        alert("load jquery.");
    })
});
require(['index']);
```





### 模块返回值

requireJS中定义的模块不仅可以返回一个对象作为结果，也可以返回一个函数作为结果。实现模块的返回值有俩种方式。

- return方式

  ```javascript
  //unit.js
  define(function(require, exports, modules) {
      function say(param) {
          alert(param);
      }
      
      return say;
  })
  
  //index.js
  define(function(require, exports, modules) {
      var unit = require('unti');
      unit.say();
  });
  ```

- export导出

  ```javascript
  //unit.js
  define(function(require, exports, modules) {
      function say(param) {
          alert(param);
      }
      
      exports.say = say;
  })
  
  //index.js
  define(function(require, exports, modules) {
      var unit = require('unti');
      unit.say();
  });
  ```



### 属性

map
​	在加载其他模块之前，都必须先加载map中的模块。

shim
​	通过require的define()方法定义的模块就是标准的，只有不准备的情况下才会使用shim定义的参数。









参考链接：

http://www.requirejs.cn/

https://www.cnblogs.com/HCJJ/p/6611669.html

https://blog.csdn.net/wrp920227/article/details/78055970