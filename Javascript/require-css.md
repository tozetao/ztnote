### require-css.js

用于管理css样式，通过它可以更好的管理项目中的各个插件。



### 模块配置

在定义模块的时候可以配置要加载css样式，这样当你加载这个模块时，对应的css样式文件也会被加载。

```js
define('foo', ['css!css/foo.css'], function() {
    console.log('hello world');
});
```

字符串css!表示使用require-css.js插件来加载样式，后面则是css样式文件相对于baseUrl的路径。



### 全局配置

在config对象中的shim属性中配置模块的依赖，当加载该模块的时候就会加载依赖的样式，可以定义多个依赖样式。

```js
require.config({
    baseUrl: 'common/',
    paths: {
        'boot': 'bootstrap/dist/js/bootstrap',
        'jquery': 'jquery/dist/jquery'
    },
    // 在加载所有模块之前，必须先加载require-css.js模块
    map: {
        '*': {
            'css': 'require-css'
        }
    },
    //定义模块的依赖
    shim: {
        'boot': {
            deps: ['css!bootstrap/dist/css/bootstrap.css', 'css!bootstrap/dist/css/bootstrap-theme.css']
        }
    }
});

```

