## Render







## Component

- 全局注册

  整个APP应用中都可以使用该组件对象

- 局部注册

  只允许在注册的组件中使用

vue组件与vue实例类似，拥有vue实例大部分内容，不同的是vue组件的data是由函数来返回对象的。



某一些标签内是无法使用组件的，例如table元素会要求子元素必须是tbody、thead等标签，这种情况可以使用is属性来引用组件。

```html
<div id="app">
    <table>
        <thead is="myhead"></thead>
    </table>
</div>

<script>
    Vue.component('myhead', {
        'template': '<div></div>'
    });
    
    var vm = new Vue({
        'el': '#app'
    });
</script>
```





### props

组件之间是可以传递数据的，vue2.x支持单向数据流，即父组件向子组件传递数据，父级prop的更新会影响子组件，反过来则不行。之所以不让子组件向父组件传递数据是为了解耦，避免父子组件之间相互影响切难以理解。

```javascript
var child = {
    props: [
        'initCount'
    ],
    data: {
        return {
       		count: this.initCount
    	}
    }
};
//定义父组件向子组件传递的属性，一般子组件使用父组件时，会重新声明一个属性来引用父组件传递进来的数据，子组件是无法直接修改传递属性的。
```

props也是支持数据验证的，具体看手册。



$emit



### 自定义事件

子组件通过\$emit来触发事件，而父组件通过$on来监听子组件触发的事件。



- .native

  如果需要监听的事件是属于DOM事件，需要使用.native修饰符。

  ```html
  <my-component v-on:click.native="handle"></my-component>
  ```



- v-model

  2.2新增。

  一个组件上的v-model默认会使用名为value的prop和input事件，即将父组件的value属性绑定到子组件的prop中，而子组件触发input事件时，可以改变prop中绑定的属性。

  ```html
  <div id="app">
  	<my-com v-model="value"></my-com>
  </div>
  
  <script>
      Vue.component('my-com', {
          template: '<div>{{counter}}<button @click="handleClick">+1</button></div>',
          props: {
              value: {
                  type: Number
              }
          },
          data () {
              return {
                  counter: 0
              }
          },
          methods: {
              handleClick () {
                  this.counter++;
                  this.$emit('input', this.counter);
              }
          }
      });
  
      new Vue({
          el: '#app',
          data: {
              value: 1
          }
      })
  </script>
  ```



### 非父子组件的通信

在vue2中，推荐使用一个空的vue实例来作为中央事件总线（bus），也就是一个中介。

bus无学习成本，同时也可以扩展bus对象的属性，方法等等。

```html
<div id="app">
    {{message}}
    <component-a></component-a>
</div>

<script>
    const bus = new Vue();

    Vue.component('component-a', {
        template: '<button @click="handleEvent">CLICK</button>',
        methods: {
            handleEvent () {
                bus.$emit('on-message', 'im am from component-a');
            }
        }
    })

    new Vue({
        el: '#app',
        data: {
            message: ''
        },
        mounted () {
            bus.$on('on-message', (msg) => {
                this.message = msg;
            })
        }
    })
</script>

```







- 子组件索引

  当同一个子组件多次使用时，可以通过ref属性给组件增加索引；

  $refs，引用对象，通过该对象可以访问有ref属性的组件，

  ```html
  <comp1 ref="a"></comp1>
  <comp1 ref="b"></comp1>
  <script>
      this.$refs.a;
      this.$refs.b;
  </script>
  ```

  



- $parent

  返回当前组件的父组件对象

- $children

  返回当前组件中的所有子组件对象



### slot

slot可以分发内容，当混合父子组件的内容就会使用到slot。



vue组件是有作用域的，在下面代码中父组件是APP组件，子组件是child，而message属性是属于父组件的。

```html
<div id='app'>
    <child>{{ message }}</child>
</div>
<script>
    Vue.component('child', {
        template: '<div>child component.</div>'
    });
    new Vue({
        el: '#app',
        data: {
            message: 'parent message'
        }
    })
</script>
```

在上面代码中，message属性是无法显示的，要将父组件的内容嵌入子组件中，需要用到slot，例如：

```html
<div id='app'>
    <child>{{ message }}</child>
</div>
<script>
    Vue.component('child', {
        template: '<div>child component.<br/><slot></slot></div>'
    });
    new Vue({
        el: '#app',
        data: {
            message: 'parent message'
        }
    })
</script>
```

整个slot的声明周期和状态是由父组件来决定的。



- 默认slot

  即父组件没有指定slot的内容时，该slot的默认内容，slot的默认内容的生命周期归子组件管理。

- 具名slot

  给slot起名，方便使用

- 访问slot

  $slots.slotName，slotName是slot的名字



### example

递归组件

```html
<div id="app">
    <tree :count="1"></tree>
</div>

<script>
    Vue.component('tree', {
        template: '<div><tree :count="count + 1" v-if="count < 3"></tree>{{count}}</div>',
        props: {
            count: {
                type: Number,
                default: 1
            }
        }
    })
    new Vue({
        el: '#app',
    });
</script>
<!-- 组件是可以引用自身的，因此可以做递归引用，只需要有basecase即可。 -->
```



异步组件，按照需要加载组件

```html
<script>
    Vue.component('child-component', function(resolve, relect){})
</script>
```



$nextTick

组件创建完毕后会调用的事件。



手动挂载实例

```html
<script>
    const myComponent = Vue.extend({
        template: '<div>from extend</div>'
    });
    
    new myCompontent().$mount()
</script>
```





## webpack

前端工程化解决的问题：

- js、css合并与压缩
- css预编译
- 生成雪碧图
- es6转es5
- 模块化



### es6模块化

export与import关键字是用于导入和导出模块的，一个模块就是一个js文件，它拥有独立的作用域，在模块中定义的变量外部是无法获取的。

```js
//config.js
var config = {
    varsion: '2.2.2'
};
export {Config};

//config.js
export var config = {
    version: 2.3
}

//add.js
export function add(a, b){
    return a+b;
}

//main.js
inport {Config} from config.js
import {add} from add.js


//默认导出
export default {version: 3.0};
import Config from './config.js';
```

- note：对于npm导入的类库也可以使用import关键字来进行导入使用。



### Install

> cnpm install webpack --save-dev
>
> cnpm install webpack-dev-server

--save是本地安装，--dev指的是作为开发模式安装，webpack-dev-server是简单的web服务器。



关于启动

```json
{
  "scripts": {
    "dev": "webpack-dev-server --open --config webpack.config.js",
    "build": "webpack --progree --hide-modules"
  },
  "devDependencies": {
    "css-loader": "^1.0.0",
    "style-loader": "^0.21.0",
    "webpack": "^4.16.1"
  }
}
```

这里的安装都是局部安装，放在nmp_modules目录下，bin文件目录下，因此执行的时候会出现找不到命令，

可以使用npm的script来间接引导webpack-dev-server服务器的启动

> npm run dev
>
> dev：dev将会使用开发模式启动
>
> build：使用webpack对项目进行编译，会自动生成可使用的js、css等文件，具体看你配置使用。



简单的说，webpack是一个前端工程化的东东，通过进行一系列的配置，如配置加载器等插件，可用对项目源文件进行编译和打包。





### Configuration

```javascript
const path = require('path');

const config = {
    entry: {
        main: './main'
    },
    output: {
        path: path.join(__dirname, './dist'),
        publicPath: '/dist',
        filename: 'main.js'
    },
    module: {
        rules: [
            {
                test: /\.css/,
                use: [
                    'style-loader',
                    'css-loader'
                ]
            }
        ]
    }
};

module.exports = config;
```

- entry：入口配置，指定webpack的配置文件以及所依赖的包。
- output：出口配置，编译后的文件存储位置



关于加载器

使用不同的加载器对整个项目中的文件进行编译打包，不同的加载器可以帮助webpack处理不同的文件，它通过module来配置的。

> cnpm install css-loader style-loader --save-dev



vue单文件组件与vue-loader

> cnpm install --save vue
>
> cnpm install --save-dev vue-loader
>
> cnpm install --save-dev vue-style-loader
>
> cnpm install --save-dev vue-template-compiler
>
> cnpm install --save-dev vue-hot-reload-api
>
> cnpm install --save-dev babel
>
> cnpm install --save-dev babel-loader
>
> cnpm install --save-dev babel-core
>
> cnpm install --save-dev babel-plugin-transform-runtime
>
> cnpm install --save-dev babel-preset-es2015
>
> cnpm install --save-dev babel-runtime

> cnpm install url-loader --save-dev
>
> cnpm install file-loader --save-dev
>
> 

> cnpm install webpack-merge  --save-dev
>
> 把俩个配置文件合并成一个配置文件
>
> cnpm install html-webpack-plugin --save-dev
>
> 生成html webpack模板
>
> cnpm install uglifyjs-webpack-plugin --save-dev



生成环境下的配置

spa是单页面应用，也就是说通过webpack编译后的产出是单个html文件和其他js、css等资源文件。





Vue对象的生命周期

- created

  

- mounted

  vue对象被挂载到dom节点上触发

- activated

  对象被激活时触发，

- destroyed

  对象被销毁是触发



如果没有开启缓存，函数触发顺序为：created => mounted => destoryed;

在开启缓存时，组件会被缓存在内存中，第一次访问组件时函数触发顺序为：created => mounted => activated，之后再次访问组件时将会一直触发activated函数，直到该组件被销毁。