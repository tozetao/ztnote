### 变量的作用域链

var关键字声明的变量，如果是在函数内声明的，变量是挂载在当前函数对象上的；如果是全局声明，那么变量是挂载在全局对象window对象上的。

```javascript
var x = 'global';

function outside(){
    var x = 'outside';
    
    function inner(){
        var x = 'inner';
        console.log(x);
    }
    
    console.log(x);
}

outside();
```



### 变量提升
Javascript程序并不全是一行一行指向的，代码的执行会有俩个阶段：
1. 在编译阶段会进行词法解析
2. 运行阶段代码会顺序执行

编译阶段会在整个文件中寻找var声明的变量，如果有var声明的变量会将其放到当前作用域的最前面，而赋值语句会停留在原来代码位置。

example:
```javascript
console.log(a)
var a = 'hello';

//上面的代码等价于

var a;
console.log(a);
a = 'hello';
```

函数声明会提升到作用域的最前面，函数表达式则不会。
example:
```javascript
show();		//success
function show(){}

show();		//error
var show = function(){}
```



提升规则：

- 编译阶段会将函数声明和使用var关键字声明的变量提升到各自当前的作用域

  ```javascript
  //编译前
  show();
  function show(){
  	console.log(a);
  	var a = '123';
  }

  //编译阶段
  function show(){
  	var a;
  	console.log(a);
  	a = '123';
  }
  show();
  ```

  ​


- 如果变量声明与函数声明是相同的，函数声明会被优先提升

  ```javascript
  show();		//输出：first

  //show()函数与show变量同时存在的情况
  function show(){
  	console.log('first');	
  }

  var show;
  show = function(){
  	console.log('second');
  }
  ```


- 如果有多个同名的函数声明，后面的函数会覆盖前面的函数声明

- 没有var声明的变量是不会进行提升的

  ```javascript
  function show(){
  	console.log(a);
  	a = '123';
  }

  show();		//error
  ```

  ​


- 变量提升的作用域是以大括号来区分的。在这点上，变量提升的作用域与变量作用域链是俩个概念

  ```javascript
  var a = true;
  show();		//undefined

  if(a){
      function show(){
          console.log(123);
      }
  }else{
      function show(){
          console.log(456);
      }
  }

  show();		//123，输出123而非456
  ```



### this
- 单独的this是指向window对象
- 全局函数中的this指向window对象
- 函数调用时加上的new关键字

- call与apply函数调用时的this指向传递进来的对象

```javascript
function show(){
    console.log(this);
}

show.call('what');	//String对象
show.call(null);		//window对象
show.call(undefined);	//window对象
```

- 定时器中的this指向window对象
- 元素绑定事件时所触发函数中的this是指向当前元素对象
- 函数调用时如果绑定了bind，那么函数中的this指向绑定的东西
```javascript
var btn = document.querySelector('input');
btn.addEventListener('click', function(){
    console.log(this);
}.bind(window));
```

- 方法如果被哪个对象调用，this就指向该对象




### 面向对象
JavaScript没有类的概念，它的对象可以认为是一些无需属性的集合。

一般使用new关键字调用函数来生成对象，例如：
```
function Person(){
	this.name = 'zhangsan';
	this.show = function(){}
}
```
上述代码经历几个步骤：
1. 创建一个新对象
2. 将this指向新对象
3. 执行构造函数代码

缺点：在使用new操作符声明一个对象时，每个对象都会有自己的属性和函数，不同对象之间的函数表达式是不同的，这就造成了内存浪费。



### 原型对象(Prorotype)
原型对象是每个函数都拥有的一个单独的对象，它具有以下特点：

- 每个函数都有一个prototype属性，该属性是指向构造函数的原型对象，例如Person函数的原型对象是Person.prototype
- 每个函数的原型对象都有一个constructor属性指向构造函数本身
- 每个实例都有一个隐式的原型对象__proto__，指向构造函数的原型对象
- 原型对象上都有一个隐式的原型对象__proto__

假设Person是函数，Prototype是原型对象，p1是Person函数的实体对象，有以下关系：
- Person.prototype => Prototype
- Prototype.constructor => Person
- p1.__proto__ => Prototype

JavaScript在调用一个属性的时候会先在对象实例自身上寻找，如果该对象没有该属性会顺着隐式原型对象向上寻找，直到寻找到存在的属性或返回null，根据该特性可以这样创建对象。
```javascript
function Person(){
	this.name = 'zhangsan';
}

Person.prototype = function(){
	console.log(this.name);
}

var p1 = new Person();
var p2 = new Person();

//这里是在p1对象上添加了一个show方法，JavaScript会在p1对象上寻找到该方法就返回，而不会去原型对象上面寻找
p1.show = function(){
	console.log('lisi');
}
console.log(p1.show == p2.show);	//false
```



### 原型链

在JavaScript的函数中一切都是对象，最顶级的对象是Object对象，所有对象都是从Object继承而来的。

Object函数由自己的原型对象，该原型对象也有隐式的原型对象__proto__，指向了null，该原型对象也有constructor属性，指向了Object构造函数。

当通过new操作符创建是一个实例对象时，通过该实例对象的隐式原型属性寻找原型对象，再通过原型对象的隐式原型属性寻找其父类的原型对象，从底部通过隐式原型属性依次向上寻找原型对象的过程就叫做原型链。




### 函数表达式
函数的声明有俩种方式：
- 显示声明
- 表达式声明：允许自调用

显示声明的函数如果需要自调用，有俩种方式：
- 在函数声明前添加+ - ~ !四个符号

```
+function Person(){
	//body
}();

-function Person(){
	//body
}();
```

- 将函数转换成表达式再自调用

```
(function Person(){
	//body
})();

//参数的传递
(function(a, b){
	console.log(a);
	console.log(b);
})(1,2);
```



### 闭包

在函数中，将另外一个函数作为返回值，这种函数就叫做闭包。闭包最大的好处在于可以绑定闭包函数外部函数的参数和变量，它的作用域是在定义的时候形成的。

注：闭包函数可以作为变量返回或者不返回。

example：
```
function show(){
	return function(){
		//body
	}
}
```



- 闭包作用域的形式是在定时的时候形成的，通过该方式可以绑定闭包函数外部的变量，例如：

  ```javascript
  function show(){
  	var a = 18;
  	return function(){
  		return a;
  	}
  }

  //以这种方式返回a变量，它是不会被释放的。
  ```



- 不要绑定后续会发生变化的变量

  ```javascript
  function count(){
  	var arr = new Array();

  	for(var i=1; i<=3; i++){
  		arr.push(function () {
  			return i*i;
  		});
  	}

  	return arr;
  }

  var result = count();

  var f1 = result[0];
  var f2 = result[1];
  var f3 = result[2];

  console.log(f1());
  console.log(f2());
  console.log(f3());	//16
  ```

  可以看到上述输出都是16，改进的代码如下：

  ```javascript
  function review_count(){
  	var arr = new Array();

  	for(var i=1; i<=3; i++){
  		arr.push((function (number) {
  			return function () {
  				return number*number;
  			}
  		})(i));
  	}

  	return arr;
  }

  var result = review_count();

  var f1 = result[0];
  var f2 = result[1];
  var f3 = result[2];

  console.log(f1());
  console.log(f2());
  console.log(f3());
  ```



- 闭包是携带状态的函数，且这些状态对外隐藏

  ```javascript
  //example：闭包实现计数器
  var counter = function(){
  	var i = 0;

  	return function(){
  		return i++;
  	}
  }();

  console.log(counter());
  console.log(counter());
  console.log(counter());
  ```





闭包是实现模块化编程一个很重要的概念，由于闭包对外隐藏细节，所以可以将window对象传入函数中，对外提供接口，例如：
```javascript
(function(window){
	funciton show(){
		//body
	}

	//对外提供接口
	var obj = {
		show: show
	};
	
	//定义命名空间
	window.zetao = obj; 
})(window);

//外部调用
zetao.show();
```
通过这种方式就不会勿扰全局变量了。