OOP语言的特点是拥有类的概念，JavaScript中没有类，JavaScript的对象可以理解成是一系列无需属性的集合。


### 属性的特性

ECMA5定义了对象属性的特性，分别如下：
- Value：
- Writable：能否修改属性的值，默认true
- Enumerable：表示属性是否允许for-in循环，默认true
- Configurable：


### 构造函数创建实例
```js
function Person(){
	this.name = 'zhangsdan';
	this.say = function(){
		alert(this.name);
	}
}

var p1 = new Person();
```

使用new操作符创建了一个对象，上述代码经历几个步骤：
1. 创建一个新对象
2. 将构造函数的作用域赋值给新对象，即this指向新对象
3. 执行构造函数代码

如果不使用new操作符，在调用一个函数的时候是将作用域指向windows对象的。

缺点：在使用new操作符声明一个对象时，在函数内部声明的方法都是一个个不同的实例对象，这回造成内存浪费。


### 原型对象
每个函数都有一个prototype属性，它指向了函数的原型对象，所有的原型对象都会有一个constructor属性，该属性指向了prototype属性所在函数的指针。

例如： Person.prototype.constructor == Person

注：你无法直接使用实例对象去访问prototype，ECMAScript5提供了Object.getPrototypeOf()方法来获取该属性。




