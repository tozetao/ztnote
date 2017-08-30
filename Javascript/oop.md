## 面向对象
ECMAScript没有类的概念，它把对象认为是无需属性的集合，属性的值可以是基础数据类型，对象或函数，简单的可以把ECMAScript认为是键值对的对象。

### 声明
- new关键字声明
```javascript
var person = new Object();
person.name = 'zhangsan';
```
- json对象的声明方式
```javascript
var person = {
	'name' : 'zhangsa'
}
```

ECMA5定义了属性的一些特性，包括：
- Configurable：能够通过delete删除属性而重新定义，默认true
- Enumerable：能够通过for-in循环属性，默认true
- Writable：能否修改熟悉的值
- Valee：包含这个属性的数据值，默认undefined

要修改对象属性的特性，必须通过Object.defineProperty()方法，例如：
```javascript
var person = {};
Object.defineProperty(person, 'name', {
	writable: false,
	value: 'hello'
})
//定义属性的值，并使其不可写		非严格模式下，忽略赋值
```

### 构造函数式的创建
在对一个函数使用new关键字时，那么该函数就是构造函数，构造函数与普通函数是一样的。
```javascript
function Person(name, age){
	this.name = name;
	this.age = age;
	this.sayName = function(){
		alert(this.name);
	}
}

var p1 = new Person('zhangsan', 20);
console.log(p1.name);
var p2 = new Person('22222', 30);
console.log(p2.name);


Person('lisi', 35);		//将函数添加到window对象中
console.log(window.name);

var tmp = new Object();
Person.call(tmp, 'p2name');		//改变函数的作用域链
console.log(tmp.name);
```
对象创建流程：
- 创建一个新的对象
- 将构造函数的作用域赋值给