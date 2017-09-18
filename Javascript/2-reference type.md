## 引用数据类型
### 1. Object类型
对象即引用类型，ECMAScript中的对象就是一组数据和功能的集合，对象可以使用new操作符跟要创建的对象类型的名称来创建，例如：
```javascript
var o = new Object();
//...
```

引用类型的值是指针，它是一个内存地址，指向存储在堆中的对象；在操作一个引用类型时，实际上是通过该引用的地址去内存中操作对象，例如对属性的操作和方法的调用。

### 2. 赋值的区别
基础类型的变量在赋值时，是对变量的值进行一次拷贝再赋予；引用类型的变量在赋值时，是将对象的引用(指针)赋予变量，而为对象添加属性时，会根据对象的引用(实际内存地址)去操作的是实际对象。

根据这一特性，基础类型的变量拷贝赋值后不会相互影响，而引用类型赋值后的对俩个变量的操作都会互相影响。

example：
```javascript
var num1 = 5;
var num2 = num1;
//俩个变量分别存储着5，对num1的操作不会影响到num2

var obj1 = new Object();
var obj2 = obj1;
obj1.name = 'lisi';
alert(obj2.name);	//lisi
//引用类型的操作是会互相影响的
```

### 2. 按值传递
EXMAScript中，所有函数或方法的参数都是按值传递的，也就是会把函数外部的值赋值一份给函数内部的参数，就跟把值从一个变量复制到另外一个变量一样。

```javascript
function addTen(num){
	num+=10;
	return num;
}

var count=20;
var result = addTen(count);
alert(count);	//20
alert(result);	//30
//基础类型的传递是不会影响函数外部变量的


function setName(obj){
	obj.name = 'micrl';
}
var person = new Object();
person.name = 'zhangsan';
setName(person);
alert(person.name);		//'micrl';

//引用类型也是值传递，但是拷贝的引用指向的是同一个对象，所以会影响到函数外的对象
```

引用类型传递时也是值传递，而不是按引用传递，如果按引用传递，是将变量的地址传递给参数，操作时就是操作这个变量的地址了。


按引用传递的概念：按引用是指在将变量传递给函数或方法时，传递是的变量本身的指针，例如在PHP中是func_name(&$name)，函数是直接操作传递进来的变量所在的内存地址的，而不是变量的一份拷贝。

### 3. 类型检测
待定


## Object对象
在ECMAScript中，Object是所有对象的父类，所有的对象都继承Object所具有的属性和方法，Object的每个实例都具有下列属性和方法：

- constructor：保存用于创建当前对象的构造函数，例如前面的例子中，构造函数就是Object().
- hasOwnProperty()：检查给定的属性是否在对象实例中，不是在实例的原型中。
- isPrototypeOf(object)：检查传入的对象是否是传入对象的原型
- propertyIsEnumerable(propertyName)：用于检查给定的属性是否能够使用for-in语句。
- toString()：返回对象的字符串表示
- valueOf()：返回对象的字符串、数值或布尔值表示，通常与toString()方法一致。

### 1. 声明
Object可以用俩种方式来声明，例如：
```javascript
var obj = new Object();

var obj = {
	name: 'name',
	age: 25
};
```

### 2. 访问属性
通过.或者中括号[]来进行访问。
```javascript
var obj = {
	name: 'name',
	age: 25
};

console.log(obj.name);
console.log(obj['name']);
```

## Function类型
函数实际上是对象，Function类型的对象，跟其他引用类型一样具有属性和方法，
函数名实际上是指向函数对象的指针。

记住：函数是对象，函数名是指针。

例如：
```javascript
function sum(){}

var sum = function(){}
```
上述的俩种声明方式是一致的。

### 1. 函数声明的解析顺序
解释器在执行环境中加载数据时，对函数声明和函数表达式的解析是不一样的。

在代码中声明的函数，解析器会优先解析，方便后续代码调用，对于表达式只有在代码真正执行时才会被解析调用(包括函数表达式)

例如：
```javascript
console.log(getColor());
function getColor(){ return 'red'; }
```
函数声明的前后顺序不影响代码执行

```javascript
console.log(getColor());
var getColor = function(){ return 'red'; }
```
函数表达式，会报错，因为下一行代码没有被执行。

### 2. arguments
arguments是Function对象的属性，它包含着传入函数中的所有参数，要注意的是argtments属性还有一个callee属性，callee是一个指针，指向拥有这个arguments对象的函数
```javascript
function factorial(num){
	if(num<=1)
		return 1;
	return num + arguments.callee(num-1);
}
```
### 3. this
this属性指向执行函数名的所属对象，在函数执行前this的值是不确定的，只有在调用函数时才能确定执行对象，确定this的值。

```javascript
window.color = 'red';
var o = {'color': 'blue'}

function sayColor(){
	console.log(this.color);	
}

sayColor();
o.sayColor = sayColor;
o.sayColor();
```

如果在全局环境中执行函数，this指向window，因为在全局环境中，变量和函数都是属于window对象的。如果在一个具体的对象执行函数，this指向的是这个对象，

注：函数名只是一个包含指针的变量而已，它指向的是函数在内存中的地址。

### 4. apply()与call()
每个函数都有俩个非继承而来的方法，apply()和call()，
这俩个方法能改变函数名的所属对象，第一个参数是函数名的所属对象，实际上等同于函数体内的this对象的值，第二个参数是参数数组。

说明：
- apply()：该方法接收俩个参数，一个是在其中运行函数的作用域，另一个是参数数组
- call()：与apply()函数一样，第一个参数接收this对象，变化的是其他参数都直接传递给函数

apply()与call()真正强大的地方是用于改变函数的作用域，例如：
```javascript
window.color = 'red';
var o = {'color': 'blue'}

function sayColor(){
	console.log(this.color);	
}

sayColor();

sayColor.call(this);
sayColor.call(window);
sayColor.call(o);
```