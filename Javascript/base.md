## 数据类型
在ECMAScript中，数据类型包括null、undefined、number、string、boolean和Object类型，

### Object类型
ECMAScript中的对象就是一组数据和功能的集合，对象可以使用new操作符跟要创建的对象类型的名称来创建。

创建Object类型的实例并为其添加属性和方法就可以创建自定义对象，例如：
```javascript
var o = new Object();
//...
```
在ECMAScript中，Object是所有对象的父类，所有的对象都继承Object所具有的属性和方法，Object的每个实例都具有下列属性和方法：
- constructor：保存用于创建当前对象的构造函数，例如前面的例子中，构造函数就是Object().
- hasOwnProperty()：检查给定的属性是否在对象实例中，不是在实例的原型中。
- isPrototypeOf(object)：检查传入的对象是否是传入对象的原型
- propertyIsEnumerable(propertyName)：用于检查给定的属性是否能够使用for-in语句。
- toString()：返回对象的字符串表示
- valueOf()：返回对象的字符串、数值或布尔值表示，通常与toString()方法一致。





## 基本类型与引用类型
因为对象的存在，所以将数据类型分为基本类型和引用类型。

基本类型的值是null、undefined、Number、String、Boolean，这5种类型是按值访问，因为操作的是保存在变量中实际的值。

引用类型的值是指针，它是一个内存地址，指向存储在堆中的对象；在操作一个引用类型时，实际上是通过该引用的地址去内存中操作对象，例如对属性的操作和方法的调用。

### 1. 赋值
基础类型的变量在赋值时，是对变量的值进行一次拷贝再赋予；引用类型的变量在赋值时，是对对象的引用进行一次拷贝，赋值时是将对象的引用赋予变量。

因此基础类型的变量赋值后不会相互影响，而引用类型赋值后的对俩个变量的操作都会互相影响。

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

### 2. 传递参数
EXMAScript中，所有的参数都是按值传递的，也就是会把函数外部的值赋值一份给函数内部的参数，就跟把值从一个变量复制到另外一个变量一样。

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

### 3. 类型检测
typeof操作符用于检测基础类型，instanceof用于检测实例对象的类型。
```javascript
alert(person instanceof Person);	//true
alert(person instanceof Object);	//true
```

## 作用域
在ECMAScript中，要理解变量和函数的作用域，要先理解执行环境（execution content）这个概念。

- 执行环境：一个函数体的内部称之为执行环境，每个执行环境都会有对应的变量对象，环境(函数)中创建的变量和函数都会附加在这个变量对象上。
全局执行环境：在web浏览器中，全局的执行对象就是window对象。

- 作用域链：当一个函数执行时，会创建变量对象的一个作用域链，它决定了如何去访问变量和函数，访问规则是：内部环境可以通过作用域链访问所有的外部环境中的变量和函数，但是外部环境不能访问内部环境中的任何变量和函数。
```javascript
var color = 'blue';

function changeColor(){
	color = 'yellow';
}
changeColor();
console.log(color);
// 通过作用域链改变外部作用域链中变量的值
```

- 没有块级作用域
```javascript
if(true){
	var num=10;
}
alert(num);	//10
```
- var声明的变量会被添加到最近的执行环境中
```javascript
function add(num1, num2){
	var sum = num1+num2;
	return sum;
}
var result = add(10, 20);
console.log(sum);	//拋出异常，删除sum的var关键字，就不会报错
```

- 未用var关键字的变量，读取或写入时会从当前作用域链向上搜索，如果在局部执行环境中找到，则停止搜索，否则会追溯到全局执行环境中，如果仍没有找到，则意味没声明该变量。
```javascript
var color = 'blue';
function getColor(){
	//var color = 'red';	//取消注释返回red
	return color;
}

alert(getColor());	//blue	
//从当前作用域链的变量对象中寻找变量的
```


## Function类型
函数其实是Function类型的实例，跟其他引用类型一样具有属性和方法，函数名实际上是指向函数对象的指针，例如：
```javascript
function sum(){}

var sum = function(){}
// 上述的俩种声明方式是一致的。
```

### 1. 函数声明与函数表达式
解释器在执行环境中加载数据时，对函数声明和函数表达式是不一样的，解释器会读取函数声明，方便执行代码调用，至于函数表达式，必须等待执行到所在的代码行，才会真正被解析，例如：
```javascript
console.log(getColor());

// function getColor(){ return 'red'; }	//函数声明

var getColor = function(){ return 'red'; }
//函数表达式，会报错

//只有在执行到函数表达式时，解释器才会解析
```
### 2. arguments
类数组对象，包含着传入函数中的所有参数，它还有一个callee属性，是一个指针，指向拥有这个arguments对象的函数
```javascript
function factorial(num){
	if(num<=1)
		return 1;
	return num + arguments.callee(num-1);
}
```
### 3. this
引用函数以执行的环境对象，也可以说是this值；在函数执行前this的值是不确定的，只有在调用函数时才能确定执行环境，确定this的值。
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

如果在全局环境中执行函数，this指向window，如果是一个具体的对象，this指向的是这个对象，因为在全局环境中，变量和函数都是属于window对象的，在执行一个全局函数时，其实是用window对象来调用该函数，所以this指向window。

注：函数名只是一个包含指针的变量而已，它指向的是函数在内存中的地址。

### 4. apply()与call()
这俩个方法用于在特定的作用域中调用函数，实际上等于设置函数体内的this对象的值。
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