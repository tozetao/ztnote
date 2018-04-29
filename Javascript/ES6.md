### let

JavaScript的变量作用域是受限于函数定义的，在if或for循环语句块中是无法定义具有局部作用域的变量的，为了解决局部作用域的问题，ES6引入了新关键字let，用于代替var声明一个局部作用域的变量。

```javascript
'use strict';

function foo() {
	var sum = 0;
	for (let i=0; i<100; i++){
		sum += i;
	}

	i+=1;	//error
}

foo();
```



### const

ES6新关键字，用于定义常量。与let相同的是，声明定义的常量也是块级作用域。

```javascript
const PI = 3.14;

PI = 3;				//不会报错，但是赋值是失败的

console.log(PI);	//3.14
```



### 解构赋值

解构赋值允许同时对一组变量赋值。

对数组进行解构赋值：

- 对多个变量进行解构赋值

  ```javascript
  'use strict';
  var [x, y, z] = ['first', 'second', 'es6'];
  ```

  注：对数组进行解构赋值时，多个变量需要用[]括起来。



- 允许嵌套，嵌套层次与位置要保持一致

  ```javascript
  var [x, [y, z]] = ['first', ['second', 'es']]

  var [, , x]     = ['hello', 'java', 'net'];		//忽略前俩个元素，赋值第三个元素
  ```



使用对象进行解构赋值：

- 对多个属性进行解构赋值

  属性名要相对应，如果赋值的属性不存在，值是undefined

  ```javascript
  var person = {
  	name    : 'zhangsan',
  	age     : 20,
  	address : {
  		province: 'shangxi',
  		city    : 'baoji'
  	}
  };

  var {name, age, fuck, address: {city, zip}} = person;

  console.log(name);
  console.log(city);
  console.log(zip);
  console.log(fuck);
  ```


- 指定解构赋值的变量名

  ```javascript
  var person = {
      name: 'zhangsan',
      passport: 'GFJDKFJ'
  };

  let {name, passport:id} = person;
  console.log(id);
  ```


- 使用默认值

  ```javascript
  var person = {
      name: 'zhangsan',
      age : 29
  };

  var {name, single=true} = person;
  ```






### Arrow Function

箭头函数，它相当于匿名函数，并且简化了定义的方式。

examples:

```javascript
var f  = x => x*x;
//只有一个表达式，不用小括号()和大括号{}

(x, y) => x * y;

var sum = (x, y, ...rest) => {
    var i, sum = x+y;
    
    for(var i=0; i<rest.length; i++){
        sum += rest[i];
    }
    
    return sum;
}
```



