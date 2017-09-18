## 基本数据类型
在ECMAScript中，数据类型包括null、undefined、number、string、boolean和Object类型；
除了Object类型，其他都是属于基本数据类型。

### 1. typeof操作符
typeof属于运算符的一种，它用于判断变量的类型，返回的判断结果有：
- undefined
- number
- string
- boolean
- object
- function

example：
```javascript
var message = "some string";
alert(typeof message);
alert(typeof 90);

alert(typeof null);	//弹出object，特殊值null被认为是一个空的对象引用
```

注：从技术角度上来讲，function也属于对象的一种，然后函数有一些特殊的属性，因此跟object分开区别是有必要的。


### 2. undefined
undefined类型只有一个值，即特殊的undefined值，如果声明一个变量而没有初始化，这个变量的值就是undefined，例如：
```javascript
var message;
alert(message);	//undefined
```

要注意的是，没有声明的变量和包含undefined值的变量是不一样的，例如：
```javascript
var message;

alert(message);
alert(age);		//产生错误
```

使用没有声明的变量虽然会产生一个error，但是可用用typeof操作符来判断，结果是undefined。

### 3. null
null类型也是只有一个值的特殊类型，null是一个空对象指针；
如果定义的变量确定是对象类型，在初始化时建议设置成null，方便后续判断并且有助于区分undefined和null。

undefined是派生自null值的，在ECMA-262中它们是相等的。
```javascript
alert(undefined == null);	// true
```

### 4. Boolean
Boolean类型只有true和false俩个字面值，注意大小写。

任意类型的变量都能够转换成Boolean值，可以使用Boolean()函数转换，转换规则取决于变量类型和变量的值，规则如下：
- String：非空字符串为true，空字符串""为false
- Number：任何非0数字为true，0或NaN为false
- Object：任何非空对象为true，null为false
- undefined：false

### 5. Number
### 整数
JavaScript使用IEEE754来表示整数和浮点数(双精度)，可以用三种字面形式来表示整数，分别是：
- 10进制：0-9表示整数，不能以0开头
- 8进制：0-7表示整数，以0开头
- 16进制：0-9，A-F表示整数，以0x开头

在进行所有算数计算时，所有以8进制和16进制表示的数字都会被转换成10进制。

### 浮点数
包含小数点且最少含有一位小数的数字被称为浮点数；
在JavaScript中浮点数存储的内存空间比整数大一倍，所以小数点后如果没有任何数字，那么该数值会以整数在保存。

数值范围，ECMAScript的最小数值大约是5e-324，保存在Number.MIN_VALUE，最大值保存在Number.MAX_VALUE，大约是1.79....e+308

### 数值转换
有3个函数能够将非数字转换成数值，分别是：
- Number()：该函数适用于任何数据类型
- parseInt()：适用于字符串转数字
- parseFloat()：适用于字符串转数字

Number()函数的转换规则：
- Boolean：true和false分别对应1和0
- Number：Number类型原样返回
- Null：返回0
- undefined：返回NaN，Not a Number
- String：String比较复杂，要遵守以下规则：
- 如果只包含数字，会被转换为10进制数值，包含正负号，转换时会忽略前导0
- 如果字符串是有效的浮点数格式则转换为浮点数
- 如果字符串是16进制格式则转换成10进制数值
- 如果字符串是空则转换为0
- 凡是不符合上述规则则转换为NaN

parseInt()函数转换规则：
- 默认忽略字符串前面空格，直到找到一个非空格字符串
- 第一个非空字符不是数字或正负号，将返回NaN
- 如果第一个非空字符是正确的，函数将会一直解析，直到解析完所有字符或碰到一个非数字字符
- parsetInt能解析不同进制的数字字符串，解析规则同上

parseFloat()
- 从字符串第一个字符开始解析，始终忽略0和空格，直到解析到一个无效的字符。
- 要注意的是第一个.字符是有效的，第二个则无效
- parseFloat无法解析其他进制的字符串
```javascript
parseFloat('123hello');	//123
parseFloat('12.35.4');	//12.35
parseFloat('0xF');		//NaN
```

### 6. String
String类型表示由0或多个Unicode字符组成的字符序列，用单引号或双引导括起来。
String类型没什么注意的，只要知道其他类型如何转字符串就可以了。

几乎每个值都有自己的toString()方法，该方法会根据变量的数据类型来返回其对应的字符串形式，Number、Boolen、String和Object都有自己的toString()方法，null、undefined没有改方法。
- Number返回对应的数字字符串
- Boolean返回"true"或"false"
- String返回自身的一个副本

String()方法也可以将其他类型转换成字符串，规则如下：
- 如果该变量拥有toString()方法，则调用该方法
- 如果值是null，返回"null"
- 如果值是undefined，返回"undefined"
