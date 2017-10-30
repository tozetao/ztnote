### 指针 
什么是指针
- 变量地址、变量名和变量值称为变量的三要素，通常把某个变量的起始地址称为指向该变量的指针。

取地址运算符
- &：&是取地址运算符，它用于查看一个变量的起始地址
```c
int c = 10;
cout << &c << endl;
// 查看一个变量的内存地址

cout << sizeof(&c) << endl;
// 查看该内存地址所占用的内存空间
```
指针运算符
- *：指针运算符能够通过内存地址来直接操作一个变量。
```c
int c = 20;
cout << *&c << endl;
cout << c << endl;
//俩种访问变量的方式是等价的
```

指针变量
- 用于存放指针的变量叫做指针变量
- 基类型：指针变量指向的变量的类型，例如：int *pointer，int就是基类型
```c
int c = 10;
int *pointer;

pointer =& c;

cout << *pointer << endl;
//*pointer是所指向的存储单元的内容，即是变量c
```

example1：
```c
#include <iostream>
using namespace std;

int main()
{
    int iCount = 50;
    int * iPtr =& iCount;
    iPtr = 23;
    
    cout << iCount << endl;
    cout << iPtr << endl;   //16进制的内存地址
    cout << &iCount << endl;    //同上
    cout << *iPtr << endl;      //23
    cout << &iPtr << endl;      //iPtr指针变量的内存地址
}
```

注：指针和指针变量是俩个概念，指针是内存地址，指针变量就是一个普通的变量，用于存放内存地址

### 指针运算符的优先级
指针与其他运算符的优先级如下：
1. 后置++ --
2. 前置++ --、逻辑非(!)、指针运算符(*)、取址运算符(&)
3. 算数运算符
4. 关系运算符
5. 逻辑与&&、逻辑或||
6. 赋值运算符(=)

优先级顺序从上到下.

example:
```c
&*ponter = &(*)pointer
*&pointer = *(&)pointer

(*pointer)++ != *pointer++
```

### 指针变量的自增自减
```
int a = 10;
int *p =& a;

cout << p << endl;
p++;
cout << p << endl;
```
一个指针变量如果进行自增或自减，它会在原有的内存地址上，跨域该指针变量类型占据的字节长度个单位的内存空间，指向这个新的内存地址。

上面的例子中，假设a变量的内存初始空间是0X0012FF10，指针变量p自增后的内存地址是在0x0012FF15，跨域了4个字节。


### 数组与指针
- 数组变量名相当于第一个元素的指针；注意它是一个常量，无法被修改。
- 指针变量做数学运算时，是通过该指针变量的基类型的字节长度作为单位来运算的；例如int型数组pointer+1，指的是索引1的内存地址

example：
```c
int main()
{
	int a[5] = {1,2,3,4,5};
	int *pointer = null;
	
	pointer = a;
	
	cout << a << endl;		//数组的起始地址，指向索引0元素的内存地址
	cout << *a << endl;		//1
	cout << &a[0] << endl;	//位于索引0的内存地址
	cout << a[0] << endl;	//1

	a++;	//error，无意义的，数组名是变量，无法自增或自减
	
	cout << pointer + 1 << endl;	//访问数组索引1元素的内存地址
	cout << *(pointer + 1) << endl;	//访问数组索引1元素的值
	cout << pointer[1] << endl;		//访问数组索引1元素的值
}
```


example2：
```c
int main()
{
	int a[5] = {1, 2, 3, 4, 5};
	int *p = null;
	
	cout << a << endl;
	cout << *p << endl;
	cout << *p++ << endl;	//1
	cout << *p++ << endl;	//2
	
	//后置++先执行，由于后置++的运算符特性，p会先被指针运算符执行，所以先输出*p，然后p再自增
}
```

example3：
```c
int a[10], *p=null, *q=null;

//赋值10个数字
for(p=a; p<a+10; p++)
	cin >> *p;

p=a;	//指向数组的起始
q=a+9;	//指向数组的尾部
```

### 指针与二维数组
```c
int main()
{
	int a[3][4] = {1,2,3,4,5,6,6,7,8,9,10,11};
	//a 等价于 &a[0]
	//在这里变量名a指向了一个存储4个int型元素的一维数组的内存地址

	int (*p)[4] = a;
	//定义一个存储4个int型元素的一维数组指针
	//基类型是int型数组
	
	cout << p+1 << endl;	//指向a数组中索引1的一维数组，等价于&a[1]
	cout << *(p+1) << endl;	//等价于a[1]，是第2个子数组中第1个元素的指针
	
	cout << *(*(p+1)+1) << endl;	
	//(*(p+1)+1)等价a[1]+1，所以 于a[1][1]

	cout << p[1][1] << endl;
}
```



### 指针与字符串
指向字符串的指针
```c
char a[10];
char *p;

p=a;
//指向字符串a的指针
```

example1: cout对字符串指针的处理
```c
int main()
{
	int a = 5;
	int *pa =& a;
	
	int b[6] = {1,2,3,4,5,6};
	int pb =& b;
	
	char c[]6 = {'a', 'b', 'c', 'd', 'e'};
	char *pc =& c;
	
	cout << a << endl;
	cout << pa << endl;

	cout << b << endl;
	cout << pb << endl;

	cout << c << endl;	//输出字符串内存
	cout << pc << endl;	//输出字符串内容，这是cout内部的处理
}
```


example2: 
```c
char buffer[6] = {'AB', 'B', 'C', '\0'};
char *pc;
pc = "hello";

cout << pc << endl;	//hello
pc++;	//将pc指针移动一位

cout << pc << endl;	//ello
cout << *pc << endl;	//e


pc = buffer;
//将pc指针指向buffer
//可以将pc指针指向其他的内存地址，但是不能用字符串直接赋值，如果是赋值的方式，意味着对"hello"字面常量进行修改，而常量是不允许修改的。
```