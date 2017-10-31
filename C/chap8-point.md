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
假设定义了一个数组：int a[5] = {1,2,3,4,5};

- 数组变量名相当于第一个元素的指针，a等价于&a[0]，注意a是一个常量，无法被修改。
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
void lop
{
    int a[10], *p=null, *q=null;
    
    //赋值10个数字
    for(p=a; p<a+10; p++)
    	cin >> *p;
    
    p=a;	//指向数组的起始
    q=a+9;	//指向数组的尾部
}
```

- &取地址符对一个变量使用时，它将返回指向该变量的一个指针
&a表示指向整个数组a的指针，与a不同的是&a的管辖范围提升了一级，在运算时是以整个数组字节长度作为单位的。
- *指针运算符对一个变量使用时，如果该变量是指针的话，将返回指针变量的值
*a表示数组第一个元素的值，它相当于将变量的范围降级了，所以*(&a)返回指向数组元素的第一个指针

example：
```c
int main()
{
    int a[4] = {1,2,3,4};
    
    //a与&a的不同之处
    
    cout << a << endl;      //返回指向数组第一个元素的指针
    cout << a+1 << endl;
    cout << &a << endl;     //返回指向数组a的指针
    cout << &a+1 << endl;   //以整个数组字节长度为单位+1

    cout << *(&a) << endl;    //返回&a的值，即指向数组a的指针
    cout << *(&a)+1 << endl;    //
}
```
数组名a和数组名&a不同之处在于指针管辖的范围不同，也就是说&a是将整个数组的字节长度作为单位来进行数学运算。

### 指针与二维数组
假设定义了一个二维
数组a[3][4] = {1,2,3,4,5,6,7,8,9,10,11,12};

- &a是指向整个二维数组的指针
- a是指向二维数组中第一个数组元素的指针，a变量的管辖范围是第一个数组
- a[0]是指向二维数组中第一个数组元素中的第一个元素的指针，它的管辖范围是元素本身
- a[0][0]是指向二维数组中第一个数组元素中第一个元素的值

得到结论：
- a与&a[0]等价；
- a[0]与&a[0][0]等价
- a[0]与*a等价；
- a[0][0]与**a等价；

规律：
- 数组名相当于指向数组第一个元素的指针
- &E相当于把E的管辖范围上升了一个级别
- *E相当于把E的管辖范围下降了一个级别

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


### 指针与函数
- 指针做函数参数
example:
```c
void rank(int *q1, int *q2)
{
	int temp;
	if(*q1 < *q2)
	{
		temp = *q;
		*p = *q;
		*q = temp;
	}
}

int main()
{
	int a,b,*p1,*p2;
	cin >> a >> b;
	p1 = &a;
	p2 = &b;
	rank(p1, p2);
	cout << a << ': ' << b << endl;
}
```

- C++会将函数的形参数组名作为数组指针变量来处理
```c
//将形参数组名作为指针变量来处理
int sum(int array[], int length)
{
    for(int i=0; i<10-1; i++)
    {
        *(array+1) = *array + *(array+1);
        array++;
    }
    return *array;
}

vomd main()
{
    int a[10] = {1,2,3,4,5,6,7,8,9,10};
    cout << sum(a, 10) << endl;
}
```

- 指向符号常量的指针
```c
//定义常量指针，限制指针实参的功能，避免影响外部实参变量的值
int sum(const int array[], int length)
{
    *(array+1) = *array + *(array+1);
    //error，由于是常量，无法修改值 
}

int main()
{
    int a = 10;
    //定义符号常量指针
    const int *p = &a;
    
    
    const int c=15;
    const int d=27;
    int e=39;
    
    const int *p = &a;  //允许
    p = &d;    //允许
    *p = 18;    //error
    
    p = &e;     //允许
    *p = 99;    //error 
    return 0;
}
```

- 指针做函数返回值
```c
//int (*arr)[4]，这种方式的定义也可以
int *get(int arr[][4], int n, int m)
{
    int *pt;
    pt = *(arr+n-1)+m-1;
    return (pt);
}

int main()
{
    int a[4][4] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
    int *temp = get(a, 2, 3);
    cout << *temp << endl;
}
```

example：局部变量的生命周期
```c
int *getvalue1()
{
    int value1=20;
    return &value1;
}

int main()
{
    int *p = null;
    p = getvalue1();
    cout << *p << endl; //未知
}
```
由于value1变量是局部变量，在getvalue1()函数调用完毕后就会被释放，所以会输出未知的值。如果想要在函数内返回的指针变量能够使用，可以返回定义在文件中的全局变量，全局变量的生命周期是在整个文件中的。

- 静态局部变量：函数中定义的变量的值不会随着函数的调用结束后而消失，即其占用的存储空间不会被释放，在下一次函数调用时仍然可以使用
```c
void function()
{
    int a = 0;
    //定义一个静态局部变量
    static int b = 0;

    a = a + 1;
    b = b + 1;
    cout << "a = " << a << endl;
    cout << "b = " << b << endl;
}

int main()
{
    for(int i=0; i<5; i++)
    {
        function();
        cout << "call again" << endl;
    }
    return 0;
}
```