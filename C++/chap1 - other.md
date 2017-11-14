## 函数指针
程序运行期间，每个函数都会占用一块连续的内存空间，而函数名就是该内存空间的起始地址。

函数指针便是把函数的起始地址赋值给一个指针变量。

定义：类型名 (* 指针变量)(参数类型...)
```c
void PrintMin(int a, int b){}

int main()
{
	int a = 5;
	int b = 6;
	void (* p)(int,int);
	
	p = PrintMin;
	p(a, b);
	return 0;
}
```

example:
```c
int MyCompare(void const * elem1, void const * elem2)
{
	unsigned int *a, *b;
	a = (unsigned int *) elem1;
	b = (unsigned int *) elem2;
	return *a - *b;
}

int main()
{
	unsigned int an[NUM] = {1,2,3,4,4,5,6};
	qsort(an, NUM, sizeof(unsigned int), MyCompare);
}
```

## 引用
定义：类型名 &引用名 = 变量名
某个变量的引用等价于这个引用，相当于该变量的一个别名。
```c
int n = 4;
int &r = n;
//定义了引用r，并初始化为n
//r的类型是int &，整型引用
```
注意：
- 定义引用时一定要将其初始化成引用某个变量。
- 初始化后将会一直引用该变量，不会再引用别的变量。
- 引用只能引用变量，不能引用常量和表达式

```c
double a = 4, b = 5;
double & r1 = a;
double & r2 = b;
r1 = 10;
cout << a << endl;

r1 = b;	//将b的值赋值给r1，并不会发生引用
cout << a << endl;
```

### 常引用
- 定义引用时，前面加const关键字，即为常引用，例如：const int & r = 10，r的类型是const int %(常整型引用)
- 常引用是不能通过引用去修改其引用的内容，但是可以更改常引用去引用哪个变量。


常引用和非常引用的转换
const T &和T &是不同的类型，T&的引用或T类型的变量可用用来初始化const T &类型的引用。
const T的常变量和const T &的引用不能用来初始化T &类型的引用，除非强制类型转换。

## const关键字
- 定义常量
- 定义常量指针
example：常量指针无法修改其引用的内容
```c
int main()
{
	int n, m;
	
	cont int * p = &n;

	*p = 5;
	//error
	
	n = 4;
	//ok
	
	p =& m;
	//ok
}
```
example：不可以把常量指针赋值给非常量指针，反过来可以
```c
int main()
{
	const int * p1;
	int * p2;
	
	p1 = p2;	//ok
	p2 = p1;	//error
	p2 = (int *)p1; //ok	
}
```
example：函数参数为常量指针时，可避免函数内部不小心改变参数指针所指地方的内容
```c
void MyPrint(const char *p)
{
	strcpy(p, "this");
	printf("%s", p);
}
```

## 动态内存分配

### new运算符
new运算符可以实现动态内存的分配

- 分配一个变量，语法：P = new T
T是任意类型名，P是一个指针，类型为T*，该语句将会动态分配出一片大小为sizeof(T)字节的内容空间，并将内存空间的首地址赋值给P。
```c
int * pn;
pn = new int;
*pn = 5;
```
- 分配一个数组，语法：P = new T[n]
T是任意类型，P是类型为T*的指针，n为分配的数组元素的个数，可以是整型或者整型表达式。
该语句会动态分配出一片大小为n * sizeof(T)字节的内存空间，并将起始地址赋值给指针p。
```c
int * pn;
int i = 10;
pn = new int [i*10]
pn[0] = 100;
pn[100] = 30;	//编译成功，运行报错，数组将会越界。
```
- new运算符的返回类型
new T和new T[n]，俩个表达式的返回类型都是T *;

### delete运算符
delete运算符用来释放动态分配的内存空间
- delete 变量
语法：delete 指针，该指针必须是指向new出来的空间。
```c
int * p = new int;
*p = 5;
delete p;
delete p;	//error，一片空间不能被delete多次
```
- delete数组
语法：delete [] 指针，该指针必须是new出来的数组
```c
int * p = new int[20];
p[0] = 1;
delete [] p;
```

### 内联函数
函数调用是有时间开销的，如果函数本身只有几条语句，执行非常快，而且函数被执行多次，相比之下调用函数所产生的这个开销会显得比较大。

内联函数用于减少函数调用的开销，编译器调用内联函数时，是将整个函数代码直接插入到调用语句处，而不会产生调用函数时的语句。

定义：在函数名前添加inline关键字
```c
inline int max(int a, int b)
{
    if(a>b)
        return a;
    else
        return b;
}
```

注：内联函数会使可执行程序的体积增加。

### 函数重载
一个或多个函数重名，但是参数类型或参数个数不相同，这种叫做函数重载，
编译器会根据参数类型和个数来选择要调用的函数，例如：
```c
int max(int a, int b);
double max(double a, double b);
int max(int a, int b, int c);
```

### 函数缺省参数
在C++中函数的参数是有默认值的。
定义函数的时候可以让最右边的连续若干个参数有默认值，那么在调用函数的时候，则相应的参数可以不传值，该参数值是默认值。
```c
void func(int x1, int x2=34){}
```