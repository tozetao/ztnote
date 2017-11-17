### 构造函数
构造函数用于初始化对象，构造函数名与类名相同，它可以有参数但是不能有任何返回值（包括void）；如果在定义类时没写构造函数，编译器会生成一个默认的构造函数的。

- 构造函数是在对象生成时调用，一旦对象实例化完毕，构造函数将不会被调用
```c
class Complex
{
    private:
        double real, imag;
    
    public:
        Complex(double r, double i);    
        test(){cout << "test" << endl;}
};
Complex::Complex(double r, double i)
{
    real = r;
    imag = i;
}

int main()
{
    Complex c(3.123);   //ok
    Complex *p = new Complex(3.324);
    p->test();
    return 0;
}
```

- 一个类可以有多个构造函数
```c
class Complex
{
    private:
        double real, imag;
    
    public:
        Complex();
        Complex(double r, double i);
        Complex(double r);
};

int main()
{
    //多个构造函数形成了重载
    Complex c[2];   //第一个构造函数
    Complex c[3] = {3, 4}   //调用第3个构造函数
    Complex c[2] = {3} //调用第3个和第1个构造函数

    Complex *p = new Complex[2];    //声明2个元素的数组
    delete []p;

    return 0;
}
```
- new关键字
```
int main()
{
    Complex *p = new Complex();
    //new表达式返回的是对象的指针

    Complex c;
    //c是一个实例对象
    
    Comelex c1[3] = {new Complex(), new Complex(3)};
    //c1数组中有3个元素，是3个Complex对象

    Complex *p1[3] = {new Complex(), new Complex(3)};
    //p1指针数组中有3个元素，索引0和1分别指向实际对象的指针，索引2是一个Complex类型的空指针，指向未知。
}
```

### 复制构造函数
复制构造函数的作用用于复制对象，将对象成员变量的值进行依次完整的拷贝。

- 只有一个参数，即对同类对象的引用
- 形式如X::X(X&)或X::X(const X&)，二者二选一，后者能以常量对象作为参数
- 如果没有定义复制构造函数，编译器默认会生成复制构造函数，由默认复制构造函数完成复制功能。

```
class Complex
{
    public:
        double real, imag;
    
        Complex(){}
        Complex(const Complex &c)
        {
            real = c.real;
            imag = c.imag;
            cout << "Copy Constructor called";
        }
}

Complex c1;
Complex c2(c1); //调用默认的复制构造函数，将c2初始化成和c1一样
```
如果自定义了复制构造函数，那么默认的复制构造函数将不存在。
注意：不允许有X::X(X)的构造函数

复制构造函数会在3种情况下发生作用：
- 当用一个对象去初始化另外一个对象时
```c
int main()
{
    Complex c2(c1);
    
    Complex c2 = c1;
    //该语句不是复制语句，是初始化语句
    
    c2 = c1;
    //赋值语句
}
```
- 如果某函数有一个参数是某个类的对象，那么该函数调用时，该类的复制构造函数会被调用
```c
class Foo
{
    public:
        Foo(){}
        Foo(Foo &f)
        {
            cout << "is be called" << endl;
        }
}

void func(Foo obj){}

int main()
{
    Foo f;

    func(f);
    //将会复制一个对象
}
```
- 如果函数的返回值是类的对象时，则函数返回时，该类的复制构造函数会被调用
```c
Foo getFoo()
{
    Foo f();
    return f;
}
int main()
{
    cout << getFoo() << endl;   
    //
}
```

### 类型转换构造函数
类型转换构造函数是为了实现类型的自动转换，它的特点是：
- 只有一个参数
- 不是复制构造函数

对象在发生类型转换时，编译器会自动调用转换构造函数同时生成一个临时对象或变量。
```c
class Complex
{
	public:
		double real, imag;
		
		//该构造函数与类同名，同时又只有一个参数
		Complex(int i){
			real = i; imag = 0;
		}

		Complex(double r, double i){
			real = r; imag = i;
		}
}
int main()
{
	Complex c1(3.3, 1.25);
	
	Complex c2 = 9;
	//c2在这里是进行初始化，而不是赋值，实际调用的是类型转换构造函数，这里不会生成临时的对象

	c1 = 7;
	//这是一个赋值语句
	//由于赋值的类型不一致所以会自动发生类型转换
	//在调用转换构造函数时7会被自动转换成一个临时Complex对象，然后赋值给c1
}
```

### 析构函数 Destructor
析构函数用于销毁对象，它的特点如下：
- 函数名与类名相同
- 函数名前会加 ~ 符号
- 没有参数和返回值
- 一个类只能有一个析构函数

析构函数会在对象销毁的时候调用，如果在对象销毁的时候，只是取消了对象的名称而保留了相应的内存空间的话，那么内存会被大量泄露掉的。

在定义类的时候如果没有定义析构函数，编译器会自动生成一个默认的析构函数，但是这个析构函数本身并没有做什么事情。

```c
class String
{
	private:
		 char *p;
	
	public:
		String(){ p = new char[10]; }
		~String();
};
String::~String()
{
	delete []p;
}
```

如果一个数组是对象数组，在对象数组生命周期结束时，对象数组中每个析构函数都会被调用。
```c
class Ctest
{
	public:
		~Ctest(){ cout << "destructor called;" << endl; }
}
int main()
{
	Ctest c[2];
	cout << "End Main" << endl;
	return 0; 
}
```
- delete运算会导致析构函数调用
```c
int main()
{
	Ctest *ptest;
	ptes t = new Ctest;		//构造函数调用
	delete ptest;	//析构函数调用

	Ctest *ptest = new Ctest[3];	//调用3次构造函数
	delete []ptest;		//调用3次析构函数
}
```

注意：析构函数和构造函数在不同编译器下会有不同的表现，每个编译器调用情况不一致，默认讨论的是C++标准情况下。

```c
int main()
{
	A *p = new A[2];
	A *p2 = new A;
	A a;
	delete []p;
}
```