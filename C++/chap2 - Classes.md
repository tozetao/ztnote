### 类
类包含成员变量和成员函数，可以将类当做是带有成员变量和成员函数的结构体，类与其他数据类型是相同的，不同的是类的类型是自定义的。

类的定义：
```c
class CRectangle
{
    public:
        int w,h;
    
    void Init(int w, int h)
    {
        w = w_;
        h = h_;
    }
};
//注：必须有分号

int main()
{
    int width = 50, height = 100;

    CRectangle r;
    //声明CRectangle类的一个对象
    
    r.Init(width, height);
    //调用对象的方法

    r.w = 34;
    //访问对象的变量
}
```
### 对象的大小
- 对象的内存空间等于所有成员变量之和的大小，上述例子CRectangle类的对象是sizeof(CRectangle)=8个字节大小。
- 每个对象都有自己的存储空间，所以一个对象的某个成员变量被改变是不会影响其他对象的。

### 对象的运算
- 对象之间可以用=赋值
- 对象不能用比较运算符进行运算，除非这些运算符进行了重载


### 对象的访问
- 通过变量进行访问
```c
CRectangle r1, r2;
r1.w = 5;
r2.Init(3, 4);
```
- 通过指针进行访问
```c
CRectangle r1, r2;
CRectangle * p1 =& r1;
CRectangle * p2 =& r2;

p1->w = 5;
p2->Init(3, 4);
```
- 通过指针访问
```c
CRectangle r2;
CRectangle & r1 = r2;

r1.w = 15;  //r1相当于r2的一个变量名
```

### 单独定义成员函数体
在定义一个类的时候，函数体和类的定义是可以分开写的。
example:
```c
//定义类
class CRectangle
{
    public:
        int w,h;
        int Area();
        int Perimeter();
};
//定义类的函数体
int CRectangle::Area(){}
```

### 类成员的访问范围
- private：指定私有成员，只能在成员函数内被访问
- public：指定公有成员，可以在任何地方被访问
- protected：指定保护成员，允许继承的成员访问

如果在类中没有定义一个成员的访问范围，默认是私有成员。

类的成员函数内部可以访问：
- 当前对象的全部属性，函数
- 同类其他对象的全部属性和函数

类的成员函数以外的方法只能访问类的公有变量。

### 内联成员函数
语法：inline function_name
```c
class B
{
    inline void func1();
};
void B::func1()
{}
```

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