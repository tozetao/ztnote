### 运算符重载
C++的运算符只能对基本类型的数据进行运算，对于抽象数据类型运算符是无法处理的。
运算符重载能够让运算符像处理基本类型数据一样，对抽象数据类型也能按照重载后的方式来进行运算。

- 对已有的运算符赋予多重的含义
- 是同一运算符作用于不同的数据类型时产生不同的行为

运算符的实现原理是，在程序编译时，会把含运算符表达式转换成对运算符函数的调用，同时会把运算符的操作数作为参数传递给运算符函数，如果运算符被多次重载时，会根据实参的类型决定调用哪个运算符函数。

运算符可以被重载为普通函数，也可以被重载成类的成员函数。

运算符重载的实质是函数重载，语法定义：返回类型 operator运算符(形参表){...}

- 重载为普通函数时，参数的个数是运算符目数
```c
class Complex
{
    public:
        double real;
        double imag;
    
        Complex(double r = 0.0, double i = 0.0)
        {
            real = r;
            imag = i;            
        }
}

Complex operator+(const Complex &a, const Complex &b){
    return Complex(a.real + a.real, b.imag + b.imag);
}

int main()
{
    
    Complex a(1,2), b(3,4), c;
    c = a+b;
}
```
- 重载为成员函数时，参数个数是运算符目数减1
因为重载为成员函数时，会将第一个操作数作为对象去调用重载运算符函数，参数的传递时从第二个操作数开始的
```c
class Complex
{
    public:
        double real;
        double imag;
    
        Complex(double r = 0.0, double i = 0.0)
        {
            real = r;
            imag = i;            
        }

        Complex operator+(const Complex &);
        Complex operator-(const Complex &);
}

Complex Complex::operator+(const Complex &operand2)
{
    return Complex(real + operand2.real, imag + operand2.imag);
}

int main()
{
    Complex x, y(4.3, 8.2), z(3.3, 1.1);
    x = y+z;
    return 0;
}
```
