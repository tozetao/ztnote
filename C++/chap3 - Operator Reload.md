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


### 赋值运算符重载
如果要将基本类型赋值给对象类型的话，需要对赋值运算符进行重载。

- 赋值运算符只能重载为成员函数，无法重载为普通函数。
```c
//一个长度可变的字符串类
class String
{
    private:
        char * str;

    public:
        String():str(null){}
   
        const char * c_str()
        {
            return str;
        }
   
        char * operator=(const char *s)
        {
            if(str){
                delete []str;
            }

            if(s){
                str = new char[strlen(s)+1];
                strcopy(str, s);
            }else{
                str = null;
            }
            return str;
        }
        
        ~String(){
            if(str) delete []str;
        }
}

int man()
{
    String s;
    s = "hello world";
    //相当于s.operator("hello world");

    cout << s.c_str() << endl;

    String s2 = "qwer"; //error，这是声明语句，并不是赋值语句，所以会报错。
}
```

- 浅拷贝：逐个字节的复制工作
```c
int main()
{
    String s1, s2;
    s1 = "this";
    s2 = "that";
        
    s1 = s2;
}
```
上面的代码中，s2赋值给s1中调用了赋值重载函数，这里只是一次简单的字节复制工作，俩个对象都是指向相同的指针，这样当对象都被销毁时就会调用俩次析构函数去销毁同一片内存空间俩次，这会造成程序的异常的。

- 深拷贝：将一个对象中指针指向的内容，复制到另外一个对象指针成员指向的地方
```c
//要完成深拷贝，添加下面的成员函数
String & operator = (const String &s)
{
    //避免自己赋值给自己
    if(str == s.str) return *this;
    
    if(str) delete []str;
    str = new char[strlen(s.str) + 1];
    strcpy(str, s.str);
    return *this;
}
```

缺陷：上述String类的复制构造函数也会发生浅拷贝现象，为了防止多次释放同一块内存空间，需要进行深拷贝。

### 运算符重载为友元


example：长度可变的整型数组类
```c
class CArray
{
	int size;
	int *ptr;
	
	public:
		CArray(int s = 0);
		Carray(CArray &a);
		~CArray();

		void push(int v);	//

		int length(){
			return size;
		}
		
		//运算符重载
		CArray & operator=(const Carray &a);
		
		//[]运算符重载
		//[]是双目运算符，中括号外是左操作数，括号内是右操作数
		//返回类型是int引用，这是为了实现a[i] = 4的赋值成功
		int & CArray::operator[](int i){
			return ptr[i];
		}
};

CArray::CArray(int s):size(s)
{
	if(s == 0
	{
		ptr = null;
	}else
	{
		ptr = new int[s];
	}
}

//复制构造函数
CArray::CArray(CArray &a)
{
	if(!a.ptr)
	{
		ptr = null;
		size = 0;
		return;
	}
	ptr = new int[a.size];
	memcpy(ptr, a.ptr, sizeof(int) * a.size);
	size = s.size;
}

ubt 
```