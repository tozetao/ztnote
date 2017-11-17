### 静态成员
在类中的成员如果前面有增加statis关键字，那么该成员是静态成员。
有static关键字的变量叫做静态变量，有static关键字的方法叫做静态方法。
```c
class CRectangle
{
    private:
        int w,h;
        static int area;
        static int number;

    public:
        CRectangle(int _w, int _h);
        ~CRectangle();
        
        static void PrintTotle();
        void sum();
};
```

- 普通成员变量在内存中每个对象都有各自一份，而静态成员变量在内存中只有一份且被所有对象共享。
- sizeof运算符不会计算成员变量的大小，即sizeof obj将忽略静态成员变量。
- 普通成员函数必须作用于某个对象，而静态成员函数并不具体作用于某个对象
- 静态成员不需要通过对象来访问
```c
int main()
{
    Crectangle::PrintTotle();
    
    //对象名来访问
    Crectangle c;
    c.PrintTotle();
    //静态成员也能通过对象来访问，这只是一种访问形式。

    //指针来访问
    Crectangle c;
    Crectangle *p =& c;
    p->PrintTotle();

    //引用来访问
    CRectangle &r = c;
    r.PrintTotle();
    
}
```

注意：
- 静态成员本质上是全局变量，哪怕一个对象都不存在，类的静态成员变量也存在。
- 静态成员函数本质上全局函数
- 必须在定义类的文件中对静态成员变量进行一次说明或初始化，否则编译能通过，链接不能通过。
- 在静态成员函数中，不能访问非静态成员变量，也不能调用非静态成员函数


设置静态成员的目的在于将某些类紧密相关的全局变量和全局函数写到类中，看上去像一个整体，易于维护。

### 成员对象
一个类的成员变量是另一个类的对象，这种成员变量叫做成员对象，
而包含这种成员对象的类叫做封闭类。

```c
class CTyre
{
    private:
        int radius;
        int width;
    
    public:
        //这是另外一种对成员变量赋值的方法
        Ctype(int w, int r):radius(r),width(w){}
};

class CEngine
{};

class Car
{
    private:
        int price;
        CTyre tyre;
        CEngine;

    public:
        Car(int p, int tr, int tw);        
}

Car::Car(int p, int tr, int tw):price(p),tyre(tr, tw){};

int main()
{
    Car car(20000, 17, 225);
    return 0;
}
```

上面代码这种初始化成员变量的方式叫做添加初始化列表，
成员对象初始化列表中的参数可以是：
- 任意复杂的表达式
- 函数/变量/表达式中的函数，变量必须有定义

当封闭类对象生成时，构造函数初始化过程如下：
- 执行所有成员对象的构造函数
- 成员对象的构造函数执行顺序与在类中定义的顺序一致
- 执行封闭类的构造函数

当封闭类对象消亡时，析构函数调用过程如下：
- 先执行封闭类的析构函数
- 再执行成员对象的析构函数，调用顺序与构造函数相反

封闭类与成员对象构造函数和析构函数的调用顺序其实是一个入栈和出栈的过程。


### 友元(Friend)
友元是为了让外部对象能够以某种形式来访问自身的私有成员。

- 友元函数：一个类的友元函数可以访问该类的私有成员。
```c
class Car;

class Driver
{
    public:
        void modifyCar(Car *car);
}

class Car
{
    private:
        int price;
    

    friend int mostExpensiveCar(Car cars[], int total);
    friend void Driver::modifyCar(Car *car);
};

//在类中进行访问
void Driver::modifyCar(Car *car)
{
    car->price = 1000;
}

//全局函数中进行访问
void mostExpensiveCar(Car cars[], int total)
{
    
}
```

- 将一个类的成员函数(包括构造、析构函数)定义成另外一个类的友元
```c
class B
{
    public:
        void function();
}

class A
{
    friend void B::function();
    //将B的函数定义成A类的友元函数，在B的函数中就能访问A的私有成员
}
```

友元类：A是B的友元类，那么A就能访问B的私有成员
```c
class Car
{
    private:
        price;

    friend class Driver;
};

class Driver
{
    public:
        Car car;
        void modifyCar()
        {
            car.price += 1000;
        }
}
```

注意：友元类之间的关系不能传递也不能继承。