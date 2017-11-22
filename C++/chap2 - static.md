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



