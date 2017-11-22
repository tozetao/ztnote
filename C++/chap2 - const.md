

### 常量对象
如果不希望某个对象的值被改变，则定义该对象的时候可以在前面加const关键字。
```c
class Demo{};

const Demo demo;
```

### 常量成员函数
在类的成员函数说明后面可以加const关键字，则该成员函数成为常量成员函数。

常量成员函数执行期间不应修改其所作用的对象，因此在常量成员函数中不能修改成员变量的值，也不能调用同类的非成员函数，静态的成员变量和函数除外。
```c
class Sample
{
    public:
        int value;
        void getValue() const;
        void func(){};
        Sample(){}
}

void Sample::getValue() const
{
    value = 0;  //wrong 
    func();     //wrong
}

int main()
{
    const Sample o;
    o.value = 100;  //err，常量对象不可被修改
    o.func();       //err

    o.getValue();   //ok，常量对象上可以执行常量成员函数
}
```
俩个成员函数，名字和参数表都一样，但是一个是const，一个不是，这种情况是重载。

### 常引用
常引用无法修改其引用的变量的值，常引用作用于对象的话，则无法修改对象的成员变量的值。