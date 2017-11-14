## 类和对象

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
对象的大小
- 对象的内存空间等于所有成员变量之和的大小，上述例子CRectangle类的对象是sizeof(CRectangle)=8个字节大小。

对象的存储空间
- 每个对象都有自己的存储空间，所以一个对象的某个成员变量被改变是不会影响其他对象的。

对象的运算
- 对象之间可以用=赋值
- 对象不能用比较运算符进行运算，除非这些运算符进行了重载


对象的访问
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

### 类成员的访问范围
- private：指定私有成员，只能在成员函数内被访问
- public：指定公有成员，可以在任何地方被访问
- protected：指定保护成员，允许继承的成员访问

example:
```c

```

如果在类中