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

example：
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
    cout << &iPtr << endl;      //
}
```

### 指针变量的自增自减

### 数组与指针
