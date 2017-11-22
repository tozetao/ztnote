### this
this是指针，其作用是指向成员函数所作用的对象。
```c
class Complex
{
    public:
        double real, imag;
    
    void print()
    {
        cout << real << "," << imag;
    }

    Complex(double r, double i):real(r),imag(i){}

    Complex addOne()
    {
        this->real++;
        this->print();
        return * this;
    }
}
```

早期的C++程序是没有编译器的，因此运行时是将C++程序转换成C程序使用C编译器来编译执行的，例如：
```c
//C++程序
class Car
{
    public:
        int price;
        void setPrice(int p);
};

void Car::setPrice(int p)
{
    price = p;
}

int main()
{
    Car car;
    car.setPrice(15);
    return 0;
}

//转换后的C程序
struct Car
{
    int price;
}

void setPrice(struct Car *this, int p)
{
    this->price = p;
}

int main()
{
    struct Car car;
    setPrice(&car, 15);
    return 0;
}
```
代码转换后，类将会转换成结构体，成员函数会转换成全局函数并且新增一个结构体指针参数作为第一个参数，该指针指向结构体。


注：静态成员中不能使用this指针，因为静态成员对象并不具体作用于某个对象，因此静态成员函数的真实参数的个数就是程序中写出的参数的个数。
