

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