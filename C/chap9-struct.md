### 结构体
- 用一组变量来描述一个事物就叫做结构体，结构体是一种数据类型。
```c
//声明一个结构体类型
struct student
{
	int age;
	int id;
	char name[20];
};

//定义该结构体类型
student student1;

//声明并定义
struct student
{
	int id;
	char name[20];
}lige1, lige2;

int main()
{
	//初始化
	student stu1 = {123, "mike"};

	//修改结构体子变量
	stu1.id = 25;
	stu1.name = {'a', 'b', 'c', 'd', 'e', '\n'}; 
}
```

- 结构体之间的赋值相当于拷贝一份副本进行赋值，俩个变量是占据不同内存空间的，
在函数中，无论是将结构体变量作为实参传递，还是将结构体变量作为函数返回值返回，都是把结构体变量拷贝一份进行传递。
```c
struct student
{
	int id;
	char name[20];
};

void renew(student one)
{
	one.id = 9000;
	one.name = "lisi";
	cout << one.id << endl;
}

int main()
{
	student stu1 = {123, "zhangsan"};
	student stu2 = stu1;
	
	renew(stu1);			//输出：9000
	cout << stu1.id << endl;	//输出：123
}
```

指向结构体变量的指针
- ->是一个指向运算符，可以访问一个指针结构体变量的成员。
```c
struct student
{
	int id;
	char name[20];
};

int main()
{
	student nike = {1, {'n', 'i', 'k', 'e', '\n'}};
	stduent *one = nike;
	
	//结构体指针的使用
	
	cout << (*one).id << (*one).name << endl;
	//one是指针，*one是指向指针的内容
	
	cout << one->id << endl;	//
}
```

利用结构体定义数组
example:
```c
int main()
{
	student myclass[3] = 
	{
		1, "zhangsan",
		2, "lisi",
		3, "wangwu"
	}
	
	//数组名是指向第一个元素的指针，也即是指向第一个结构体的指针，自然是以结构体字节作为长度单位
	student *one = mycalss;
	cout << one->id << one->name << endl;
	one++;	
	cout << one->id << one->name << endl;
}
```
