### 函数指针
程序运行期间，每个函数都会占用一块连续的内存空间，而函数名就是该内存空间的起始地址。
函数指针便是把函数的起始地址赋值给一个指针变量。

定义：类型名 (* 指针变量)(参数类型...)
```c
void PrintMin(int a, int b){}

int main()
{
	int a = 5;
	int b = 6;
	void (* p)(int,int);
	
	p = PrintMin;
	p(a, b);
	return 0;
}
```

example:
```c
int MyCompare(void const * elem1, void const * elem2)
{
	unsigned int *a, *b;
	a = (unsigned int *) elem1;
	b = (unsigned int *) elem2;
	return *a - *b;
}

int main()
{
	unsigned int an[NUM] = {1,2,3,4,4,5,6};
	qsort(an, NUM, sizeof(unsigned int), MyCompare);
}
```