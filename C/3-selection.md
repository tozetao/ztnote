### if
```php
# 语法
if (表达式)
	语句
else if(表达式)
	语句
...
else
	语句
```

### 三元运算符
语法：
```php
表达式1 ? 表达式2 : 表达式3
```
example：
```php
int i, j, k;
i = 1;
j = 2;
k = i > j ? i : j;
k = (i >= 0 ? i : 0) + j;
```

### C89的布尔值
C89标准中没有定义布尔类型，但是因为许多程序需要变量存储假或真值，缺少布尔类型可能会有点麻烦，针对这种限制是定义宏，例如
```php
#define TRUE 1;
#define FALSE 0;

int flag = 25;

if(flag == TRUE)
	...
```

### C99的布尔值
C99提供了_Bool类型，布尔变量声明为：_Bool flag；

_Bool是无符号的整数类型，但是和一般的整数类型不同，它只能赋值0或1，如果_Bool存储非0值会导致变量赋值为1.

除了_Bool类型的定义，C99还提供了一个新的头<stdbool.h>，该头提供了bool、true、false宏，使用如下：
```php
bool flag;

flag = true;
flag = false;
```

### switch
语法：
```php
switch (表达式) {
	case 常量表达式：语句
	...
	case 常量表达式：语句
	default：语句
}
```
- 分支：每个分支都是以case + 常量表达式 + 冒号:作为起始，后面跟若干语句 
- 常量表达式：不能包含变量和函数调用，例如5+10是常量，但是n+10就错误了
- 语句：分支的语句不需要用括号括起来，通常会以break结束当前分支运行
- break：该关键字会导致程序执行完当前分支并跳出switch，如果没有break关键字，在确定一个case分支后，程序会执行当前case分支后续的所有语句，包括后续的case分支。
- default：default不是必须的

如果有多个分支执行相同逻辑，可用这样编写语句：
```php
switch (grede) {
	case 4:
	case 3:
	case 2:
		...
		break;
	case 0:
		...
	default:
		...
}
```
