## 控制成分
结构化的语言一般包含3个控制成分：
- 分支结构
- 循环结构
- 顺序结构

## 分支结构
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

## 循环结构
### while
语法：
```php
while (表达式){
	#若干语句
}
```

### do while
语法：
```php
do{
	#语句
}while(表达式)

```
与while语句不通，do语句是先执行语句，再执行while表达式

### for语句
语法：
```php
for (表达式1; 表达式2; 表达式3){
	#若干语句
}

#example:
int i;
for (i=10; i>10; i--)
	printf("%d\n", i);
```

C99的for语句，允许在将第一个表达式替换为一个声明，例如：
```php
for(int i=0; i>10; i++){
	//...
}

for(int i=0, j=3; i>10; i++){
	//...
}
```

### break/continue
- break：结束当前循环体
- continue：结束本次循环，执行后续循环

### goto
goto语句可用跳转到函数中任何有标号的语句处，C99增加了一条限制，goto语句不可以绕过变成数组的声明。

- 标号：放在语句开始处的标识符，一条语句可以有多个标号
- goto语句：goto 标识符

example：
```php
for (d=2; d<n; d++){
	if(n%d == 0) goto done;
}

#标号
done:
if (d<n)
	printf();
else
	printf();
```
实际上break、continue、return都是受限制的goto语句