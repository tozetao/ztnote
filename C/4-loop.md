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