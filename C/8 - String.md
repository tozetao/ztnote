在C中，字符串是多个字符和一个位全为0的NUL字节组成的字符数组。NUL字节是字符串的终止符，它本身不属于字符串的一部分，所以字符串的长度不包括NUL字节。



### 字符串常量

当一个字符串常量出现在表达式中，它的值就是一个指针常量。编译器会将这些字符存储在内存中的某个位置，并存储一个指向第一个字符的指针。

```c
"xzy" + 1;
// 字符串常量是一个指针，它表示在该指针上加1，因此指向第二个字符。

*"xzy";
// 对一个指针执行间接访问操作，结果是该指针所指向的内存，该表达式的结果是字符x。

"xzy"[1];
*("xzy" + 1);
// 上面俩个表达式是等价的。
```





### string.h

```c
#include <stddef.h>
#include <string.h>

size_t strlen(char const *str);
//计算字符串的长度

char *strcpy(char const *dist, char const *source);
//拷贝一个字符串

int strcmp(char const *s1, char const *s2);
//如果s1大于s2，返回大于0的值，如果s1小于s2，返回小于0的值，如果s1等于s2则返回0


char *strncpy(char const *dist, char const *source, size_t len);
char *strcat(char *dist, char const *src, size_t len);
char *strncmp(char const *s1, char const *s2, size_t len);
```



find

```c
char *strchr(char const *str, int ch);
char *strrchr(char const *str, int ch);
//查找一个字符

char *strpbrk(char const *str, char const *group);
//返回str字符串中第一个匹配group中任意一个字符的位置

char *strstr(char const *s1, char const *s2);
//查找一个子字符串
```



### 内存处理

在内存中，包含0值的数据并不少见，因此需要利用mem系列函数来处理数据。

```c
void *memcpy(void *dst, void const *src, size_t length);
//从src的起始位置复制length个字节到dist的内存起始位置，如果src与dst出现重叠，它的结果是未定义的。


memcpy(saved_answers, answers, count * sizeof(answers[0]));
//如果要复制的数据大于一个字节，要确保数量与数据类型的长度相乘，保证复制正确的数据


void *memmove(void *dst, void const *src, size_t length);


void *memcmp(void const *a, void const *b, size_t length);
//对俩段内容进行比较，共比较length个字节。这些值会按照无符号字符逐个字节进行比较，如果用于整型或浮点型比较将会出现不可预料的结果。


void *memchr(void const *a, int ch, size_t length);
//用于查找某个字符，查找length个字节


void *memset(void *a, int ch, size_t length);
//把a起始的length个字节设置为ch的值，例如：memset(buffer, 0, SIZE);
```

