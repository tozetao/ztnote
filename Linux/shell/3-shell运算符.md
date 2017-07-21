
## shell运算符
bash支持算数运算符、布尔运算符、关系运算符、字符串运算符和文件测试运算符，原生bash不支持数学运算，但是能通过awk和expr命令来计算。

expr是一款表达式计算工具，使用它能完成表达式的求值运算，例如：
```
#!/bin/bash
val=`expr 2 + 2`
echo "Total value : $val"
```
注1：表达式和运算符之间要有空格，例如 2+2 是不对的，必须写成 2 + 2，这与我们熟悉的大多数编程语言不一样。
注2：完整的表达式要被 ` ` 包含，注意这个字符不是常用的单引号，在 Esc 键下边。

### 1. 算术运算符
| 符号 | 说明 |
|-----|------|
| + | 加 |
| - | 减 |
| * | 乘 |
| / | 除 |
| % | 取余 |
| == | 相等 |
| != | 不等 |
| = | 赋值 |

主要使用expr命令来进行运算，例如：
```
#!/bin/sh
a=10
b=20
val=`expr $a + $b`
echo "a + b : $val"
val=`expr $a - $b`
echo "a - b : $val"
val=`expr $a \* $b`
echo "a * b : $val"
val=`expr $b / $a`
echo "b / a : $val"
val=`expr $b % $a`
echo "b % a : $val"
if [ $a == $b ]
then
   echo "a is equal to b"
fi
if [ $a != $b ]
then
   echo "a is not equal to b"
fi
```
注：乘号(*)前边必须加反斜杠(\)才能实现乘法运算；
注：条件表达式要放在方括号之间，并且要有空格，例如 [$a==$b] 是错误的，必须写成 [ $a == $b ]。

### 2. 关系运算符
- -eq，检测两个数是否相等，相等返回 true。
- -ne，检测俩个数是否不相等，是的话返回true
- -gt，大于
- -lt，小于
- -ge，
- -le，

### 3. 布尔运算符
| 运算符 | 说明 |
|-------|------|
| ! | 非运算，表达式为true将返回false，表达式false将返回true |
| -a | 与运算，两个表达式都为 true 才返回 true。|
| -o | 或运算，有一个表达式为 true 则返回 true。|  

### 4. 字符串运算符
| 运算符 | 说明 |
|-------|------|
| = | 检查俩个字符串是否相等 |
| != | 检测俩个字符串是否不等 |
| -z | 检测字符串长度是否为0 |
| -n | 检测字符串长度是否不为0 |
| str | 检测字符串是否为空 |

```
#!/bin/sh
a="abc"
b="efg"

if [ $a ]
then
   echo "$a : string is not empty"
else
   echo "$a : string is empty"
fi

if [ $a = $b ]
then
   echo "$a = $b : a is equal to b"
else
   echo "$a = $b: a is not equal to b"
fi
```
注：字符串相等的判断与数字相当的判断不同，字符串相等的判断是一个=号

### 5. 文件测试符
文件测试符用于检测unix文件的各种属性。

```
[ -f "somefile"]
# 判断是否一个文件

[ -d "Directory"]
# 判断目录是否存在

[ -x "/bin/ls"]
# 判断/bin/ls是否存在并有可执行权限

[ -r "/bin/ls" ]
# 判断文件是否可读

[ -n "$var"]
# 判断$var变量是否有值

[ "$a"="$b"]
# 判断俩个变量是否相等

注：通常用[]中括号来表示条件测试，中括号中表达式前后一定要有空格，这是语法规定。


# 判断登录shell的名称
# $SHELL是系统变量
if [ ${SHELL}="/bin/bash" ]; then
	echo 'true'
else
	echo 'false'
fi
```
具体看手册。。。