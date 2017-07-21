## 变量
### 1. 定义变量
定义变量时，不用加$符号，例如
> variableName='name'

注：变量名和等号之间不能有空格，同时变量名命名规则如下：
- 首个字母必须是a-z或A-Z或_
- 中间不能有空格
- 不能使用其他标点符号

### 2. 变量的使用
使用一个变量只需要在变量名前加$符号即可，例如：
```
name="zhangsan"

echo $name
echo ${name}
# 这里的花括号是可选的，加花括号是为了让解释器识别变量的边界


echo "this is $name"
echo "${name}is a teacher"
# 变量同样也能在双引号中使用，同php一样
```
### 3. 重新定义变量
已定义的变量可以被重新定义过，例如：
```
url="www.baidu.com"
echo $url

url="www.sina.com"
echo $url
```
注：第二次赋值不能写成$url=...

### 4. 只读变量
使用readonly命令可以将变量定义成只读变量，只读变量的值不能被改变。
下面尝试更改只读变量，结果报错：
```
#!/bin/bash
name="zhangsan"
readonly name

name="lisi"
#报错
```

### 5. 删除变量
使用unset命令可以删除变量，语法：
> unset variable_name

变量被删除后不能再次使用，unset命令不能删除只读变量。
```
#!/bin/bash
url="www.baidu.com"
unset url

echi $url
# 没有输出
```

### 6. 变量的类型
运行shell时，会存在3种变量：
- 局部变量：局部变量在脚本中或命令中定义，仅在当前shell中使用，其他shell启动的脚本不能访问局部便利那个
- 环境变量：所有启动shell都能访问环境变量，必要的时候shell脚本也可以定义环境变量。
- shell变量：shell变量是由shell程序设置的特殊变量，shell变量中有一部分是环境变量，也有一部分是局部变量。

## shell特殊变量
变量名只能包含数字、字母和_，这是因为某些包含其他字符的变量是有特殊意义的，这样的变量称为特殊变量。

例如，$表示当前shell脚本进程的id，即pid。
> echo $$

特殊变量列表

| 变量 | 含义 |
|-----|------|
| $0 | 当前脚本文件名 |
| $n | 传递给脚本或函数的参数。n 是一个数字，表示第几个参数。例如，第一个参数是$1，第二个参数是$2。|
| $# | 传递给脚本或函数的参数个数 |
| $* | 传递给脚本的所有参数 |
| $@ | 同上，所有参数被" "双引号包含着 |
| $? | 上个命令的退出状态，或函数的返回值 |
| $$ | 当前shell进程的id，对于shell脚本，就是这些脚本所在的pid |

### 1. 命令行参数
运行脚本时传递给脚本的参数叫做命令行参数，命令行参数用$n表示，例如$1表示第一个参数，以此类推。

```
#!/bin/bash
echo "File Name: $0"
echo "First Parameter : $1"
echo "First Parameter : $2"
echo "Quoted Values: $@"
echo "Quoted Values: $*"
echo "Total Number of Parameters : $#"
```

运行：
> $ ./test/sh zhangsan alii

### 2. $@和$*的区别
俩个特殊变量都代表着传递给脚本的所有参数，不同的是被""双引号引用的时候表现方式不一样，$@会将参数分开，以"$1"，"$2"，"$n"的形式输出，$*则会将参数作为一个整体输出，"$1 $2 $n"。

测试的时候你循环下这俩个变量就能看出区别。

### 3. 退出状态
所谓退出状态就是上一个命令的执行后返回的结果。
退出状态是一个数字，大部分命令执行成功返回0，失败返回1.
```
$ ./test.sh zhangsna 25
$ echo $?
# 输出0
```

## shell替换
如果在表达式中包含特殊字符，shell就会进行替换，例如在双引号中使用变量就是一种替换，转移字符也是一种替换。例如：
```
#!/bin/bash

a=10
echo -e "value of a is $a \n"
# 这里的-e表示对转义字符进行替换，如果没有-e选项，则会原样输出
```

### 1. 变量替换
变量替换可以根据变量的状态（是否为空、是否定义等）来改变它的值

### 2. 命令替换
命令替换是指Shell可以先执行命令，将输出的结果暂存起来，在适当的地方输出。

语法：
> `command`

```
#!/bin/bash
DATE=`date`
echo "Date is $DATE"
USERS=`who | wc -l`
echo "Logged in user are $USERS"
UP=`date ; uptime`
echo "Uptime is $UP"
```
