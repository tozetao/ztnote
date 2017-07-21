## shell基础

### 1. 文件格式介绍
```shell
#!/bin/bash

echo "hello world"
```
在上面的代码中，#!是说明这个文件的类型，linux根据#!以及后面的信息来确定文件的类型，关于这点可以查看"man magic"命令和/usr/share/magic文件了解更多内容。

/bin/bash表明文件是一个bash程序，并且需要/bin目录下的bash程序来解释执行。
bash程序一般放在/bin目录下，如果你的linux系统比较特别也可能放置在/sbin、/usr/bin、/usr/sbin、/usr/local/sbin或/usr/local/bin目录下。

如果寻找不到bash程序，可以通过whereis bash、locate bash命令来查找bash命令的位置，#在bash文件中是注释的意思，#!除外。

### 2. shell文件的执行
- 显示调用：通过bash或sh命令来执行，例如bash filename.sh
- 隐示调用：引用当前文件来执行，例如./filename.sh

### 3. 输入、输出和错误输出
在linux中除了输入、输出俩个概念，还有一个错误输出的概念。
- 标准输入：stdin，默认为键盘输入
- 标准输出：stdout，默认为屏幕输出
- 错误输出：stderr，输出的是错误信息

std是standard单词的意思，在bash中，标准输出是1，错误输出是2，输入、输出和错误输出主要用于I/O的重定向，就是说需要改变他们的默认设置。
```shell
> 重定向操作符
$ ls -l > result
// 该命令是将ls -l命令的结果重定向输出到result文件中

$ ls -l >> result
// 同上，只不过是将结果附加输出到result文件中。

$ find /home -name z* 2> err_result
// 如果是非root用户，上面的命令将会因为无权限访问一些目录而报错，错误的信息将会输出到err_result文件中

$ find /home -name z* >& all_result
// 将输出、错误信息输出一起重定向输出到all_result文件中

- 重定向操作符
$ (cd /source/directory && tar cf - . ) | (cd /dest/directory && tar xvfp -)
// 该命令表示把 /source/directory 目录下的所有文件通过压缩和解压，快速的全部移动到 /dest/directory 目录下去

```

### 4. shell特殊参数（命令行参数）
script针对参数已经有设定好一些变量名称，对应如下：
```
$ /path/to/scriptname opt1 opt2 opt3
// $0	$1	$2	$3
// 上述的命令中，$0是执行的脚本名字，$1是第一个参数，以此类推，
```
除了数字变量外，还提供了一些特殊变量供script使用
```
$#，表示参数总数
$@，引用所有的参数，每个参数都是独立的，分别用""双引号分隔开
$*，引用所有的参数，不同的是所有参数是以空格分隔连接起来。
```

### 5. date
```shell
#!/bin/bash

PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:~/bin
export PATH

echo -e "i will use 
```
