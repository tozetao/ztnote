### echo
用于向终端输出字符串，配置-e选项才能才能输出反义字符

example：
```
echo 'this is demo';

echo -e "echo 3 lines\n\n\n";

echo 'the log file have be done' > log.txt
# 重定向输出

echo "hello world\""
# 引号是一个特殊字符，在输出时需要转义
```

### read
read命令用于在标准输入中读取一行，并把输入中的每个字段的值赋予shell变量。

example：
```
read name age address
echo $name;
echo $age;
echo $address;
```

### cat
显示文件内容，创建文件，还可以用于显示控制字符。
注：在文件分页处不会停下来，会显示整个文件内容，因此可以把cat命令的输出通过管道符传递到另外一个具有分页功能的命令中。

命令格式：cat [options] filename1...

options选项：
- -n：由1开始对所有输出的行数编号
- -b：同-n，只不过对于空白不编号
- -s：当遇上有连续俩行以上的空白行，就代替为一行的空白行
- -v：显示非打印字符

example:
```
cat file1
# 显示file1文件内容

cat file1 file2 file3
# 显示多个文件内容

cat file1 file2 file3 > bigfile
# 将多个文件的内容重定向到新文件中

$ cat
hello world
first
<ctrl + d>
$
# 新建文件

cat /dev/null > file
# 清空file文件内容

cat -b httpd.conf
# 输出文件内容并编号

cat -s /etc/X11/XF86Config | sed '/^[[:space:]]*$/d'
# 使用sec与cat去除空白行
```

### more

### tee
读取标准输入的数据，并将内容输出成文件。

格式：tee -options file1 file2 ...

options选项：
- -a：将内容附加到文件后面，而不覆盖。
- -i：忽略终端信号
- --help
- --version：显示版本信息

example：
```
tee teedemo.txt
# 读取标准输入内容，并输出到文件中。

cat demo.txt |tee file1 file2
# 显示demo.txt文件内容并把内容复制到其他文件中

ls -l |tee file 
```

### 管道符
通过管道把一个命令的输出传递给另外一个管道作为输入，管道用竖杆|表示。

格式： 命令1 | 命令2，其中|是管道符

### Linux命令执行顺序
linux命令执行顺序，||和&&和;
- command1 && command2：左边的命令执行成功，右边的命令才会执行
- command1 || command||：左边的命令执行失败，右边的命令才会执行
- command;command2：命令顺序执行

example：
```
cp test.txt test_bak.txt && cat test_bak.txt
# 拷贝成功则显示拷贝的文件

cata || touch b.txt
# 第一个命令执行失败，第二个命令会被执行

pwd;date;more d.txt
```

### grep
搜索过滤，grep是一个文本搜索工具命令，它可用于shell脚本，grep会返回一个状态值来说明搜索的状态，搜索成功返回0，失败返回1，搜索的文件不存在则返回2.

格式：grep -options pattern file

pattern是正则表达式

参数选项：
- -c：计算符合pattern的行数
- -n：显示符合匹配样式的内容，并显示其所在的行数
- -i：忽略字符串大小写
- -A：除了显示符合样式的那一列之外，并显示改行之后的内容。
- -f<规则文件>：指定规则文件，其内容含有一个或多个规则样式，让grep查找符合规则条件的文件内容，格式为每行一个规则样式。



example：
```
cat test.txt |grep -f test2.txt
# 使用test2.txt文件的规则列表去匹配test.txt文件中的内容。

ps aux |grep php-fpm
# 查找php-fpm进程

ps aux |grep -c php-fpm
# 统计php-fpm进程个数

cat test.txt |grep -nf test
```