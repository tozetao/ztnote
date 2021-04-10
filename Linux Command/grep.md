## grep
搜索过滤，grep是一个文本搜索工具命令，它可用于shell脚本，grep会返回一个状态值来说明搜索的状态，搜索成功返回0，失败返回1，搜索的文件不存在则返回2.

格式：grep -options pattern file

pattern是正则表达式，也可以是字符串，在grep命令中输入字符串参数时，最好用双引号括起来，在调用匹配模式时，应使用单引号。

参数选项：
- -c：计算符合pattern的行数
- -n：显示匹配行及匹配行数
- -i：忽略字符串大小写
- -r：递归模式，可同时处理所有层级子目录中的文件
- -l：只输出符合对比的文件名
- -A：除了显示符合样式的那一列之外，并显示改行之后的内容。
- -f<规则文件>：指定规则文件，其内容含有一个或多个规则样式，让grep查找符合规则条件的文件内容，格式为每行一个规则样式。
- -e：切换为egrep，egrep有更多便利的功能


example：

```
cat 
48      Dec     3BC1977 LPSX    68.00   LVX2A   138
483     Sept    5AP1996 USP     65.00   LVX2C   189
47      Oct     3ZL1998 LPSX    43.00   KVM9D   512
219     dec     2CC1999 CAD     23.00   PLV2C   68
484     nov     7PL1996 CAD     49.00   PLV2C   234
483     may     5PA1998 USP     37.00   KVM9D   644
216     sept    3ZL1998 USP     86.00   KVM9E   234
# 案例文本

grep "sort it" *
# 在所有文件中查询sort it字符串

grep "48" test.txt
# 行匹配

grep -c "48" test.txt
# 返回匹配的行数

grep "48\>" test.txt
# 精准匹配

cat test.txt |grep -f test2.txt
# 使用test2.txt文件的规则列表去匹配test.txt文件中的内容。

ps aux |grep -c "php-fpm"
# 统计php-fpm进程个数
```

grep与正则：在使用正则表达式时，最好用单引号括起来，防止与shell冲突。
example：
```
grep '^[34]' test.txt
# 匹配以3/4起始的行

grep -i 'k.*d' test.txt
# 忽略大小写，匹配以k开头，以d结尾字符串的行

grep '^$' test.txt
# 查询空行
```

类似于grep、sed、awk这些命令在使用正则表达式时，能够识别正则表达式大多数的元字符，也有一些元字符需要转义才能使用，目前已知要转义的有{}、()

