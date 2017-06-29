## find命令
find pathname -options [-print -exec -ok]

- pathname：ind命令所查找的目录路径。例如用.来表示当前目录，用/来表示系统根目录。
- -print：find命令将匹配的文件输出到标准输出。
- -exec：find命令对匹配的文件执行该参数所给出的shell命令。相应命令的形式为'command' {} \;注意{}和\；之间是有空格的。
- -ok：和-exec的作用相同，只不过在执行每一个命令之前，都会给出提示，让用户来确定是否执行。

### options选项
- -maxdepth n：指定查找目录的深度，n是数字，代表目录深度。
- -name：根据文件名来查找
- -ctime：根据文件更改时间来查找，有-n，+n参数，+n指文件更改时间距离当前n天之前，-n指文件更改时间距离当前时间n天以内
- -perm：按照文件权限来查找文件。
- -prune：使用这一选项可以使f i n d命令不在当前指定的目录中查找，如果同时使用-depth选项，那么-prune将被f i n d命令忽略。
- 
- -user： 按照文件属主来查找文件。
- -group：按照文件所属的组来查找文件。
- -size n：[c] 查找文件长度为n块的文件，带有c时表示文件长度以字节计。
- -depth：在查找文件时，首先查找当前目录中的文件，然后再在其子目录中查找。
- 
- -nogroup：查找无有效所属组的文件，即该文件所属的组在/ e t c / g r o u p s中不存在。
- -nouser：查找无有效属主的文件，即该文件的属主在/ e t c / p a s s w d中不存在。
- -newer file1 ! file2：查找更改时间比文件f i l e 1新但比文件f i l e 2旧的文件。
- -type 查找某一类型的文件，-d、-f、-l

example：

> find . -maxdepth 1 -name "abc*" -exec ls -l {} \;
// 在当前目录查找abc开头的文件名，并执行ls命令


### xargs
xargs是用于代替find指令的-exec选项。

find命令的-exec选项处理匹配到的文件时，find命令会将匹配到的文件一起传递给exec执行，但有些系统对于传递给exec选项的长度有限制，这样会导致find命令运行一段时间后错误，xargs便是用于解决这个问题的。

find命令将匹配到的文件传递给xargs时，xargs只获取一部分，在处理完毕后才处理下一部分。

example：
> find . -maxdepth 1 -name "t*" |xargs ls -ld
// find配合xargs使用
