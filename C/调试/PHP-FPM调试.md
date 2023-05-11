

一般通过top命令观察进程状态。

如果崩溃了可以core dump，再借助php源码目录下的.gdbinit文件来进行调试。

```shell
gdb php -c core.xxx
source /xxx/php-src-dir/.gdbinit
zbacktrace
```

strace和strace_p也可以用于查看进程的系统调用次数。