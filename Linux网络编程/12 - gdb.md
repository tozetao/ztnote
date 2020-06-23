> gdb  可执行文件，用于调试一个程序。

gdb指令

- break

  设置断点，后面可以输入函数名、代码函数

- info break

  显示断点信息

- delete break + 断点序号

  删除某个断点



- s

  步进一步跟踪，如果碰到执行的函数则会进入函数

- n

  步进一步跟踪，会跳过函数继续向下执行



- print  + 变量名

  打印变量的值

- display + 变量名

  实时的显示变量的数值



- bt

  显示堆栈信息，例如通过该命令可以查看函数栈的信息。



- set scheduler-locking on

  设置只运行当前线程

- info threads

  显示线程信息



