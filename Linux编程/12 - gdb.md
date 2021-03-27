## gdb

gdb一般用于调试c/c++程序。要调试c/c++程序，需要在编译的时候将调试信息加入到可执行文件中，gcc编译器-g选项就可以做到这一点。

gdb的启动方式：

> gdb <program>

program就是可执行文件。

> gdb <program> core

用gdb同时调试一个运行程序和core文件，core是程序非法执行后产生的core dump文件

> gdb <program> <PID>

如果你的程序是一个服务程序，那么你可以指定这个服务程序运行时的进程ID。gdb会自动attach上去并调试它。program应该在PATH环境变量中搜索得到。



启动时的附加选项：

- -c <file>

  调式core dump的core文件。

- -d <directory>

  加入一个源文件的搜索路径。默认搜索路径是环境变量中PATH所定义的路径。

- -s <file>

- -se <file>



程序的运行设置

运行参数

运行路径

工作目录

输入输出





调试已运行的程序

有俩种方法可以调式已经运行的程序。

- 在unix下用ps查看正在运行程序的PID，然后用gdb <program> PID格式挂接正在运行的代码。
- 先用gdb <program> 关联上源代码，并进行gdb，在gdb中用attach命令来挂接进程的PID，并用detach来取消挂接的进程。





断点



观察点









### 断点

> break 行数/函数名

用于设置断点。



> info break

显示断电信息。



> delete break + 断点编号

删除某个断点



### 跟踪



> n

前进一步，显示当前要执行的代码，注意这时候显示的代码还未执行，如果碰到执行的函数会跳过它向下执行。

> s

前进一步，不同的是碰到执行的函数会进入函数内部。

> c

continue命令的缩写，表示继续运行。比如代码执行到一个循环时，可以通过c命令来跳过整个循环的执行到达下一步代码。





### 打印

> print var

print用于打印变量的值，var是变量。

> display var

实时显示变量的值。



### 堆栈

> bt

显示堆栈信息，例如通过该命令可以查看函数栈的信息。





### 线程调试

> set scheduler-locking on

设置只运行当前线程



> info threads

显示线程信息





## core dump

core dump是记录况应用程序崩溃时的信息，以下几种情况会导致产生core dump文件：

- 内存访问越界（数组越界、字符串无\n结束符、字符串读写越界）
- 堆栈溢出
- 非法指针（如空指针异常或非法地址访问）
- 多线程程序中使用了线程不安全的函数，如不可重入函数
- 多线程读写的数据未加锁保护（临界区资源需要互斥访问）



#### 开启core dump

> ulimit -c

用于查看core dump机制是否开启，0则默认不产生core dump。

> ulimit -c 0

关闭core文件的产生。

> ulimit -c unlimited

开启core文件。







#### core dump路径

> /proc/sys/kernel/core_pattern

core_pattern文件保存着core file的生成路径，默认情况下是保存在应用程序当前目录下，但是如果应用程序调用了chdir()函数切换了当前工作目录，则会保存在对应的工作目录。



> echo "/data/<core_file>" > /proc/sys/kernel/core_pattern

通过修改core_pattern文件的路径，可以改变core文件产生的路径。

在这里core_file可以是以下通配符：

- %p

  表示进程的pid

- %u

  进程用户id

- %g

  进程用户组id

- %s

  linux信号

- %t

  dump的时间戳

- %h

  主机名

- %e

  可执行文件名

如果模式的第一个字符是"|"，内核将会把模式的其余字符作为一个命令来运行。内核转储将会被写入到标准输入而不是文件。



linux的内核参数都是保存在内存中，因此可以通过命令直接修改生效，但是当系统重启后，之前设置的参数就会丢失，而系统每次都会去/etc/sysctl.conf文件中读取内核参数。因此可以将该参数写在这个配置文件中：

```
kernel.core_pattern=core.%u.%p
```

保存退出并执行sysctl -p命令使其生效。



注：针对ulimit的设置只会在当前会话生效。









## php-fpm core dump

开启core dump，让php-fpm输出core文件。

- ulimit -c unlimited

  设置core dump大小

- 修改内核core dump文件位置

- rlimit_core = unlimited

  修改php-fpm.conf，配置fpm在收到SIGSEGV信号后记录core dump。

- 重启fpm

注：在php5.6中测试中是不需要第三个步骤的，设置好core的路径后重启fpm就可以看到core dump文件。









## 参考资料

c（core dump）：https://blog.csdn.net/u010150046/article/details/77775114



php-fpm gdb调试：

https://www.laruence.com/tag/how-to-gdb-a-php-core

https://blog.csdn.net/weixin_42737865/article/details/81115269

https://blog.druggo.org/post/2013/05/02/%E4%B8%80%E4%BE%8Bphp%E8%BF%9B%E7%A8%8B%E7%9A%84SIGBUS%E6%95%85%E9%9A%9C

https://www.cnblogs.com/sixiong/p/7048135.html





gdb问题排查

http://werty.cn/2019/10/PHP/Segmentation%20fault%20%E9%97%AE%E9%A2%98%E6%8E%92%E6%9F%A5%E8%AE%B0%E5%BD%95/



debuginfo

https://blog.csdn.net/chinainvent/article/details/24129311



安装包：http://debuginfo.centos.org/6/x86_64/