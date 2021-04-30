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

### core dump

core dump是记录况应用程序崩溃时的信息，以下几种情况会导致产生core dump文件：

- 内存访问越界（数组越界、字符串无\n结束符、字符串读写越界）
- 堆栈溢出
- 非法指针（如空指针异常或非法地址访问）
- 多线程程序中使用了线程不安全的函数，如不可重入函数
- 多线程读写的数据未加锁保护（临界区资源需要互斥访问）



#### core文件大小

ulimit -c

该命令用于设置core文件的大小。这个值如果为0不会产生core文件，因为core文件一般比较大，一般使用ulimit -c unlimited设置为不限制大小，这样在任意情况下都会产生core文件。



ulimit有效期：ulimit命令只会对当前会话生效，当会话关闭后会丢失之前的设置。





#### limits.conf

ulimit命令只针对当前会话生效，如果要永久生效需要更改/etc/security/limits.conf文件。

关于limits.conf配置文件：https://www.e-learn.cn/topic/2959291



centos系统：

在centos中，修改了limits.conf文件在下次登陆中会生效。不知道是不是ssh中使用了pam。





```conf
-- /etc/security/limits.conf
* soft core unlimited
root soft core unlimited
```







我在centos上面修改了文件后立刻生效，但是ubuntu则不成功。





#### core文件名

默认情况下内核在coredump时所产生的core文件放在与该程序相同的目录中，并且文件名为core.xxx。可以通过以下几种方式来修改core文件名。



> /proc/sys/kernel/core_pattern

core_pattern是一个模板文件，保存着core文件的生成路径，定义了生成core文件名的模板。模板变量有：

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

```shell
echo /tmp/core-%p-%u > /proc/sys/kernel/core_pattern
```

比如该命令的设置生成的core文件名可能为core-10010-555。



core_pattern有效期：core_pattern文件的内容在系统重启之前都是有效的。















#### sysctl.conf

文件所在位置：/etc/sysctl.conf。

系统内核配置文件。系统每次重启时都会读取该文件配置，如果要使修改该文件立即生效，可以执行/sbin/sysctl -p命令。



```conf
kernel.core_pattern=/tmp/core.%e.%p
kernel.core_user_pid=0
```

配置dump core文件名的生成模板。



关于临时修改内核参数：linux的内核参数都是保存在内存中，可以通过命令直接修改生效，但是当系统重启后之前设置的参数就会丢失。







### PHP-FPM

```shell
echo '/tmp/core-%e.%p' > /proc/sys/kernel/core_pattern
echo 0 > /proc/sys/kernel/core_uses_pid
ulimit -c unlimited
```

执行完上面命令再重启PHP-FPM进程后就会产生core dump文件了。

```conf
rlimit_core = unlimited
```

有关ulimit的PHP-FPM配置。



> 测试1

环境：PHP5.6.5，PHP-FPM未设置core-ulimited选项

产生条件：在执行ulimit -c unlimited命令并重启FPM进程后，如果进程异常退出可以产生core文件（注：即使关闭当前会话也会产生）。

关闭条件：执行ulimit -c 0再重启进程后就不会产生core文件了。



> 测试2

环境: PHP7.2.33



> 测试3：在不同会话中执行程序

会话1：ulimit -c unlimited。如果程序异常可以产生core dump文件。

会话2：ulimit -c 0。如果程序异常不会产生core dump文件。











腾讯服务器上面的修改：

1. /etc/profile文件加入了命令：ulimit -S -c 0 > /dev/null2 > &1
2. 编辑了/etc/security/limits.conf文件，加入了ulimit的设置。soft core unlimited；root soft core unlimited
   













## 参考资料

c（core dump）：https://blog.csdn.net/u010150046/article/details/77775114





https://mengkang.net/1195.html

https://www.cnblogs.com/gatherstars/p/6019766.html

https://segmentfault.com/a/1190000015579363



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