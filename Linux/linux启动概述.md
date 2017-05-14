### 系统启动流程 ###

> 初始化bios

bios包含了cpu的相关信息、设备启动顺序信息、硬盘信息、内存信息、时钟信息、PnP特性等等，计算器首先要加载bios信息，才知道如何去读取设备信息。

> 读取MBR

硬盘上第0磁道第一个扇区称为MBR，也就是Master Boot Record，只有512字节，这里很重要，存放了预启动信息、分区表信息。

系统找到BIOS所指定的的硬盘的MBR后，就会将其复制到0x7c00地址所在的物理内存中，被复制的这部分信息就是Boot Loader，具体到你的电脑就是lilo或者grub了。

> Boot Loader

Boot Loader就是在操作系统内核运行之前的一小段程序，通过这段小程序，可以初始化硬件设备、建立内存空间的映射图，从而将系统的软硬件环境带到一个合适的状态，为加载系统内核做准备。

Boot Loader有若干种，其中Grub、Lib和spfdisk是常见的Loader。
以Grub为例，系统会读取内存中的grub配置信息（一般为menu.lst和grub.lst），依照配置信息加载不同的操作系统。

> 加载内核

根据grub设定的内核映像所在路径，系统读取内存映像，并进行解压缩操作，此时一般屏幕会输出Uncompressing Linux的提示，当内核解压缩完毕后，屏幕会输出Ok，Booting The Kernel！

系统将解压后的内核放置在内存之中，并调用start_kernel()函数来启动一系列的初始化函数，并初始化各种设备，完成Linux核心环境的建立，至此linux内核已经建立起来了，基于Linux的程序就可以正常运行了。

> 用户层init，依据inittab文件来设定运行等级

内核被加载后，第一个运行的程序是/sbin/init，该文件会读取/etc/inittab文件，并依据此文件来进行初始化工作。
/etc/inittab文件的最主要的作用是设定linux的运行级别，其设定形式是":id:5：initdefault:"，这表明linux运行在等级5上。

Linux等级设定如下
0：关机
1：单用户模式
2：无网络支持的多用户模式
3：有网络支持的多用户模式
4：保留，未使用
5：有网络支持有X-Window支持的多用户模式
6：重启

> 6.init进程执行rc.sysinit

设定运行级别后，Linux系统执行的第一个用户层文件是/etc/rc.d/rc.sysinit脚本程序，该文件做的工作非常多，包括设定PATH，设定网络配置(/etc/sysconfig/network)、启动swap分区、设定/proc等。

> 启动内核模块

具体是依据/etc/modules.conf文件或/etc/modules.d目录下的文件来装载内核模块。

> 执行不同运行级别的脚本

根据运行级别的不同，系统会运行rc0.d到rc6.d中相应的脚本程序来完成相应的初始化工作和启动相应的服务。
具体位置：/etc/rc.d

> 执行/etc/rc.d/rc.local

rc.local就是在一起初始化工作后，Linux留给用户设置和启动自己需要的东西。

> 执行/bin/login程序

进入登录状态。