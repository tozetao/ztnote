



### 内存资源

- CODE

  可执行文件代码段大小，即text resident set（TRS）（KB）

- DATA

  数据段大小+栈空间大小，即data resident set（DRS）（KB）

- RES

  resident size（KB），进程占用的实际物理内存大小。对应%MEM列（占用内存空间百分比）。

  RES = CODE + DATA

- SWAP

  Swapped size（KB），使用的交互分区大小。

- USED

  RES + SWAP大小

- SHR

  Shared Memory（KB），指明了VIRT中有多少是共享内存或库大小。

  比如你的应用是依赖了某个C库。

- VIRT

  Virtual Image（KB）虚拟内存分配大小，包含了其他文件（如代码文件、共享库、mmap等）的映射，交换分区映射的地址区间大小。

  以虚拟内存的地址空间占用理解就对了，并不能准备反应物理内存的占用。



> %MEM = RES / total MEM

$MEM表示一个进程占用总内存空间大小的百分比，由RES除以总内存大小得到。





### CPU资源

- TIME

  进程占用CPU时间。如果使用S命令启用累加模式（Cumulative Model）则包含增进的子进程运行时间。

  要注意它不是从进程启动到当前的一个时间，而是占用CPU使用的时间。

- TIME+

  CPU时间，比如5:20:30，对应CPU的时间格式为：minutes:seconds:hundredths。

- %CPU

  CPU使用率

- P

  last used cpu。



### CPU使用率

top命令中关于CPU使用率的主要由以下数据来提现：

> %Cpu(s) 0.3 us,    0.0 sy,    0.0 ni,    99.7 id,    0.0 wa,    0.0 hi,    0.0 si,    0.0 st

其中：

- us 表示用户态的CPU占用时间
- sy 表示系统内核态CPU占用时间
- ni 手动设置了nice值的进程占用的CPU时间
- id CPU空闲时间
- wa CPU等待IO完成的时间
- hi 处理硬件中断的时间
- si 处理软件中断的时间
- st 虚拟化环境中被虚拟机steal（偷走）占用的时间。



### 平均负载

Linux中的平均负载有如下一个值，一个简单的理解是它们分别对应1分钟、5分钟、15分钟的平均负载。

> load average: 1.01, 1.10, 1.18

但是其实它是指数移动平均来计算的，也就是之前的负载值对当前的负载值的影响比较小。

值得说明的是，其统计的不仅仅是运行中的任务，也包含D（uninterruble_sleep, disk sleep）状态的任务，其数据也要结合CPU核数来看才有意义。

简单的说平均负载除CPU核数大于3，那么负载就有问题。也就是说平均有3个进程排在一个CPU核中等待执行。





### 进程状态

进程状态有4种

- R Running 运行中
- S Sleeping 睡眠中
- D Disk sleep 即uninteruptible_sleep 一半是等待硬件IO
- Z Zoombie 一般是父进程在调用wait系统调用读取子进程状态之前就退出了，导致子进程还占用了进程表的空间。













### Field

- NI

  nice value，NI值就是友善度，由于进程是抢占式运行的，因此友善度越高进程得到的运行时间就越少。默认是0，范围[-20, 19]。

- PR

  Priority（通过20+NI计算出来的），跟NI值一样的语义，也是友善度。值越低抢占的运行时间就越多，实际的优先级就越高。

- REM

  进程占用的实际内存大小。

- %MEM

  进程使用的内存占用总存在大小的百分比

- COMMAND

  进程名字或启动的命令行。



交互命令

f/F：增加或移除字段。

X：增加固定宽度。



筛选进程。

L：Locate，定位，用于筛选进程。

&：表示再次查找。





### 命令行选项

-p：监控指定PID的进程

-u：监控特定用户的进程

-c：显示进程的完整启动命令



example：

```shell
top -c -p $(pgrep -d ',' -f nginx)
```

观察特定进程名的信息，如果进程死亡top命令将会失效。



```shell
pidof <你的进程名字> |awk '{for(i=1;i<=NF;i++){ printf $i; if(i!=NF) printf ",";}}' |xargs terminator -x top -p


# 这个就是个awk命令，其中输出使用print的话会换行，使用printf就不会换行
awk '{for(i=1;i<=NF;i++){ printf $i; if(i!=NF) printf ",";}}'

# 启动top命令{}
xargs terminator -x top -p 
```





最后一个子命令我认为也是一个难点，由于直接用 "xagrs top -p"会报错：top: failed tty get，我看到很多网友说这个是在后台运行的原因。也可以加一个-b参数，但是就不会动态刷新了，就是死板的输出，没有了top本身的美感，让人头疼。

没有找到终端。 于是想到用终端启动命令，我这里用的terminator，大家可以即兴发挥。






### pgrep

pgrep命令用于显示特定进程名的信息。

- d：定义多个进程之间的分隔符，如果不定义则使用换行符。
- -P：根据父进程pid找出所有子进程。
- -n：如果该程序有多个进程在运行，则仅找出最新的
- -o：同上，只不过是找出最老的进程（最早启动的）
- x：表示进程的名字必须完全匹配，以上的选项均可以部分匹配
- -l：不仅打印pid，也打印进程名
- -f：一般与-l配合使用，将打印进程的参数（COMMAND）



