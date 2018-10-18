## 信号

信号是事件发生时对进程的通知机制，信号打断了程序执行的正常流程。针对每个信号定义了一个唯一的小整数，<signal.h>以SIGxxx形式的字符对这些整数做了定义。



一个进程可以像另一进程发送信号，进程也可以向自己发送信号。发往进程的信号通常都是源于内核。引发内核为进程产生信号的事件有：

- 用户输入能够产生信号的中断特殊指令，例如Ctrl+C终止字符。

- 硬件事件

  即硬件发生异常，硬件检测到一个错误条件并通知内核，再由内核发送相应信号给进程。

- 软件事件

  即程序触发的，例如进程执行的CPU时间超限，或者该进程的某个子进程退出，或者定时器到期等。



信号的通知过程

信号可分为标准信号和实时信号，标准信号是内核向进程通知事件，它的范围为1-31。

信号因某些事件产生后会在稍后传递给某一进程，而进程也会采取相应措施处理信号，在信号产生和到达期间，信号会处于pending状态。只有但内核调度进程执行时，等待信号才会马上到达，或者进程正在运行也会立即传递信号。



信号的响应

进程对于特定信号采用默认的处理行为，除了特定信号外，针对某些信号可以编写信号处理器程序，例如shell为SIGINT信号（Ctrl+C）做了处理，触发SIGINT信号时会终止程序执行并将控制返回shell的主输入循环(主界面)。



常见信号有：

- SIGABRT

  当进程调用abort()函数，系统向进程发送信号

- SIGALRM

  调用alrm()或setitimer()设置的定时器到期会产生该信号

- SIGINT

  当用户输入终止字符时，终端驱动程序想发送该信号给前台进程组。

- SIGHUP

  当终端断开(挂机)时，将发送该信号给终端控制进程，许多守护进程在收到SIGHUP信号后会重新进程初始化并重新读取配置文件，可借助于kill命令或同样效果的脚本或程序发送SIGHUP信号。

- SIGSEGV

  当应用程序对内存的引用无效时就会产生该信号，C语言中往往是解引用的指针包含了错误地址（未初始化的指针）或传递一个无效参数供函数使用，该信号的命名源于术语“段违规”。







### signal()

```c
#include <signal.h>

void (*signal(int sig, void (*handler)(int)))(int);
// returns previous signal disposition on success, or SIG_ERR on error.

signaction();
```

signal()和signation()可设置信号处理，signal()是设置信号处理的原始API，在不同unix系统中实现存在差异，因此signaction()是建立信号处理程序的首选API。

在signal()中，sig参数是要处理的信号编号，handler是一个函数指针，即信号处理函数，handler也可以是SIG_DEL或SIG_INT信号，因此signal()调用成功返回先前的函数指针，也可以是另外俩个信号。







信号处理器程序是当信号传递给进程时会调用的一个函数。调用信号处理器程序会随时打断主程序流程；内核会代表进程来调用处理器程序；当处理器程序返回时，主程序会在处理器打断的位置恢复执行。













singal原型的代码理解：

```c
typedef void (*sighander_t)(int);

sighander_t signal(int sig, sighander_t handler)
{
    handler(sig);
    return handler;
}

void foo1(int a)
{
    printf("%d\n", a); 
}

int main(int argc, char const *argv[])
{
    signal(5, &foo1);
    return 0;
}
```

