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

信号处理器程序的执行流程：

信号处理器程序是当信号传递给进程时会调用的一个函数。调用信号处理器程序会随时打断主程序流程；内核会代表进程来调用处理器程序；当处理器程序返回时，主程序会在处理器打断的位置恢复执行。

example：

```c
#include <stdio.h>
#include <signal.h>

void sigHandler(int sig)
{
    printf("Ouch\n");
}

int main(int argc, char const *argv[])
{
    if (signal(SIGINT, sigHandler) == SIG_ERR) {
        printf("error signal\n");
        return 1;
    }

    int i;
    for (i=0; i<3; i++) {
        printf("%d\n", i);
        sleep(3);
    }
    return 0;
}
```



注：内核会忽略SIGKILL和SIGSTOP信号的捕获。





### kill()

```c
include <signal.h>

int kill(pid_t pid, int sig);
```

一个进程能够使用kill()系统调用向两位一个进程发送信号；之所以用kill术语是因为早期unix系统实现中大多数信号的行为都是终止进程。

pid参数标识一个或多个目标进程，sig参数指定要发送的信号。

pid值的不同会产生多种情况：

- pid大于0，会向pid指定的进程发送信号。
- pid等于0，会向调用进程同组的每个进程发送信号，包括进程本身。
- pid小于-1，会向组id等于pid绝对值的进程组内的所有下属进程发送信号
- pid等于-1，信号的发送范围是：调用进程将信号发往每个目标进程，除去init（进程id为1）和调用进程本身。

进程向另一进程发送信号是需要权限的：

- 特权进程（CAP_KILL）可以向任何进程发送权限。
- 以root用户和组运行的init进程只能接收已安装了处理器函数的信号，这是为了防止意外沙溪init进程。
- 发送方进程的实际或有效用户ID与发送方进程的实际用户ID或保存设置用户ID（Saved set-user-id）相匹配，该非特权进程可以向另一进程发送信号。
- SIGCONT信号要特殊对待，非特权进程可以向同一会话中的任何进程发送这一信号。

如果没有进程与pid匹配kill()调用将失败，同时errno置为ESRCH（查无此进程）；如果进程无权发送信号，kill()调用也是失败，errno会置为EPERM。

如果pid所指的一系列进程（pid是负值时）只要其中某个进程发送信号成功，则kill()调用成功。



应用：检查一个进程是否存在，如果信号为0表示空信号，kill()不会发送空信号但是仍然会检查是否能向目标进程发送信号。

example：

```c
#include <stdio.h>
#include <signal.h>
#include <errno.h>

int isProcess(int pid)
{
    int s = kill(pid, 0);
    if (s == 0) {
        return 1;
    }
    
    // 进程存在但是没有权限发送信号
    if (errno == EPERM) {}
    
    // 不存在的进程
    if (errno == ESRCH) {}
}
```



### raise()

```c
#include <signal.h>

int raise(int sig);
```

进程向自身发送信号，当使用raise()系统调用时，信号会立即传递（即在返回结果之前）。

raise()系统调用相当于kill(getpid(), sig)，支持线程的系统会将raise(sig)实现为pthred_kill(pthred_self(), sig)，表示将信号传递给调用raise(sig)的线程，而kill(getpid(), sig)会将信号发送给进程内的所有线程。

raise()出错将返回负数，可能发生错误EINVAL，即信号无效。



### 信号掩码

信号掩码用于阻塞信号传递：内核会为每个进程维护一个信号掩码，即一组信号，并将阻塞这组信号传递给该进程。

如果将遭阻塞的信号发送给某进程，那么对该信号的传递将延后，直到从进程信号掩码中移除该信号，从而解除阻塞为止。



向信号掩码添加一个信号的方式有：

- 使用signaction()函数建立信号处理器程序时，可以指定一组额外信号，当调用该处理器程序时会将其阻塞。
- 在调用信号处理器程序时，可将信号添加到信号掩码中，这是signaction()函数的标志的功能。
- 使用sigprocmask()系统调用，显示的添加或移除信号掩码中的信号。



```c
#include <signal.h>

int sigprocmask(int how, const sigset_t *set, sigset_t *oldset);
```

sigprocmask()可以修改进程的信号掩码，也可获取现有掩码。调用成功返回0，失败返回-1

how参数的类型有：

- SIG_BLOCK

  将set信号集的信号添加到信号掩码中，等价于将信号掩码设置为当前值与set的并集。

- SIG_UNBLOCK

  将set指向信号集中的信号从信号掩码中移除。即使要解除的阻塞的信号当前不处于阻塞状态，也不会返回错误。

- SIG_SETMASK

  将set指向的信号集赋给信号掩码。

oldset参数会存储返回之前设置的信号掩码。如果想要获取信号掩码而又不对其改动，那么可将set参数指定为空，这时会忽略how参数。



### 等待状态的信号

```c
#include <signal.h>
int sigpending(sigset_t *set);
```

如果某进程接收到一个该进程正处于阻塞的信号，那么会将该信号添加到等待信号集中，在解除了对该信号的锁定时，会将该信号传递给该进程。

sigpending()能够获取当前进程正在等待的信号集。

等待信号集与信号掩码是不同的，信号掩码保存着要阻塞的信号，而等待信号集存储处于pengding状态的信号（即该信号在阻塞状态下触发是便处于pengding状态）。

example：

```c
#define _GUN_SOURCE
#include <stdio.h>
#include <signal.h>

void sighandle(int sig)
{
    printf("signal: %d\n", sig);
    if (signal(SIGINT, SIG_DFL) == SIG_ERR) {
        printf("sig_del error\n");
    }
}

int main(int argc, char const *argv[])
{
    /* code */
    int i;
    sigset_t blockSet, prevSet, pendSet;

    // 注册sigint信号的处理程序
    if (signal(SIGINT, &sighandle) == SIG_ERR) {
        printf("registe sigint error.\n");
        return -1;
    }

    sigemptyset(&blockSet);
    sigaddset(&blockSet, SIGINT);

    // 阻塞SIGINT信号
    if (sigprocmask(SIG_BLOCK, &blockSet, &prevSet) == -1) {
        printf("block set error\n");
        return -1;
    }

    // 睡眠5秒，在睡眠期间触发SIGINT信号，看看该信号的处理程序是否会调用
    for(i=0; i<5; i++) {
        sleep(1);
        printf("sleep %d\n", i);
    }

    // 获取阻塞的信号
    if (sigpending(&pendSet) == -1) {
        printf("sigpending error\n");
        return -1;
    }

    // 判断SIGINT信号是否在信号掩码中
    if (sigismember(&pendSet, SIGINT)) {
        printf("SIGINT is in set\n");
    }

    // 将信号掩码重置为原先的状态以解除对信号的阻塞
    printf("end pending...\n");
    if (sigprocmask(SIG_SETMASK, &prevSet, NULL) == -1) {
        printf("setmask error\n");
        return -1;
    }

    return 0;
}
```

等待信号集只是一个掩码，仅表明一个信号是否发生而未表明该信号发生的次数，如果一个信号在阻塞状态下产生多次，那么会将该信号记录在等待信号集中，并在稍后仅传递一次。

如果进程没有设置阻塞信号，其收到的信号也可能会比发送给它的要少的多，这是因为发送程序会在每次获得调度运行时发送多个信号给接受者，然而接受程序运行时传递过来的信号只有一个，因为只会将这些信号中的一个标记为等待状态。

这段描述具体看仓库编写的代码。。。



### sigaction()

```c
#include <signal.h>

int sigaction(int sig, const struct sigaction *act, struct sigaction *oldact);
```

sigaction()用于设置信号处理，它允许在获取信号处置的同时无需将其改变，还可设置各种属性对调用信号处理器的行为施以更加精准的控制。

sig参数表示要获取或改变的信号，该参数可以是SIGKILL和SIGSTOP之外的任何信号。

act参数是结构指针，指向描述信号新处置的数据结构，oldact参数也是指向同一类型结构的指针，用来返回之前信号处理的相关信息。



sigaction结构预览：

```c
struct sigaction {
    void (*sa_handler)(int);
    sigset_t sa_mask;
    
    int sa_flags;
    void (*sa_restorer)(void);
    
    // 其他略...
}
```

sa_handler成员与signal()的handler参数相同，是信号处理器程序，sa_handler成员是函数指针才会对sa_mask、sa_flags成员进行处理。

sa_mask成员指定了一组要阻塞的信号。在调用处理器程序时，会在调用之前将这组信号中未处于信号掩码中的任何信号添加到进程掩码中，这些信号会停留在进程掩码中，直到信号处理器程序返回才会自动删除这些信号。

利用sa_mask可以指定一组信号，不允许这组信号中断此处理器程序的执行。

同时对处理器程序调用的信号会自动添加到进行掩码中，这意味着当处理器程序在执行时，如果同一个信号抵达多次，信号处理器程序是不会递归中断自己的。在处理器程序执行这段时间重复产生的信号，对信号的传递是一次性的。(在之前的代码已经测试过了，一个信号阻塞时，产生的多次信号只会传递一个。)

sa_flags是一个位掩码，用于控制信号处理过程的各种选项，包含的位有：

- SA_NOCLDSTOP

  若sig为SIGCHLD信号时，当接收一信号而停止或恢复某一子进程时，将不会产生此信号。

- SA_NOCLDWAIT

  若sig为SIGCHLD，当子进程终止时不会将其转化为僵尸。

- SA_NODEFER

  捕获信号时，不会在执行处理器程序时将信号自动添加到进程掩码中。

- SA_ONSTACK

  针对此信号调用处理器函数时，使用了有signaltstack()安装的备用栈。

- SA_RESETHAND

  当捕获该信号时，会在调用处理器函数之前将信号重置为默认值（SIG_DEL）

- SA_RESTART

- SA_SIGINFO

  调用信号处理器程序时携带了额外参数，包含了信号的深入信息。



### pause()

```c
#include <unistd.h>

int pause(void);
```

调用pause()将暂停进程的执行，直到信号处理器函数中断该调用为止，或者直到一个未处理信号终止进程为止。

处理信号时，paue()会遭到中断，并总是返回-1，并将errno置为EINTR。