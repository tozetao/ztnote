### 概念

信号是事件发生时对进程的一种通知机制，信号打断了程序执行的正常流程。

体现在系统中信号是一个唯一的整数，从1开始定义，在<signal.h>文件中以SIGxxx形式的字符对这些整数做了定义，由于各个系统定义的信号编号略有差异，一般使用信号名来表示某个信号。

信号分别标准信号和实时信号。标准信号是内核向进程通知事件，它的范围为1-31。





### 生成方式

按照信号的产生方式，可以分为同步生成与异步生成。

- 异步生成

  异步生成指的是引发信号产生的事件，其发生与进程无关，例如用户输入中断字符，子进程终止等；

- 同步生成

  同步生成指的是信号的产生是由进程本身的执行造成的，进程通过kill()等系统调用向自身发送信号或执行特定的机器语言指令导致硬件异常而产生的信号，例如SIGBUSS、SIGFPE等信号。

注：同步与异步只是信号产生的方式，而不是针对信号本身，所有信号即可以同步产生也可以异步产生。



### 触发流程

信号的整个处理流程可以分为是：产生信号 => 处于等待（pending）状态 => 送达信号 => 处理信号。



pending：在信号产生和到达进程期间，信号会处于pending状态，只有但内核调度进程执行时，或者正在运行的进程信号才会立即到达。

如果将信号添加到进程的掩码中，该信号触发时，内核会阻塞它，不让它送达到进程。阻塞状态下的信号也认为是处于pending状态。



处理信号的方式有多种，如果不通过程序处理，内核会有以下的默认处理行为：

- 忽略信号：内核将信号丢弃，进程对信号没有任何影响。
- 杀死进程
- 杀死进程并产生转储文件
- 暂停进程的执行
- 恢复进行的执行

而通过程序处理信号，可以注册信号处理程序，或者忽略信号处理、重置信号的处理方式。



### 信号处理程序

信号处理程序是指当信号传递给进程时会调用的一个函数，调用信号处理器程序会随时打断主程序流程；内核会代表进程来调用处理器程序，当处理器程序返回时，主程序会在处理器打断的位置恢复执行。

- 在执行一个信号处理器时，如果内核再次捕获信号会怎么样？

  如果是捕获相同的信号，内核会按照捕获的顺序队列执行；如果不是相同的信号，那么会中断次处理函数的执行，转而去调用捕获信号的处理函数，如此递进。

注：内核会忽略SIGKILL和SIGSTOP信号的捕获。

example：

```c
#include <stdio.h>
#include <signal.h>

void sigHandler(int sig)
{
    printf("Ouch\n");
}

// 分别在主程序和信号处理程序睡眠期间触发信号，查看运行效果。
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





### 信号集

一组不同信号的集合称为信号集，sigset_t系统数据类型可以用于表示信号集，它是一个位掩码。

系统提供了以下函数来操作信号集：

```c
sigemptyset(sigset_t *set);
```

将一个信号集置空。

```c
sigfillset(sigset_t *set);
```

初始化一个信号集，使其包含所有信号，包括实时信号。

信号集必须通过以上函数来进行初始化，因为C语言并不会对自动变量进行初始化。



```c
sigaddset(sigset_t *set, int sig);
```

在信号集中添加一个信号。

```c
sigdelset(sigset_t *set, int sig);
```

在信号集中删除一个信号。



```c
sigismember(const sigset_t *set, int sig);
```

如果sig是set信号集的成员，函数将返回1，否则返回0



- NSIG常量

  该常量定义于signal.h文件中，其值等于信号最大编号加1，为了使其可见需要使用特定实现的编译器选项，例如在linux中必须定义以下功能测试宏之一：BSD\_SOURCE、\_GUN_SOURCE、_SVID\_SOURCE.

```c
#define _GUN_SOURCE
#include <string.h>
#include <signal.h>

//打印信号集中添加的信号
void printSigset(sigset_t *sigset)
{
    int i;
    for(i = 1; i < NSIG; i++){
        if(sigismember(sigset, i)){
            printf("signal %d in sigset\n", i);
        }
    }
}
```



### 信号掩码

内核为每个进程维护了一个信号掩码，即一组信号，处于进程信号掩码中的信号会被阻塞，无法传递给该进程。

如果将遭阻塞的信号发送给某进程，那么对该信号的传递将延后，直到从进程信号掩码中移除该信号，从而解除阻塞为止。



```c
#include <signal.h>
int sigprocmask(int how, const sigset_t *set, sigset_t *oldset);
//returns 0 on success, or -1 on error.
```

sigprocmask()可以修改进程的信号掩码，也可获取现有掩码，调用成功返回0，失败返回-1。

how参数对进程信号掩码的操作类型：

- SIG_BLOCK

  将set信号集的信号添加到信号掩码中，等于将当前进程的信号掩码值与set信号集进行并集运算。

- SIG_UNBLOCK

  将set信号集中的信号从进程的信号掩码中移除，即使该信号不存在也不会返回错误。

- SIG_SETMASK

  将set信号集赋给进程的信号掩码。

oldset参数如果不为空，则其指向一个sigset_t结构缓冲区，用于存储进程之前的信号掩码。

在SUSv3规定中，如果有任何等待信号因sigprocmask()的调用而解除了阻塞，那么在该调用返回之前至少会传递一个信号，可以认为如果解除了某个信号的阻塞，那么会立刻将该信号传递给进程。

注：SIGKILL和SIGSTOP信号是无法进行阻塞的。



### pending

所谓pending指信号已经发出，但是接受信号的进程还未来得及处理。比如进程目前在内核态运行且还没有返回到用户态，或者进程将该信号设置为阻塞状态。



### 等待信号集

```c
#include <signal.h>

int sigpending(sigset_t *set);
//returns 0 on success, or -1 on error.
```

sigpending()能够获取当前进程正在等待的信号集。



进程拥有自身的等待信号集，如果它接受到一个正处于阻塞的信号，会将其添加到等待信号集中。当解除对该信号的阻塞时，才会将信号传递给该进程进行处理。

等待信号集只是一个掩码，对于标准信号来说，仅表明该标准信号是否发生，而未表明该信号发生的次数。

如果同一个信号在阻塞状态触发多次，那么会将该信号记录在等待信号集中，并且在解除阻塞后仅传递一次。即使是在未阻塞状态下，接收方收到的信号也会比发送方发的少的多，这是因为发送进程会在每次获得调度运行时发送多个信号给接受者，然而接受进程运行时传递过来的信号只有一个，因为只会将这些信号中的一个标记为等待状态。

如果修改了处于等待状态信号的处置（处理方式），那么在解除对该信号的阻塞后，会根据新的信号处置来处理该信号。例如对处于等待状态信号设置为SIG_IGN或者SIG_DEL，忽略该信号，从而阻止传递处于等待状态的信号。

注：等待信号集与信号掩码是不同的，信号掩码存储要阻塞的信号，而等待信号集是存储pending状态的信号，即进程阻塞的信号哪些是被触发的。



example: 测试信号阻塞情况下，多次触发该信号后的处理情况

```c
#define _GUN_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

void sighandle(int sig)
{
    printf("signal: %d\n", sig);
}

void printPendingSig(sigset_t *pendSet)
{
    int i;
    for(i = 1; i < NSIG; i++) {
        if (sigismember(pendSet, i)) {
            printf("pending signal %d\n", i);
        }
    }
}

//阻塞某个信号，在睡眠期间，多次发送该信号，测试解除阻塞信号后的处理情况
int main(int argc, char const *argv[])
{
    sigset_t pendSet, prevSet, blockSet; 
    int i, cnt;

    sigemptyset(&prevSet);

    // 为所有信号注册处理程序
    for(i = 1; i < NSIG; i++) {
        signal(i, sighandle);
    }

    // 阻塞所有信号
    sigfillset(&blockSet);
    sigprocmask(SIG_BLOCK, &blockSet, &prevSet);

    // 睡眠，在睡眠期间手动去触发信号，测试效果
    for(cnt = 0; cnt < 3; cnt++) {
        printf("sleep %d\n", cnt + 1);
        sleep(1);
    }

    // 打印正在阻塞的信号
    sigpending(&pendSet);
    printPendingSig(&pendSet);

    // 解除阻塞信号
    sigprocmask(SIG_SETMASK, &prevSet, NULL);

    return 0;
}
```



### 传递时机

一个未阻塞信号是在什么时候传递的？根据信号产生的方式，传递的时机是不同的。

同步产生的信号会立即传递，例如进程通过kill()等系统调用向自身发送信号，该信号会在函数调用返回之前就已经发出，或者硬件异常触发的信号。

异步产生一个信号时，即使该信号未将其阻塞，在信号的产生和传递之间仍然可能会有一个瞬间的延迟，在延迟期间，信号处于等待状态。这是因为内核将等待信号传递给进程的时机是该进程正在执行，且发生在由内核态向用户态的下一次切换时，意思是：

- 进程在前一次调度超时后，再次获得调度时，即获得一次时间片段的执行

- 系统（函数）调用完成时。

  要注意的是，信号的传递可能会引起正在阻塞的系统调用提前结束。



### 传递顺序

讨论标准信号的传递顺序只有在解除对多个信号的阻塞下才有意义，因为不阻塞的信号在触发时就直接传递过去了。

如果解除对多个信号的阻塞，信号会立即传递给进程，标准信号的传递顺序是由系统决定的，因此编写代码时不能依赖于传递顺序的。



### signal()

```c
#include <signal.h>

void (*signal(int sig, void (*handler)(int)))(int);
// returns previous signal disposition on success, or SIG_ERR on error.
```

signal()用于注册信号处理程序，signal()调用成功返回之前的信号配置，失败返回SIG_ERR。

在signal()中，sig参数是要处理的信号编号，handler是一个函数指针，即信号处理函数，除了函数指针，也可以使用下面的值来替代：

- SIG_DEL

  将信号处理重置为默认值，例如将signal()调用所改变的信号处理还原。

- SIG_INT

  忽略该信号

注：signal()是注册信号处理函数的原始API，在不同unix系统中实现存在差异，一般不建议使用。



singal原型可以理解为：

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











### kill()

```c
include <signal.h>
int kill(pid_t pid, int sig);
```

kill()不是用于杀死进程，而是用于向进程发送信号。一个进程能够使用kill()系统调用向自身或者其他进程发送信号。

pid参数标识一个或多个目标进程，sig参数指定要发送的信号。

pid值的不同发送给进程的方式也不同：

- pid大于0，会向pid指定的进程发送信号。
- pid等于0，会向调用进程同组的每个进程发送信号，包括进程本身。
- pid小于-1，会向组id等于pid绝对值的进程组内的所有下属进程发送信号
- pid等于-1，信号的发送范围是：调用进程将信号发往每个目标进程，除去init（进程id为1）和调用进程本身。

如果没有进程与指定的pid对应，kill()调用会失败，并将errno置为ESRCH（查无此进程）。



进程向另一进程发送信号是需要适当的权限：

- 特权进程（CAP_KILL）可以向任何进程发送权限。
- 以root用户和组运行的init进程只能接收已安装了处理器函数的信号，这是为了防止意外沙溪init进程。
- 如果发送方进程的实际或有效用户ID与接收方进程的实际用户ID或保存设置用户ID（Saved set-user-id）相匹配，那么允许发送信号
- SIGCONT信号要特殊对待，非特权进程可以向同一会话中的任何进程发送这一信号。

如果进程无权发送信号，kill()调用也是失败，errno会置为EPERM；如果pid所指的一系列进程（pid是负值时）只要其中某个进程发送信号成功，则kill()调用成功。



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



### sigaction()

```c
#include <signal.h>

int sigaction(int sig, const struct sigaction *act, struct sigaction *oldact);
```

sigaction()用于设置信号处理，它允许在获取信号处置的同时无需将其改变，还能通过设置各种属性调整对调用信号处理器的行为，以此得到更加精准的控制。

- sig

  sig参数表示要获取或改变的信号，该参数可以是SIGKILL和SIGSTOP之外的任何信号。

- sigaction *act

  act参数是一个结构指针，指向描述信号新处置的数据结构，oldact参数也是指向同一类型结构的指针，用来返回之前信号处理的相关信息。

sigaction结构

```c
struct sigaction {
    void (*sa_handler)(int);
    sigset_t sa_mask;
    
    int sa_flags;
    void (*sa_restorer)(void);
    // 其他略...
}
```

- sa_handler

  sa_handler成员与signal()的handler参数相同，是信号处理器程序，sa_handler成员是函数指针才会对sa_mask、sa_flags成员进行处理。

- sa_mask

  sa_mask成员指定了一组要阻塞的信号。在调用处理器程序时，会在调用之前将这组信号中未处于信号掩码中的任何信号添加到进程掩码中，这些信号会停留在进程掩码中，直到信号处理器程序返回才会自动删除这些信号。

  利用sa_mask可以指定一组信号，不允许这组信号中断此处理器程序的执行。

- sa_flags

  是一个位掩码，用于控制信号处理过程的各种选项，包含的位有：

  SA_NOCLDSTOP：若sig为SIGCHLD信号时，当接收一信号而停止或恢复某一子进程时，将不会产生此信号。

  SA_NOCLDWAIT：若sig为SIGCHLD，当子进程终止时不会将其转化为僵尸。

  SA_NODEFER：捕获信号时，不会在执行处理器程序时将信号自动添加到进程掩码中。

  SA_ONSTACK：针对此信号调用处理器函数时，使用了有signaltstack()安装的备用栈。

  SA_RESETHAND：当捕获该信号时，会在调用处理器函数之前将信号重置为默认值（SIG_DEL）

  SA_SIGINFO：调用信号处理器程序时携带了额外参数，包含了信号的深入信息。

  SA_RESTART：???

要注意的是，引发信号处理器调用的信号会自动添加到掩码中，这意味着当处理器程序在执行时，如果同一个信号抵达多次，信号处理器程序是不会递归中断自己的。

在处理器程序执行这段时间重复产生的信号，对信号的传递是一次性的。(在等待信号集中，一个信号阻塞时，产生的多次信号只会传递一个。



### pause()

```c
#include <unistd.h>

int pause(void);
```

调用pause()将暂停进程的执行，直到信号处理器函数中断该调用为止，或者直到一个未处理信号终止进程为止。

处理信号时，paue()会遭到中断，并总是返回-1，并将errno置为EINTR。

example：

```c
void siginfoHandler(int sig, siginfo_t *si)
{
    printf("%d\n", sig);
}

int main(int argc, char const *argv[])
{
    struct sigaction sa;
    sigset_t mask;
    sa.sa_sigaction = siginfoHandler;
    sa.sa_flags = SA_SIGINFO;
    sigfillset(&mask);

    sigaction(SIGINT, &sa, NULL);

    pause();
    printf("%d\n", errno);
    return 0;
}
```

