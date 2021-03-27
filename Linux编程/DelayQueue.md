### fork



```c
pid_t fork(void);
```

fork出一个子进程。程序再执行fork()函数时会在父子进程中各返回一次。在父进程中返回子进程的pid，在子进程中返回的值为0。

fork()函数在实现的实现，会把当前父进程的所有相关值克隆一份，包括地址空间、文件描述符、程序计数器等，就连执行代码也会拷贝一份。新派生出的子进程与父进程近乎一样，为了区别俩个进程，实现者通过改变fork函数的栈空间值来判断。对应到程序就是返回值不同。







### wait

一个子进程退出时，系统内核还会保留该进程的若干信息，比如退出状态。这样的进程如果不回收就会变成僵尸进程。在linix中，僵尸进程会被挂到进程号为1的init进程上。

所以由父进程派生出来的子进程必须被回收，否则子进程就变成僵尸进程，占用系统内存空间。

处理子进程退出的方式一般是注册一个信号处理函数，补获信号SIGCHILD信号，在处理函数中调用waitpid函数来完成对子进程资源的回收。

- SIGCHILD：子进程退出或中断时内核向父进程发出的信息，该信号的默认处理是忽略。



```c
pit_t wait(int *statloc);
```

函数返回俩个值，一个是函数返回值，标识已终止的子进程的进程id。另一个是statloc指向返回的子进程终止的实际状态。这个值可能为正常终止、被信号杀死、作业控制停止等。

对于wait函数，如果有一个或多个子进程在正常运行，那么wait将会阻塞，直到第一个进程终止。如果没有子进程来运行，该函数将返回-1。



```c
pit_t waitpid(pid_t pid, int *statloc, int options);
```

waitpid函数是wait的加强版本，它提供了更多的参数。

- pid

  允许指定任意要等待终止的进程ID，值-1表示等待第一个终止的子进程。函数返回退出的子进程id，发生错误时返回-1（即没有运行的子进程）。

options有以下选项：

- WNOHANG

  函数不会阻塞，并且没有子进程时返回0。

  函数错误默认返回-1，当提供了此参数后函数错误返回0。这里的错误是指不存在子进程。

- WEXITSTATUS





简单的说这俩个函数都会阻塞当前进程，直到有某个子进程退出。注意啊，不要把wait与信号搞混了，wait函数是父进程阻塞等待处理子进程的退出消息，用于回收子进程的资源。

而SIGCHLD信号是子进程退出时由内核先父进程发送SIGCHLD信号，倆者不是同一个概念。





example：让多个进程在同一个时间死亡，测试是否能够完全回收所有子进程。

```c
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <wait.h>

int main()
{
    for(int i = 0; i < 5; i++) {
        if (fork() == 0) {
            printf("fork child proces: %d\n", i+1);
            sleep(5);
            printf("exit\n");
            exit(0);
        }
    }

    int pid = 0;
    while (1) {
        pid = waitpid(-1, 0, WNOHANG);
        printf("handle a process, pid: %d\n", pid);
        sleep(1);
    }

    return 0;
}

```



### pcntl_signal

pcntl_signal无法将信号直接注册到操作系统信号设置中，所以pcntl信号需要依赖tick机制来实现。

具体原理时触发信号后将信号加入一个队列，然后在php的ticks回调函数中不断检查是否有信号，如果有则调用回调函数，如果没有则跳出函数。

```php
declare(ticks = 1);	// 注释掉这一行将无法补获信号。

function sig_handler($no) {
    switch($no) {
        case SIGUSR1: echo "sigusr1\n"; break;
        case SIGUSR2: echo "sigusr2\n"; break;
        default: echo "unknow\n";
    }
}

pcntl_signal(SIGUSR1, 'sig_handler');
pcntl_signal(SIGUSR2, 'sig_handler');

posix_kill(posix_getpid(), SIGUSR1);
posix_kill(posix_getpid(), SIGUSR2);
```

更好的做法是使用pcntl_signal_dispatch函数来实现。

```php
<?php

function signalHandler($no) {
    switch($no) {
        case SIGUSR1:
            echo "SIGUSR1\n";
            break;
        case SIGUSR2:
            echo "SIGUSR2\n";
            break;
        default:
            echo "unknow\n";
    }
}

pcntl_signal(SIGUSR1, "signalHandler");
pcntl_signal(SIGUSR2, "signalHandler");

while (true) {
    sleep(2);
    posix_kill(posix_getpid(), SIGUSR1);
    posix_kill(posix_getpid(), SIGUSR1);
    posix_kill(posix_getpid(), SIGUSR2);

    pcntl_signal_dispatch();
}

```

pcntl_signal_dispatch()会调用每个等待信号注册的处理器。

上面的代码可以看出信号确实是存储在队列中的，同样发出了俩次信号，PHP都能够处理。



对于pcntl_signal_dispatch()函数的理解。这个函数就是让我们手动的去检查是否有可以处理的信号，如果有就执行，否则忽略。



example：信号是可以中断阻塞函数的

```php
function handler($no){
    echo "receive signal: " . $no ."\n";
}

pcntl_alarm(2);
pcntl_signal(SIGALRM, "handler");

echo "sleep 10 seconds\n";
sleep(10);
echo "The process was interrupted\n";

pcntl_signal_dispatch();
```



















实现：

PHP怎么回收子进程？

PHP socket实现

server怎么写入到delay bucket的？

timer读取delay bucket时，bucket的数量有多少个，会发生多个timer读取同一个bucket吗？

consume消费进程可以有多个，但是ready queue有多少个？多个进程消费一个queue，是否会发生重复消费。





线程：wait notify

https://bbs.csdn.net/topics/397425361

https://www.jianshu.com/p/b16296e9ac85



线程：生产者-消费者

https://bbs.csdn.net/topics/394368155

https://blog.csdn.net/weixin_39656575/article/details/80720809

https://blog.csdn.net/zgrjkflmkyc/article/details/8971894

https://blog.csdn.net/qq_41681845/article/details/89715560?utm_medium=distribute.pc_relevant_t0.none-task-blog-BlogCommendFromMachineLearnPai2-1.control&dist_request_id=1328740.10883.16168121928755001&depth_1-utm_source=distribute.pc_relevant_t0.none-task-blog-BlogCommendFromMachineLearnPai2-1.control



进程与线程

http://c.biancheng.net/cpp/u/xitong/



https://www.bilibili.com/video/BV1Xk4y1q7RK?from=search&seid=3676160698073698957