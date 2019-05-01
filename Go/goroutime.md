### coroutine

协程可以看作是能够保存上下文，可让出执行但不可抢占的执行单元。

- 协程与线程

  线程是抢占式的，使用线程无法确定系统何时调度线程，何时切换线程。而协程是非抢占式的。在没主动交出CPU之前是不会被突然切换到其他协程上的，因此使用协程的好处是不用加锁，访问共享的数据不用进行同步操作。



- 协程与异步回调

  异步回调可以避免IO阻塞，但是需要将处理操作写在回调函数中，而回调函数会使连贯的业务代码拆分到多个回调函数中，增加维护的成本，而使用协程能够以同步的写法写入异步效果的代码。



实现分析：

实现协程主要是保存函数调用的上下文，函数调用上下文包括：

- 寄存器的值
- 函数调用栈

```c
int setjmp(jmp_buf buf);
```

将堆栈上下文环境保存在buf中，供longjmp()调用时使用。

setjmp()返回值依赖于longjmp传递，如果longjmp没有传递返回值将返回0，否则返回longjmp传递的值。



```c
void longjmp(jmp_buf buf, int val);
```

执行longjmp()可以跳转会之前执行setjmp()的地方，跳转是可以跨函数的。





### ucontext

ucontext.h基本使用流程：

- 获取上下文

- 设置上下文

  分配新堆栈，并定义后继上下文。

- 激活上下文

  在激活上下文时，将调用函数func，并传递遵循argc参数的一系列整数参数，调用者必须在argc中指定这些参数的数量。当此函数返回时，将激活后继上下文。如果后继上下文指针为NULL，则线程退出。



```c
int getcontext(ucontext_t *ucp);
```

将当前上下文信息保存到ucp中。

```c
void makecontext(ucontext_t *ucp, void (*func)(), int argc, ...);
```

修改ucp指向的上下文，绑定上下文要执行的函数。

在调用makecontext之前，调用者必须为此上下文分配一个新堆栈，并i将其地址分配给ucp->uc_stack，并定义后继上下文并将其地址分配给ucp->uc_link。

```c
int setcontext(const ucontext_t *ucp);
```

激活ucp指向的上下文。

```c
int swapcontext(ucontext_t *oucp, ucontext_t *ucp);
```

将当前上下文保存在oucp中，然后激活ucp指向的上下文。







setcontext()是跳转到指定的上下文中。

swapcontext()是在执行完当前上下文后，跳转到后继上下文的地方来执行代码。



实现逻辑：

一个存储执行协程执行单元的List。

- 创建一个协程

  会保存创建的上下文、保存主函数的上下文（即create_coro的函数结尾），然后激活创建的上下文，执行任务。

- 挂起一个协程

  一般会在任务中挂起一个协程。

  挂起一个协程时，会保存任务挂起的上下文，并激活主函数上下文。激活函数上下文，create_coro其实就执行完毕了；在后续中需要检查是否有协程需要执行。

- 恢复一个协程

  会激活挂起的上下文，恢复任务的执行，同时保存主函数的上下文。

  激活一个协程后，resume_coro函数就执行完毕了。

    

三个阶段中，协程单元都会保存每个阶段的挂起点。



执行完毕后，协程单元处于Free状态。



RUNNABLE：协程处于创建状态

SUSPEND：协程处于挂起状态

FREE：协程执行完毕。





create()

在create()内部执行当前上下文，然后激活主函数上下文。



yield()

yield会在协程所执行的任务函数中进行使用，yield会切换到主函数上下文，同时保存当前切换点的上下文。

因为使用一个全局调度器来保存主函数的上下文，所以yield第一次切换会跳转到create()函数保存的main函数的上下文，也就是create()函数会执行完毕。



swapcontext(coro, main);

切换回主函数，保存协程的切换点。



resume()

swapcontext(main, coro);

切换回协程，保存主函数的恢复点。





yield与resume是一对一的关系，yield必须resume，协程才会恢复执行。

第一次yield，create()函数会执行完毕。resume时，激活协程断点，恢复协程执行，同时记录main断点。

第二次yield，协程中断执行，记录中断断点，恢复到main断点执行；



main：保存主函数的恢复点。

coro：保存协程的中断点。



### goroutine

go协程是独立的处理单元，无法确定协程是什么时候开始执行的，因此代码逻辑必须独立与协程的调用顺序。

go协程提供了一种简单的方法来编写并发程序，它与协程的有所区别：





### channel

通道是一种用于发送类型化数据的管道，负责协程之间的通信。

数据在通道进行传递时，在任何给定时间，一个数据被设计为只有一个协程可以对其访问，所以不会发生数据争抢，通过这种设计避免了共享内存导致的竞争问题。







通道阻塞

通信是同步且无缓冲的。

一个没有缓冲的通道在没有空间保存数据时，发送者使用通道发送数据时，接收者必须准备好接收数据，否则将会阻塞。所以通道的发送和接收操作在对方准备好之前是阻塞的。

example：发送者阻塞

```go
func sendData(ch chan int) {
    fmt.Println("send 1")
    ch <- 1
    
    //发生阻塞
    fmt.Println("send 2")
    ch <- 2
    
    fmt.Println("send 3")
    ch <- 3
}

func getData(ch chan int) {
    fmt.Println(<- ch)
}

func main() {
    ch := make(chan int)
    go sendData(ch)
    go getData(ch)
}
```

example：接收者阻塞

```go
func sendData(ch chan int) {
    ch <- 1
}

func getData(ch chan int) {
    fmt.Println("receive 1")
    fmt.Println(<- ch)
    
    //发生阻塞
    fmt.Println("receive other")
    fmt.Println(<- ch)
}

func main() {
    ch := make(chan int)
    go sendData(ch)
    go getData(ch)
}
```



默认通道的容量是0，即未使用缓冲区的通道，通信只有在双方都准备好的情况下才成功。如果通道带缓冲区，它将是异步的，在缓冲区变满（发送）或变空（接收）之前通信是不会阻塞的。



信号量模式：在协程中，通过通道发送信号，告诉main协程处理已经完成。





### time

计时器，周期性的向Time通道发送时间。

```go
package time
func NewTicker(dur Duration)
func NewTick(dur Duration)
```

NewTicker()与NewTicker()都可以创建计时器，不同的是NewTicker()返回一个包含Time类型通道的结构体，而NewTick()会返回一个Time类型通道。

example：

```go
tick := time.NewTicker(1e9)

for {
    select {
        case val := <- tick.C:
        fmt.Println(val)
    }
}
```



定时器

time包中通过time.After()来创建定时器，它只会向Time通道发送一次时间。

```go
boom := time.After(5e8)

for {
    select {
        case <-boom:
        fmt.Println("boom")
        return;
    }
}
```







