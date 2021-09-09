### goroutine

在Go中，应用程序并发处理的部分是由Go协程来实现的。Go协程是独立的处理单元，多个Go协程可以并发的执行。

goroutine与普通协程的区别在于：

- Go协程通过通道来通信；普通协程通过让出和恢复来进行通信。
- Go协程是并行的，协程一般来说不是并行的。

因为goroutine是并行的，且由go来调度运行goroutine，所以编写代码的逻辑必须与goroutine的调用顺序无关。



example：并发演示

```go
func main() {
    go spinner(100 * time.Millisecond)
    const n = 45
    fibN := fib(n) // slow
    fmt.Printf("\rFibonacci(%d) = %d\n", n, fibN)
}

func spinner(delay time.Duration) {
    for {
        for _, r := range `-\|/` {
            fmt.Printf("\r%c", r)
            time.Sleep(delay)
        }
    }
}

func fib(x int) int {
    if x < 2 {
        return x
    }
    return fib(x-1) + fib(x-2)
}
```

spinner函数用于显示等待动画，我们通过go关键字开启一个协程负责运行该函数，然后程序会继续执行，计算斐波那契数列，直至计算完成，程序运行结束后，所有开启的协程也被关闭。





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







