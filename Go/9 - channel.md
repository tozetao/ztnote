### chan

chan是Go独有的数据类型，是一种用于发送类型化数据的管道，有发送和接收俩个操作，用于协程之间的通信。



### 非缓冲通道

一个未指定容量的chan称为非缓冲通道。

对于非缓冲通道，无论时发送操作还是接收操作，一开始执行就会被阻塞，直到配对另一方开始执行，数据才能继续传递。

因此非缓冲通道是在用同步的方式传递数据，只有收发双方对上了，数据才传递成功。相比之下缓冲通道是在用异步的方式传递数据。

example：

```go
func main() {
	channel := make(chan int)
	go send(channel)
	go receive(channel)

	time.Sleep(time.Duration(5) * time.Second)
}

func send(channel chan int) {
	fmt.Println("before send")
	channel <- 100
	fmt.Println("after send")
}

func receive(channel chan int) {
	fmt.Println("sleep 2 second")
	time.Sleep(time.Duration(2) * time.Second)
	<-channel
	fmt.Println("after receive")
}
```

观察运行状况可以发现发送和接收是一个同步行为，如果接收方未接收到通道中的数据，那么发送方也会阻塞发送代码那里，直到接收方收到数据。

反之在通道上先触发接收操作，那么它也会被阻塞，直到有发送方在同个channel上发送数据。

```go
func main() {
	channel := make(chan int)
	go send(channel)
	go receive(channel)

	time.Sleep(time.Duration(5) * time.Second)
}

func send(channel chan int) {
	//发送方睡眠2秒再进行接收数据，观察接收者的阻塞情况
	fmt.Println("sleep 2 second")
	time.Sleep(time.Duration(2) * time.Second)
	channel <- 100
	fmt.Println("sending done.")
}

func receive(channel chan int) {
	fmt.Println("before receive")
	<-channel
	fmt.Println("after receive")
}
```









### 缓冲通道

带缓冲的channel持有一个元素队列，在创建channel时指定容量的大小就可以得到带有缓冲的channel。

向channel发送数据就是向管道队列的尾部插入数据，接收channel数据就是从队列头部获取数据。

对于缓冲通道，如果通道已满，那么针对它的所有发送都会被阻塞，直到通道中有元素被取走。同时发送方在阻塞时，它们所在的goroutine会顺序的放入到通道内部中的等待发送队列，所以唤醒时通知的顺序总是公平的。

相对的，如果通道为空，那么对它的所有接收操作都会被阻塞，直到通道中有新的元素出现。这时Go运行时系统会通知最早等待接收数据的那个goroutine，并使它再次执行接收操作。

同样的通道也有等待接收队列，当接收方触发阻塞时，所在的goroutine也会顺序放入到等待接收队列中。

注：跟非缓冲通道最大的不同点是缓冲通道是使用异步的方式来发送数据的，如果缓冲通道未满，发送方在发送完元素之后可以继续往下执行代码，不用等到接收方处理发送的数据。

example：

```go
func send() {
	numberChan := make(chan int, 2)
    // 不阻塞
	numberChan <- 1
	fmt.Println("send 1")
    // 不阻塞
	numberChan <- 2
	fmt.Println("send 2")

    // 阻塞
	numberChan <- 3
	fmt.Println("send 3")
}
```



example：等待发送队列的测试

```go
ch := make(chan int, 2)

ch <- 10
ch <- 20

// 这几个goroutine发送时阻塞的顺序与接收顺序总是一致的。
for i := 0; i < 3; i++ {
    go func(i int) {
        fmt.Printf("goroutine i = %d\n", i)
        ch <- i
    }(i)
}

time.Sleep(time.Second * 3)
for i := 0; i < 5; i++ {
    val, ok := <- ch
    fmt.Println("val: ", val, ", ok: ", ok)
}
```









### 单向通道

单向的channel指的是channel只能用于发送或者接收。

典型的应用场景是：当一个channel作为一个函数参数时，它一般总是被专门用于只发送或只接收。

```go
func main() {
	var ch = make(chan string)

	for i := 0; i < 3; i++ {
		go download(ch, "a.com/" + string(i + '1'))
	}
	receive(ch)
}
// 只用于写
func download(ch chan <- string, url string) {
	ch <- url
}

// 只用于读
func receive(ch <-chan string) {
	for i := 0; i < 3; i++ {
		msg := <-ch
		fmt.Println("finish url: ", msg)
	}
}
```



### 关闭的通道

channel是可以关闭的。

对于一个关闭的channel，基于该channel的任何发送行为都会引发panic，但是可以对一个关闭的channel执行接收操作，并且可以接收到已经发送成功的数据。

如果channel中已经没有数据，接收者goroutine继续对该channel执行接收操作不会阻塞，并且会返回空值（通道对应类型的初始值）。

```go
func main() {
	var ch = make(chan int)
	go send(ch)
	receive(ch)
}

func send(channel chan int) {
	for i := 0; i < 5; i++ {
		channel <- i
	}
	close(channel)
}

func receive(channel chan int) {
	for {
		val, ok := <-channel
		fmt.Println("val: ", val, ", ok: ", ok)
		if ok {
			continue
		}
		break
	}
}
```



### select

select是多路复用，能够检测多个通道是否有可以执行的非阻塞操作。select语句的写法与switch语句类似，都是有几个case的候选分支和最后的default分支，比如：

```go
select {
	case <-ch1:
    	//...
    case ch <- x:
    	//...
    default:
    	//...
}
```

select分支的选择规则：

- 对于每一个case表达式，都至少包含一个发送操作或接收操作。

- 包含的候选分支的case表达式先求值，并且求值的顺序是依从代码编写的顺序从上到下的。

- 对于每一个case表达式，如果其中的发送表达式或接收表达式在求值时，相应的操作正处于阻塞状态，那么该case表达式的求值就是不成功的。

- 仅当select语句中的所有case表达式都被求值后，它才会开始选择候选分支。如果没有满足条件的候选分支可以执行，则会选择默认分支执行。

  如果这时没有默认分支，那么select会被阻塞。直到候选分支中有满足选择条件，select语句才会被唤醒，该候选分支才会被执行。

- 如果select语句发现同时有多个候选分支满足选择条件，那么它就会用一种伪随机的算法在这些分支中选择一个并执行。注意即使是在select被唤醒时碰到的这种情况，也会这样做。

example：

```go
//运行下面代码n次，可以发现channel的选择是随机的。
func main() {
    abort := make(chan struct{})
	x := make(chan int)
	y := make(chan string)

	go func() {
		os.Stdin.Read(make([]byte, 1))
		abort <- struct{}{}
	}()

	go func() {
		x <- 100
	}()

	go func() {
		y <- "hello"
	}()

	select {
		case n := <-x:
			fmt.Println(n)
		case s := <-y:
			fmt.Println(s)
		case <-abort:
			fmt.Println("abort")		
	}
}
```



### example

example：并行的好处

```go
var ch = make(chan string)

func main() {
	for i := 0; i < 3; i++ {
		go download("a.com/" + string(i + '1'))
	}
	for i := 0; i < 3; i++ {
		msg := <-ch
		fmt.Println("finish url: ", msg)
	}
}

func download(url string) {
	//模拟下载阻塞一秒钟
	fmt.Println("start to download", url)
	time.Sleep(time.Second)
	ch <- url
}
```

如果没有goroutinue，那么任务将会串行执行，需要3秒钟。在Go的并发处理中，每当一个goroutinue阻塞时就会调度执行其他goroutinue，以此类推，程序就只需要1秒钟的执行时间。



example：

```go
var ch = make(chan string)

func main() {
	for i := 0; i < 3; i++ {
		go download("a.com/" + string(i + '1'))
	}
	for i := 0; i < 3; i++ {
		msg := <-ch
		fmt.Println("finish url: ", msg)
		fmt.Println("sleep 1 second")
		time.Sleep(time.Second)
	}
}

func download(url string) {
	ch <- url
}
```

这个实例说明了接收者的处理速度决定了发送者的发送效率。

