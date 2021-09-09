

### channel

通道是一种用于发送类型化数据的管道，负责协程之间的通信。

数据在通道进行传递时，在任何给定时间，一个数据被设计为只有一个协程可以对其访问，所以不会发生数据竞争，通过这种设计避免了共享内存导致的竞争问题。



### 行为

一个channel有发送和接收俩个操作，都是通信行为。

发送和接收操作都是使用 ```<-``` 操作符：

```go
channel <- x			//send
result := <-channel		//receive
```



一个未指定容量的channel称为无缓冲的channel。对于一个无缓冲的channel来说，接收和发送操作将导致俩个goroutine发生一次同步操作。

也就是说，在一个无缓冲的channel上执行发送操作将会导致发送者goroutine阻塞，直到有另一个goroutinue在相同的channel上成功接收到数据，俩个goroutinue才能继续执行后面的语句。

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
	//接收者睡眠2秒再进行接收数据，观察发送者的阻塞情况
	fmt.Println("sleep 2 second")
	time.Sleep(time.Duration(2) * time.Second)
	<-channel
	fmt.Println("after receive")
}
```

反之一个goroutinue在channel上先触发接收操作，那么它也会被阻塞。直到有另外个goroutinue在同个channel上发送数据。

```go
func main() {
	channel := make(chan int)
	go send(channel)
	go receive(channel)

	time.Sleep(time.Duration(5) * time.Second)
}

func send(channel chan int) {
	//接送者睡眠2秒再进行接收数据，观察接收者的阻塞情况
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





### 带缓冲的channel

带缓冲的channel持有一个元素队列，在创建channel时指定容量的大小就可以得到带有缓冲的channel。

向channel发送数据就是向管道队列的尾部插入数据，接收channel数据就是从队列头部获取数据。如果队列是空的，发送操作将会执行到队列满时阻塞；如果队列是满的，接收操作将会执行到队列为空时阻塞。

```go
func send() {
	numberChan := make(chan int, 2)

	numberChan <- 1
	fmt.Println("send 1")

	numberChan <- 2
	fmt.Println("send 2")

	numberChan <- 3
	fmt.Println("send 3")
}
```





### 单向的channel

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



### 关闭的channel

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

select是多路复用，即能够检测多个channel是否可执行的非阻塞操作。

select语句的写法与switch语句类似，都是有几个case和最后的default分支，比如：

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

每个case代表一个channel通信操作，可以是通道发送或通道接收。

select会等待case中有能够执行的channel，当有非阻塞的channel时，select才会去通信并执行case之后的语句。对于一个没有任何case的select会永远等待下去。

default分支表示当其他case都不满足条件时，将会默认执行的代码部分。



如果多个case同时就绪时，select会随机的选择一个执行，来保证每个channel都有平等的被select执行的机会。

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



### goroutine泄漏

在go协程使用通道通信时，需要注意通道是否会因为阻塞而导致无法被垃圾回收。

如果因为通道阻塞而导致整个协程阻塞，阻塞的通道和协程无法回收的话，就会造成goroutine泄漏。

