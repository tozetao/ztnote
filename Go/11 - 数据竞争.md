并发概念：

在单线程的程序中，程序的执行顺序只有程序的逻辑来决定，而在多个goroutine的程序中，每个goroutine内的语句也是按照顺序执行的，当时你无法确定在goroutine中的事件x、y的执行顺序，也就是事件x、y是并发的。

- heppens before

  在讨论并发编程时，当我们说事件x在y事件之前发生，我们并不是说事件x在事件上比事件y更早，我们要表示的意思是要保证在此之前的事件已经完成。

  比如在此之前更新变量的某些前置操作已经完成，那么你就可以放心的依赖这些完成的事件。

- 并发

  当我们说x事件既不是在y事件之前发生也不会在y事件之后发生，就说x事件和y事件是并发的。这并不是意味着事件x和y就一定是同时发生的，我们只是无法确认这俩个事件发生的先后顺序。



### 数据竞争

- 概念

  在俩个以上的goroutine并发访问相同的变量且至少其中一个为写操作时就会发生数据竞争。

- 解决方式

  避免多个goroutine访问公有变量，而是由一个goroutine监控该公有变量，该goroutine称为监控goroutine。

  在程序运行期间，只能由该监控goroutine读取或更新该变量，其他goroutine只能通过一个channel来发送请求给监控goroutine，以此来查询更新变量。

example：模拟多个人存钱到同一个账户

```go
// 负责充值的channel，同一时间只有一个goroutine能够
var deposits = make(chan int32)

// 负责读取余额的channel
var balances = make(chan int32)

func main()  {
	// 演示多个人存钱
	for i := 0; i < 5; i++ {
		amount := int32(i + 10)
		go deposit(int32(i), amount)
	}

	go teller()

	time.Sleep(5 * time.Second)
}

func teller() {
	// 总余额
	var balance int32

	for {
		select {
			case amount := <-deposits:
				balance += amount
				fmt.Printf("The current balance is %d\n", balance)
			case balances <- balance:
		}
	}
}

func deposit(uid, amount int32) {
	deposits <- amount
}

func balance() int32 {
	return <-balances
}
```





### 二元信号量

可以用一个容量1的channel，创建一个最多只能由一个goroutine在同一个时刻访问一个共享变量，一个只能为1和0的信号量叫做二元信号量。

```go
var (
    balance int64
    sema = make(chan struct{}, 1)
)

func deposit(amount int) {
    sema <- struct{}{}
    balance += amount
    <-sema
}
```





### 互斥锁

sync.Mutex类型实现了互斥锁，互斥锁其实就是二元信号量的实现，它实现了同一时刻只能由一个goroutine去执行加锁和解锁之间的代码块。

- 临界区

  在Lock和Unlock之间的代码内容goroutine可以随意读取和修改，这个代码段叫做临界区。

- mutex不可重入

  Go认为互斥量是不可冲入的，也就是说如果对一个互斥量进行了加锁，那么在没有互斥量锁的时候，再次加锁会造成永久阻塞。

example：

```go
var (
    mu sync.Mutex
    balance int
)

func deposit(amount int) {
    defer mu.Unlock()
    
    mu.Lock()
    //执行逻辑
}
```





### 读写锁

允许多个只读操作并行执行，但是写操作会完全互斥，这种锁叫做"多读单写锁"。即读读不互斥、读写互斥。

Go的sync.RWMutex提供了读写锁类型。

```go
var (
	se sync.RWMutex
	total int = 1
)

func RLock() int {
	defer se.RUnlock()
	se.RLock()
	return total
}

func WLock() {
	defer se.Unlock()
	se.Lock()
	total += 100
	time.Sleep(2 * time.Second)
}

func main()  {
    //读读不互斥
    RLock()
    RLock()
    
    //读写互斥
	WLock()
	RLock()
}
```

RWMutex只有在读写操作情况下，并且并发读的次数比并发写的次数多的情况下，效率才比Mutex高。





### sync.Once

sync.Once能够保证在并发初始化变量的时候，只进行一次初始化。

从概念上将，一次性初始化需要一个互斥量和一个boolean变量来记录初始化是不是完成了，boolean变量默认为false，互斥量保证boolean变量和客户端结构。在第一次初始化后boolean变量会变为true，后续的调用不会做任何操作。

```go

var (
	userMap = make(map[string]int, 10)
	loadUserMapOnce sync.Once
)

func User(name string) int {
	loadUserMapOnce.Do(loadUserMap)
	return userMap[name]
}

func loadUserMap() {
	userMap["a"] = 10
	userMap["b"] = 25
	userMap["c"] = 35
}
```

其实一次性初始化使用读写锁来实现也可以，只不过较为复杂，因为Go实现了一次性初始化。



