单个协程的通知：利用channel，可以实现一个goroutine通知另一个goroutine任务是否完成。

```go
func master(in chan int) {
	for index := 0; index < 10; index++ {
		time.Sleep(1 * time.Second)
		fmt.Println("The current number is ", index)
		if index == 5 {
			in <- index
		}
	}
}

func slave(out chan int) {
	fmt.Println("Waiting to receive data from the master")
	v := <-out
	fmt.Println("received number ", v)
}

func TestCoroutine(t *testing.T) {
	numbers := make(chan int)

	go master(numbers)
	go slave(numbers)

	time.Sleep(10 * time.Second)
}
```



多个协程针对一个协程的通知：比如多个Worker协程通知Main协程任务完成度。

```go
func ManyToOne() {
	var total int
	var wg sync.WaitGroup
	sizes := make(chan int)

    // for循环创建多个工作协程
	for i := 1; i <= 10; i++ {
		wg.Add(1)
		go func(n int) {
			defer wg.Done()
			sizes <- n
		}(i)
	}
    
    // 一个关闭协程，通过计数器，监听工作协程是否运行完毕
	go func() {
		wg.Wait()
		close(sizes)
	}()

    // 主协程负责接收工作协程的结果
	for n := range sizes {
		total += n
	}

	fmt.Println("total: ", total)
}
```



并发的退出

指的是终止多个worker协程的退出，这里的终止指的是终止发出终止信号后产生的worker协程，而无法终止已经在运行的worker协程。

在实现上会通过一个done channel对所有的worker协程广播，对于广播之后生成的worker协程，它们将会停止执行。

```go
func main() {
    // worker协程
	go func() {
		for i := 0; i < 100; i++ {
			go func(m int) {
				if cancelHandle(doneChan) {
					fmt.Println("terminate: ", m)
					return
				}
				fmt.Println("i am a worker, number: ", m)
			}(i)
            
            // 为了观察效果，设置睡眠1秒
			time.Sleep(1 * time.Second)
		}
	}()

	// done 协程
	go func() {
		os.Stdin.Read(make([]byte, 1))
		close(doneChan)
	}()
    
    time.Sleep(20 * time.Second)
}
```





实现计数信号量：利用带有缓存的channel，可以实现限制并发的计数信号量。

```go
var sema = make(struct{}, 20)

//最多会有20个goroutine同时运行，其他会处于阻塞状态
for i := 0; i < 100; i++ {
	go func(){
        defer <-sema
        sema <- struct{}{}
    }()
}
```

