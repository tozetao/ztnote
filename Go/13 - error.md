### 错误

Go程序使用error值来表示错误状态，error类型是一个内建接口。

```go
type error interface{
    Error() string
}
```

通常函数会返回一个error值，调用它的代码应该判断error是否为nil来进行错误处理。

```go
i, err := strconv.Atoi("42")
if err != nil {
    fmt.Printf("couldn't convert number: %v\n", err)
    return
}
fmt.Println("Converted integer:", i)
```

error为nil时表示成功，非nil的error表示失败。





### panic

不可恢复的错误称为panic，比如数组下标越位、引用空指针等。

当panic被调用后，程序将立刻终止当前函数的执行 ，并开始回溯goroutine的栈，运行任何被推迟（defer）的函数。若回溯到goroutine栈的顶端，程序就会终止。

example：

```go
func main() {
	f(3)
}

func f(x int) {
    defer fmt.Printf("defer %d\n", x)

	fmt.Printf("f(%d)\n", x + 0 / x)
	f(x - 1)
}
// 输出结果
// f(3)
// f(2)
// f(1)
// defer  0
// defer  1
// defer  2
// defer  3
```

该案例中f函数会一直递归，当实参x为0时，0/x会产生一个panic，这时中断函数的执行，向上回溯goroutine的栈，执行defer函数。





### recover

产生panic时程序会终止函数的运行并一直向上回溯goroutine的栈，而内建的recover函数可以取回goroutine的控制权来恢复程序运行。

调用recover将停止回溯过程，并返回传入panic函数的实参。由于在回溯时只有被推迟（defer）的函数执行，因此recover函数只能用在被推迟函数中。

recover函数会返回panic产生的error，我们可以针对error的类型进行相应的处理。



example：

recover的简单应用，在服务器中recover产生panic的goroutine，避免影响其他goroutine的正常运行。

```go
func server(workChan <-chan *Work) {
    for work := range workChan {
        go safelyDo(work)
    }
}

// recover panic，避免影响其他goroutine的执行。
func safelyDo(work *Work) {
    defer func() {
        if err := recover(); err != nil {
            log.Println("work failed: ", err)
        }
    }()
    do(work)
}
```



