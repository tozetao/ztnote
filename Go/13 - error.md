### Error

Go认为，如果函数运行失败是预期的结果，那么应该返回错误而不是异常。

这样设计的原因是对于某个应该在控制流程中处理的错误而言，将其当作异常抛出会混乱对错误的描述，并且这个异常会将堆栈信息返回给终端用户，这些复杂且无法快速定位错误。



在Go中，预定义了error接口类型用来表示错误类型。

```go
type error interface{
    Error() string
}
```

error类型如果是nil表示程序运行成功，如果是non-nil表示程序运行失败。





### panic

不可预知的错误称为panic，比如数组下标越位、引用空指针等。

当发生panic时，会马上中断当前函数的执行，同时所有的defer语句会被执行，然后将控制权交还给接收到panic的函数调用者。

这样向上冒泡到最顶层，并执行每层的defer，在栈顶处程序崩溃并输出日志信息。日志信息包括函数调用的堆栈跟踪信息和panic value。

```go
func main() {
	f(3)
}

func f(x int) {
    defer fmt.Printf("defer %d\n", x)
    
	fmt.Printf("f(%d)\n", x + 0 / x)
	f(x - 1)
}
```

panic会引起程序崩溃，一般用于严重错误，对于大部分可预期的错误，应该使用Go的错误处理机制。

注：对于每个goroutine，日志信息都会有与之相对的发生panic时的函数调用堆栈信息。



### recover

recover函数用于从panic异常中恢复，它能够使程序重新获得控制权，停止终止过程而恢复程序正常运行。

recover只能在defer修饰的函数中使用，如果发生了panic，recover会使程序从panic异常中恢复并返回panic value，导致panic的函数不会继续运行，但能正常返回；如果是正常执行，调用recover会返回nil。

说明：因为程序发生panic时会执行defer语句，所以在defer函数中捕获panic。

```go
func main() {
	fmt.Printf("Calling test\r\n")

	test()

	//由于test()函数捕获了panic异常，因此后续的代码能够正常执行。
	fmt.Printf("Test completed\r\n")
}

func badCall() {
	panic("bad end")
}

func test() {
	defer func() {
		if e := recover(); e != nil {
			fmt.Printf("Pancing %s\n", e)
		}
	}()

	badCall()

	//这里不会被执行到
	fmt.Println("after bad call")	
}
```







