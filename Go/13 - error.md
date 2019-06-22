### Go的错误

程序不能按照预期运行就是错误。

对于将运行失败看作是预期结果的函数，它们会返回一个额外的返回值，通常是最后一个，以此来传递信息。

如果导致失败的原因只有一个，额外的返回值是一个布尔值，一般是ok；如果失败的原因不止一种，尤其是I/O操作，用户需要了解更多的错误信息，额外的返回值就是error类型。



在Go中，预定义error接口类型用来表示错误类型。

```go
type error interface{
    Error() string
}
```

error类型如果是nil表示程序运行成功，如果是non-nil表示程序运行失败。



Go认为，在函数运行失败时返回的错误信息是一种预期的值，而不是异常，因此Go使用控制流程来处理错误。

这样设计的原因是对于某个应该在控制流程中处理的错误而言，将其当作异常抛出会混乱对错误的描述，并且这个异常会将堆栈信息返回给终端用户，这些复杂且无法快速定位错误。

注：错误值表示异常状态，错误内容表示具体的异常信息。



### 错误处理

错误处理一种有5种方式：

- 传递错误

  函数执行失败时返回错误信息是处理错误的一种方式。

  被调函数f(x)会将调用信息（函数名作为前缀）和参数信息作为发生错误时的上下文，放在错误信息中并返回给调用者。

  当错误最终由main函数处理时，错误信息应当清晰的显示从原因到后果的因果链。

  ```go
  doc, err := html.Parse(resp.Body)
  resp.Body.Close()
  if err != nil {
      return nil, fmt.Errorf("parsing %s as HTML: %v", url,err)
  }
  ```

- 重新尝试失败的操作

  这种错误一般发生在请求服务的程序，例如建立一个链接可以以若干次数一定频率进行重试。


- 输出错误信息并结束程序

  这种处理方式只能在mian函数中执行，对于库函数发生错误时应该向上传递，除非该错误意味着程序内部包含不一致性，即bug，才能在库函数中结束程序。

- 输出错误

  通过标准错误流输出错误，或者通过log包提供的函数输出错误，这种方式不中断程序的运行。

  ```go
  if err := Ping(); err != nil {
      log.Printf("ping failed: %v; networking disabled",err)
  }
  ```

- 忽略错误







### panic异常

运行时错误会引起panic异常，比如数组下标越位、引用空指针等。

当发生panic时，会马上中断当前函数的执行，同时所有的defer语句会被执行，然后将控制权交还给接收到panic的函数调用者。这样向上冒泡到最顶层，并执行每层的defer，在栈顶处程序崩溃并输出日志信息。

日志信息包括函数调用的堆栈跟踪信息和panic value。

```go
func main() {
	f(3)
}

func f(x int) {
	fmt.Printf("f(%d)\n", x + 0 / x)
	defer fmt.Printf("defer %d\n", x)
	f(x - 1)
}
```

panic会引起程序崩溃，一般用于严重错误，对于大部分可预期的错误，应该使用Go的错误处理机制。

注：对于每个goroutine，日志信息都会有与之相对的，发生panic时的函数调用堆栈信息。



### recover恢复异常

recover内置函数用于从panic异常中恢复，它能够使程序重新获得控制权，停止终止过程而恢复程序正常运行。

recover只能在defer修饰的函数中使用，如果发生了panic，recover会使程序从panic异常中恢复并返回panic value，导致panic的函数不会继续运行，但能正常返回；如果是正常执行，调用recover会返回nil。

说明：程序发生panic时会执行defer语句，因此在defer函数中捕获panic。

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







