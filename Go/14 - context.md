### sync.Context

Context包提供一种上下文机制，能够在goroutine之间传递deadlline、取消信号（cancellation signals）或者其他请求相关信息。

Context接口：

```go
type Context interface {
    
    Done() <-chan struct{}
    
    Deadline() (deadline time.Time, ok bool)
    
    Err() error
    
    Value(key interface{}) interface{}
}
```

```go
Done() <-chan struct{}
```

Done方法会返回一个chan，用于阻塞当前运行的代码，直到以下条件发生时，channel才会被关闭，进而解除阻塞。

- WithCancel创建的context返回的CancelFunc被调用。该context以及派生子context的Done channel都会收到该取消信号。
- WithCancel创建的context，deadline到期
- WithCancel创建的context，timeout到期

Done方法需要配合select语句使用。



```go
Deadline() (deadline time.Time, ok bool)
```

WithDeadline方法会给context设置deadline（截至时间），调用Deadline()返回deadline的值。如果没设置ok返回值为false。

该方法可以用于检查当前时间是否临近deadline。



```go
Err() error
```

如果Done的channel被关闭了，Err函数会返回一个error，说明错误原因：

- 如果channel是因为被取消而关闭，打印canceled
- 如果channel是因为deadline到期了，打印deadline exceeded

重复调用返回相同值。



```go
Value(key interface{}) interface{}
```

返回由WithValue()关联到context的值。



#### 创建Context

```go
Background() Context
```

该方法返回一个根Context。根Context不会被cancel，这个方法只能用于最外层函数中，比如main函数。



#### 派生Context

一个Context被cancel，那么它的派生context都会收到取消信号，表现为context.Done()返回的channel收到值。有四种方法派生Context：

```go
func WithCancel(parent Context) (ctx Context, cancel CancelFunc)
```

最常用的派生context方法。该方法接收一个父context，父context可以是Background context或者其他context，返回CancelFunc。如果被调用会导致Done channel关闭，因此绝对不要把cancelFunc传递给其他方法。



```go
func WithDeadline(parent Context, d  time.Time) (Context, CancelFunc)
```

该方法会创建一个带有deadline的context。当deadline到期后，该context	以及该context派生的子context都会收到cancel通知。

另外如果提前调用Cancel'Fun'c则会提前发送取消通知。



```go
func WithTimeout(parent Context, timeout time.Duration) (Context, CancelFunc)
```

与WithDeadline类似。



```go
func WithValue(parent Context, key, val interface{}) Context
```

该方法会创建一个携带信息的context，可以是user信息、认证token。该context与其派生的子context都会携带这些信息。

参数key是信息的唯一key，该key类型不应对外暴漏，为了避免与其他包可能的key类型冲突，所以使用WithValue应该像下面例子的方式间接调用WithValue。

参数val则是真正存储到context中的值。

```go
package user

import "context"

// User 类型对象会被保存到 Context 中
type User struct {
    // ...
}

// key 不应该暴露出来。这样避免与包中其他 key 类型冲突
type key int

// userKey 是 user 的 key，不应暴露; 
// 通过 user.NewContext 和 user.FromContext 间接使用 key
var userKey key

// NewContext 返回携带 u 作为 value 的 Context
func NewContext(ctx context.Context, u *User) context.Context {
    return context.WithValue(ctx, userKey, u)
}

// FromContext 返回关联到 context 的 User类型的 value 的值
func FromContext(ctx context.Context) (*User, bool) {
    u, ok := ctx.Value(userKey).(*User)
    return u, ok
}
```



#### example

```go
package main

import (
	"context"
	"fmt"
	"time"
)

type Cake struct {
	state string
}

func baker(cooked chan<- *Cake) {
	for {
		cake := new(Cake)
		cake.state = "cooked"
		cooked <- cake
	}
}

func ice(cooked chan<- *Cake) {
}

func sleepRandom1(stopChan chan struct{}) {
	i := 0
	for {
		time.Sleep(1 * time.Second)
		fmt.Printf("This is sleep Random1: %d\n", i)
		i++
		if i == 5 {
			fmt.Println("cancel sleep Random1")
			stopChan <- struct{}{}
			break
		}
	}
}

func sleepRandom2(ctx context.Context) {
	i := 0
	ctxChild, _ := context.WithCancel(ctx)
	go sleepRandom3(ctxChild)

	for {
		time.Sleep(1 * time.Second)
		fmt.Printf("This is sleep Random2: %d\n", i)
		i++

		select {
		case <- ctx.Done():
			fmt.Printf("Why? %s\n", ctx.Err())
			fmt.Println("cancel sleep Random2")
			return
		default:
		}
	}
}

func sleepRandom3(ctx context.Context) {
	i := 0
	for {
		time.Sleep(1 * time.Second)
		fmt.Printf("This is sleep Random3: %d\n", i)
		i++

		select {
		case <- ctx.Done():
			fmt.Printf("Why? %s\n", ctx.Err())
			fmt.Println("cancel sleep Random3")
			return
		default:
		}
	}
}

func main() {
	ctxParent, cancelParent := context.WithCancel(context.Background())
	ctxChild, _ := context.WithCancel(ctxParent)

	stopChan := make(chan struct{})

	go sleepRandom1(stopChan)
	go sleepRandom2(ctxChild)

	// 等待Random1运行结束
	select {
	case <- stopChan:
		fmt.Println("stopChan received")
	}
	// 通知Random2所在goroutine以及派生的goroutine停止运行
	cancelParent()

	for {
		time.Sleep(time.Second * 1)
		fmt.Println("Continue...")
	}
}
```

