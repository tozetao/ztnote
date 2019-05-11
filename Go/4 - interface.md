### Interface

接口可以定义一组抽象（不具体实现）方法来说明对象的行为，所以接口是一种抽象类型。

接口按照如下格式进行定义：

```go
type Namer interface {
    methods(param_list) return_type
}
```



Go的多态：

接口的方法由具体类型来实现，当某个类型的实例实现了接口定义的方法，那么该接口类型的变量可以指向这个实例的引用，并且该变量可以调用实例实现了接口定义的方法。

这便是多态，是同一种类型在不同实例上表现出不同的行为。

```go
type Areaer interface {
    func Area();
}

struct Square {
    SideLength int
}

struct Triangle {
    BottomLength int
    Height int
}

func (self Square) Area() {
    return self.SideLength * self.SideLength
}

func (self Triangle) Area() {
    return self.Height * self.BottomLength / 2
}
```





### 类型断言

类型断言指在运行期间，判断接口类型的变量是哪种具体类型。

```go
v, err := interfaceVar.(T)
```

interfaceVar是某种接口类型的变量，T是要转换的类型。如果转换合法，v是interfaceVar转换到类型T的值，err会是true；否则v是类型T的零值，ok是false，也不会发生运行时错误。

注：在做类型断言处理时，总是要处理错误。





### 接口类型与接收者

在实现接口定义的方法时，实现方法的接收者类型决定了接口能够被赋值的类型。如果接收者是值类型，那么接口变量可以接收值变量和指针变量；如果接收者是指针类型，那么接口变量只接收指针变量。

```go
type List []int

type Appender interface {
	Append(int)
}

type Lener interface {
	Len() int
}

func (this *List) Append(val int) {
	*this = append(*this, val)
}

func (self List) Len() int {
	return len(self)
}

func main() {
	var llst Lener;
	var alst Appender

    lst := new(List)
	alst = lst
	llst = lst
    
	var lst2 List
	alst = lst2		//编译报错，
	llst = lst2
}
```



### 空接口

任何其他类型都实现了空接口，空接口类似于Java中的Object类，因此空接口变量可以赋予任何类型的值。

- 空接口的长度

  空接口变量在内存中占用2个字长：一个用于存储它包含的类型，另一个用来存储它包含的数据或者指向数据的指针。

利用空接口的特性，可以构建通用类型的数据结构，例如通用型的数组、节点。

example：通用类型的切片

```go
type Element interface {}

func main() {
    //构建一个通用类型的切片
    elements := make([]Element, 10)
    elements[0] = 100
	elements[1] = "hello world"
	fmt.Printf("%d %s\n", elements[0], elements[1])
}
```

example：通用类型的节点结构

```go
type Node struct {
    left     *Node;
    right    *Node;
    data     interface{}
}
```



接口与接口

一个接口的值可以赋值给另一个接口变量，只要底层实现了必要的方法。这个转换是在运行时检查的，转换失败会导致一个运行时错误。





### 反射

反射能够在运行时检查类型和变量，reflect包提供了一系列元编程的方法。reflect包中最重要的是Value对象和Type对象，Value对象提供数据信息，Type对象提供类型信息。

```go
func TypeOf(i interface{}) Type
```

reflect包定义的方法，用于获取参数的动态类型信息，该方法返回Type对象。

```go
func ValueOf(i interface{}) Value
```

用于返回参数的运行时数据，该方法返回Value对象。



通过反射获取运行时数据信息，比如以下代码：

```go

func main() {
    var x float64 = 3.4
    
    value := reflect.ValueOf(x)
    fmt.Println(value.Float())
    fmt.Println(value.Type())
    fmt.Println(value.Kind())
}
```



反射可以修改运行时变量的值。如果要通过反射修改参数的值，那么需要反射参数的指针，才能修改到原参数的值。

```go
var x float64 = 3.1415926
value := reflect.ValueOf(&x)

value = value.Elem()	//返回value对象包含的值或指向的指针
value.SetFloat(5.5555)	//修改原参数的值
fmt.Println(x)			//输出5.55555
```



反射也可以在运行时解析结构的字段信息、方法信息。具体的使用可以查看Value对象提供的方法。

