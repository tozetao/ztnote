map是一个哈希表。



声明

```go
var m map[K]V
```

map的声明方式，K是键，必须是可以使用 == 操作符比较的类型，而V是键对应的值，它可以是任意类型。

map的零值是nil，对一个nil的map插入一个key是会发生panic异常的。



初始化

```go
m := map[k]v{}
m := make(map[k]v)
```

map可以通过字面值初始化，也可以通过make()函数来初始化。默认初始化的map的元素是空的，这时候的map是可以插入元素的。



访问元素

如果一个key是存在的，将会返回key所对应的值；如果一个key是不存在的，将会返回对应值类型的零值。

```go
ages := make(map[string]int)
fmt.Println(ages["a"])		// 0
```

如果要判断一个key是否存在map中，可以使用短变量赋值方式来访问key：

```go
age, ok := ages["abc"]
```

ok是一个布尔值，它表示key"abc"是否在map中。



遍历



内置函数





