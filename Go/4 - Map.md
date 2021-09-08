map是一个哈希表。



#### 声明

```go
var m map[K]V
```

map的声明方式，K是键，V是值。Key必须是可以使用 == 操作符比较的类型，而V可以是任意类型。

一个map在声明时未进行初始化，它的零值是nil，对一个nil的map插入一个key是会发生panic异常的。



#### 初始化

```go
m := map[k]v{}
m := make(map[k]v)
```

map可以通过字面值初始化，也可以通过make()函数来初始化。默认初始化的map的元素是空的，这时候的map是可以插入元素的。



#### 操作

```go
// 添加
scores["lisi"] = 100
scores["zhangsan"] = 50

// 访问
fmt.Println(scores["lisi"])
fmt.Println(scores["not_found"])    //如果一个key是不存在的，将会返回对应值类型的零值。

// 判断是否存在
_, ok := scores["not_found"]
fmt.Println(ok)
```





#### 遍历

```go
m := map[string]string{
    "Sam": "Male",
    "Alice": "Female",
}

for key, value := range m {
    fmt.Println(key, ":", value)
}
```





#### 内置函数





