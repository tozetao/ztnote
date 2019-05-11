### 数组

数组是一个由固定长度的特定类型元素组成的序列，数组与slice最大的区别是，数组长度是固定的，slice长度是运行时可改变的。

数组在内存中是一段连续的存储空间，并且Go语言的数组是值类型，数组赋值时是对整个数组值的拷贝。

声明格式：

```go
var array [length]type
```

length是数组长度，俩个类型相同但是长度不同的数组是俩种类型，比如：

```go
var arr1 [5]int;	//长度为5的int型数组
var arr2 [3]int;	//长度为3的int型数组
```





### 切片

切片是引用类型，它是对数组某个连续片段的引用，切片的长度是运行时可改变的。

声明：

```go
var slice []type;
```

初始化：

```go
var slice []type = array[start:end]
```

切片是可索引访问的，对数组进行切片后，切片索引将从0开始访问。

- 切片的长度

  长度 = end - start，也就是引用数组[start, end)左闭右开这段范围的元素。

- 切片的容量

  cap()用于计算切片的最大容量。

  它等于切片的长度 + 数组切片之外的长度，也就是slice[start]到数组末尾的数组长度。

- 数组共享

  由于是引用类型，对同一个数组的不同切片是共享同一个数组数据的。

example：

```go
//创建长度为4的数组并创建了一个相关切片
var slice1 = []int{1,2,3,4}[:]
var slice2 = []int{1,2,3,4}
```

注：字符串是纯粹不可变的数组，因此可以切片。



### make()

当一个数组还没定义时，可以使用make创建一个切片，同时创建好相关数组。



make函数的原型：

```c
func make([]T, len, cap)
```

[]T是要创建切片的类型，len是切片的长度，cap是可选参数，表示切片的容量，也可以认为是数组的长度；如果len、cap参数同时使用，那么会创建一个只占用数组len个项的切片。



初始化：

```go
var slice []type = make([]type, len)	//slice := make([]type, len)
```



### new()

new(T)为每个新的类型T分配一块内存，它会按照数据类型对开辟的内存空间做默认初始化，并返回类型为T的内存地址。new(T)适用于值类型的数组和结构体。

```go
var a *int = new(int);					// *a == 0
var slice *[]string = new([]string)		// *slice == nil

//简短赋值
a := new(int)
slice := new([]string)
```



make(T)是返回类型为T的初始值，它只适用于slice、map和channel。与new的区别在于：new()函数是返回指向内存的指针，而make是返回值类型。

new(T)、mak(T)都是在堆上分配内存空间。
