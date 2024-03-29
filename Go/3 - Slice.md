### 数组

数组是一个由特定类型元素组成的固定长度的序列，体现在内存中就是连续的存储空间。数组与slice最大的区别是，数组长度是固定的，slice长度是运行时可改变的。



#### 声明方式

```go
var array [length]type
```

length是数组的长度。数组的长度必须是常量表达式，因为数组的长度必须在编译期间确定。



#### 初始化

```go
var a[3]int = [3]int{1, 2, 3}
q := [...]int{4, 5, 6}		//使用...省略号由编译器来确定数组长度。
```



#### 数组类型

数组的长度是构成数组类型的一部分，因此不同长度的数组是俩种不同的类型。

```go
a := [2]int{1, 2}
b := [3]int{1, 2}
c := [..]int{1, 2}
fmt.Println(a == b, a == c)	//false, true
```

并且俩个数组只有在类型相同且元素都相同的情况下才相等，否则就认为是不同的。



数组也是值类型，因此数组赋值时是对整个数组值的拷贝。

```go
a := [...]int{1, 2, 3, 4}
b := a

fmt.Println(a)
fmt.Println(b)

b[0] = 10000
fmt.Println(a)
fmt.Println(b)	//1000, 2, 3, 4，可以看到俩个数组不是引用同一块内存的。
```















### Slice

切片是引用类型，它代表变长的序列。序列中的每个元素都有相同的类型。



#### 声明

```go
var slice []type;
```

slice和数组的字面值语法很类似，它们都是用花括弧包含一系列的初始化元素，但是对于slice并没有指明序列的长度。这会隐式地创建一个合适大小的数组，然后slice的指针指向底层的数组。







#### Slice的组成

Slice是数组的抽象，它由三个部分组成：长度、容量和指向底层数组的指针。

- 指针

  指针指向第一个Slice元素所对应的底层数组元素的地址，要注意的是Slice的第一个元素并不一定就是数组的第一个元素。

- 长度

  切片中所包含的元素数目

- 容量

  容量一般是从切片的第一个元素开始数起，直到底层数组元素末尾的个书，长度是不能超过容量。





#### 操作

```go
// 创建一个长度为5，容量为5的slice
slice1 := make([]int, 3)

// 附加元素
slice1 = append(slice1, 1,2,3,4)

// 子切片
sub1 := slice1[3:]		//1,2,3,4
sub2 := slice1[:3]		//0,0,0
sub3 := slice1[2:4]		//0,1
```

slice[start : end]表示创建一个新的切片，其中0 <= start <= end <= cap(s)，新的切片是引用切片slice第 start 个元素到第 end - 1 个元素的子序列。

子切片只有 end - start 个元素，如果start位置的索引被忽略将使用0代替，而end位置的索引被忽略将使用len(s)代替。



####  子切片的共享数组

通过上面这种方式切割出来的子切片，它们共享底层数据，引用的都是底层数组，只不过所引用的区间不同。如果多个切片引用的区间有重叠的元素，那么对重叠元素的修改将会影响到这些切片。

```go
nums := [...]int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

slice1 := nums[2:7]
slice2 := slice1[1:3]

fmt.Println(slice1)		//3, 4, 5, 6, 7
fmt.Println(slice2)		//4, 5

//slice1元素的复制会影响到slice2
slice1[1] = 100
fmt.Println(slice1)		//3, 100, 5, 6, 7
fmt.Println(slice2)		//100, 5
```



#### 切片容量的计算

```go
package main

import "fmt"

func main() {
	s := []int{2,3,5,7,11,13}

	// 截取切片使长度为0
	s = s[:0]
	printSlice(s)

	// 扩展长度
	s = s[:4]
	printSlice(s)	// len=4 cap=6

	// 丢弃前俩个元素。注意：在这里由于切片的第一个元素发生了改变，因此切片的容量也发生改变，由6变为4
	s = s[2:]
	printSlice(s)	// len=2 cap4

	// 因为扩展的长度不能大于容量，因此报错。
	// s = s[:6]
}

func printSlice(s []int) {
	fmt.Printf("len=%d cap=%d %v\n", len(s), cap(s), s)
}
```







#### nil切片

切片的零值是nil，一个nil值的切片没有底层数组，并且长度和容量都是0。

```go
var s1 []int
fmt.Println(s1 == nil)		//true

s2 := make([]int, 0)
fmt.Println(s2 == nil)		//false
```

但是一个非nil的切片长度和容量也有可能是0，比如下面的例子。所以判断一个切片的元素是否为空，要使用 len (s) == 0来判断，而不应该判断切片是否为nil。

example：

```go
s1 := make([]int, 0)
fmt.Println(s1 != nil)		//true
fmt.Println(len(s1) == 0)	//true
```



#### 切片的比较

不允许使用 == 操作符来比较俩个切片是否含有全部相等的元素，这是因为切片的元素是间接引用的，它在不同时刻可能包含不同的元素。

slice唯一合法的比较是和nil比较，例如：

```go
if summer == nil {}
```



#### make

make()函数可以创建一个指定元素类型、长度和容量的切片。如果省略容量，则容量等同于长度。

```go
make([]T, len)
make([]T, len, cap)
```

在底层，make创建了一个匿名的数组，然后返回一个slice。前一条语句中，slice是整个数组。在第二个语句中，slice只引用了整个数组前len个元素，但是容量是包含整个数组，额外的元素是留给未来增长的。








