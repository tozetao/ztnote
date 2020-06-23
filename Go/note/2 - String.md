### Unicode

Unicode是一个符号系统，它收集了世界上所有的符号。

每个符号分配一个唯一的Unicode码点，其实就是一个int32数字。将一个符号表示为一个int32序列，这种编码方式称为UTF-32或UCS-4，每个Unicode码点都使用同样大小的int32bit来表示。

- 缺点

  浪费存储空间，大部分计算机可读的文本是ASCII字符，并且常用字符远远小于65535，也就是说16bit就能表示常用字符了。



### UTF8

UTF8是一个将Unicode码点编码为可变长字节序列的编码。

它使用1到4个字节来表示每个Unicode码点，ASCII部分字符只使用1个字节，常用字符使用2到3个字节表示。每个符号编码后的高端bit位用于表示用多少个字节编码。

```ini
0xxxxxxx                             runes 0-127    (ASCII)
110xxxxx 10xxxxxx                    128-2047       (values <128 unused)
1110xxxx 10xxxxxx 10xxxxxx           2048-65535     (values <2048 unused)
11110xxx 10xxxxxx 10xxxxxx 10xxxxxx  65536-0x10ffff (other values unused)
```

如果高bit位是0时，表示ASCII的0到127对应的字符；如果是110则表示用2个字节来编码；如果是1110表示用3个字节编码；如果是11110则表示用4个字节编码。





### 字符串

字符串是不可改变的字节序列，文本字符串通常被解释为采用UTF8编码的Unicode码点（rune）序列。

- 字节序列

  由于字符串是一个字节序列，因此可以像访问数组一样去访问某个字节。

  ```go
  var s string = "hello world"
  fmt.Println(s[0])
  ```

- 不变性

  字符串的字节是可以访问的，但是不可修改。

  由于不变性，使得复制任意长度的字符串的代价都是低廉的。并且一个字符串和它的子切片的操作都可以安全的共享相同的内存。

  ```go
  var s string = "你好!"
  var t string = s
  ```

  字符串的不变性可以理解为，字符串变量存储的并不是具体的字节序列，而是指向字节序列的一个指针。



### 字符串面值

字符串值可以用字符串面值方式编写，只需要把字节序列写在双引号中即可。

```go
s := "hello，你好"
```

因为Go的源文件采用UTF8编码，所以在字符串面值中可以使用反斜杠 \\ 的转义序列插入任意的数据。

常见的ASCII转移字符有：

```ini
\t
\n
\r
\'	单引号，只允许在'\''形式的rune符号面值中
\"	双引号，只允许在"..."形式的字面值中
```

也可以使用unicode码点来表示字符，有俩种形式：

- \uxxxx表示16位的码点
- \Uxxxxxxxx表示32位的码点

其中x是一个16进制数字，每个码点都是对应字符的UTF-8编码值。



### Rune

rune类型是int32型，就是一个字符经过UTF8编码后的int32序列，也被称为Unicode码点。反过来的话，一个rune解码后对应一个字符。



在Go中一般采用rune序列来表示UTF8字符串，因为rune大小一致，支持数组索引方便切割。如果将字符串转换为[]rune类型，将返回字符串编码后的Unicode码点序列。

```go
s := "Hello, 世界"
r := []rune(s)			
fmt.Println(r)				//unicode码点序列，可以看到每个unicode码点对应一个字符。
```



因为使用UTF8编码，所以不要简单的认为字节数就是字符数。

```go
s := "Hello, 世界"
fmt.Println(len(s))						//13
fmt.Println(utf8.RuneCountInString(s))	//9
```



for range会隐式的对字符串解码

```go
for _, val := range "中国你好!" {
    fmt.Printf("%c", val)
}
```





### Byte

byte是字节类型，一个字符串其实就是一个byte数组或byte切片。



- 字符串和字节slice是可以相互转换的。

```go
s := "abc"
b := []byte(s)
s2 := string(b)
```

一个[]byte(s)的转换是分配了一个新的字节数组用于保存字符串数据的拷贝，然后引用这个字节数组。而把一个字节数组转换成字符串则是构建了一个新的字符串拷贝，并且确保是不可变的。





- 字符串的构建

  通过+运算符来构建字符串是很浪费内存的，因为字符串的不变性，俩个字符串相加时会再分配一块内存空间来存储型的字符串。

  解决的方式是使用bytes包提供了Buffer类型，它用于字节slice的缓存。

  一个Buffer是空的，但是随着string、byte或[]byte等类型数据的写入可以动态增长。一个bytes.Buffer类型是不需要初始化的，因为零值也是有效的。

```go
func intsToString(values []int) string {
    var buf bytes.Buffer
    buf.WriteByte('[')
    for i, v := range values {
        if i > 0 {
            buf.WriteString(", ")
        }
        fmt.Fprintf(&buf, "%d", v)
    }
    buf.WriteByte(']')
    return buf.String()
}
```

