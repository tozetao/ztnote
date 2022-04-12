### Unicode

Unicode是一个符号系统，它收集了世界上所有的符号。

每个符号分配一个唯一的Unicode码点，其实就是一个int32数字，这种编码方式也被称为UTF-32或UCS-4，每个Unicode码点都使用同样大小的int（32位）来表示。

Unicode的确实是浪费存储空间，在计算中大部分可读的文本是ASCII字符，并且常用字符远远小于65535，也就是说16个bit就能表示常用字符了，所以才会设计出节省空间的UTF8编码。



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



### String

在Go语言中，字符串字面量使用UTF8编码。也就是说如果是ASCII字符会使用1个byte存储，如果是中文字符会使用2-3个byte存储。

example：

```go
s1 := "go你好"
s2 := "hello world"
fmt.Println(reflect.TypeOf(s1[2]).Kind())
fmt.Println(reflect.TypeOf(s2[2]).Kind())

//可以看到第3个字节转换成字符类型并不是中文字符'你'
fmt.Printf("%d %c\n", s1[2], s1[2])
fmt.Printf("%d %c\n", s2[2], s2[2])
```



因为字符串字面量使用UTF8编码，所以不要简单的认为字符数就是字节数。

```go
s := "Hello, 世界"
fmt.Println(len(s))						//13
fmt.Println(utf8.RuneCountInString(s))	//9
```



### 转移字符

我们也在字符串面值中可以使用反斜杠 \\ 的转义序列插入任意的数据。常见的ASCII转移字符有：

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

rune类型是int32的别名，它可以用于表示一个unicode编码后的字符。

```go
s := "Hello, 世界"
r := []rune(s)			

//可以看到每个元素对应一个unicode码点
fmt.Println(r)				
```



对于字符串，range会通过解析UTF-8，分离成每个独立的unicode码点。错误的编码将会占据一个字节，并用U+FFFD来表示。

```go
for _, char := range "日本 \x80 寓" {// \x80 is an illegal UTF-8 encoding
    fmt.Printf("%U %c\n", char, char)
}
```