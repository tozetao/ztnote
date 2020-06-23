### strings

strings包实现了处理字符串的简单函数。



Reader

strings包中的Reader是一个通过读取一个字符串，并实现了io.Reader等接口的对象。



### bufio

bufio包实现了带缓存的I/O操作，它封装了一个io.Reader或io.Writer对象，另外创建了一个新的对象（Reader或Writer），这个对象实现了提供缓冲和文档读写的接口。

注：返回的新对象虽然与io包的Reader、Writer对象同名，但是提供的接口是不一样的。



Reader

bufio包中的Reader是实现了一个对io.Reader对象的缓冲读对象（结构）



