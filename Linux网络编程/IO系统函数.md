### readv/writev

```c
ssize_t readv(int fd, const struct iovec *iov, int iovcnt);
```

把从fd读取到的数据按照顺序散布到iovec指定的多个缓冲区中。



```c
ssize_t writev(int fd, const struct iovec *iov, int iovcnt);
```

writev会把iovec数组的多个缓冲区数据按照顺序写入到fd中。



iov指针是一个iovec结构体数组，它的定义如下：

```c
struct iovec {
    void *iov_base;		// 起始地址
    size_t iov_len;		// Number of bytes to transfer，要传输的字节数
}
```

每个iovec描述了一块要传送的数据，它开始于iov_base，并且拥有iov_len个字节长度。iovcnt参数则表示有多少个iovec元素。



#### 返回值

成功后，readv()、preadv()和preadv2()返回读取的字节数；writev()、writev()和writev2()返回写入的字节数。出错时，返回-1，并适当地设置errno。

注意：如果调用成功，传输的字节数少于请求的字节数，这不是错误（详见read(2)和write(2)）。



#### 缓冲区的处理顺序

缓冲区是按数组顺序处理的。这意味着readv()在继续处理iov[1]之前完全填满iov[0]，依此类推。如果没有足够的数据，那么iov所指向的所有缓冲区都可能被填满；类似的writev在处理iov[1]之前必须写满ov[0]，依此类推。



#### 原子性

readv()和writev()执行的数据传输是原子的。writev()写入的数据是一个单独的块，不会与其他进程中的写入输出混在一起（pipe(7)例外）。

类似地，readv()保证从文件中读取一个连续的数据块，不管其他线程或进程中的读取操作是如何进行的，它们的文件描述符指向同一个打开的文件（详见open(2)）。





