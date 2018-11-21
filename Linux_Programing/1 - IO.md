### 文件描述符

所有I/O操作的系统调用都是以文件描述符来表示，它是一个非负小整数。
文件描述符可以表示所有类型的已打开文件，包括普通文件、socket、管道（pipe）、FIFO、终端和设备等。



在系统中已经预定义了标准文件描述符，标准文件描述符的POSIX名称定义在<unistd.h>文件中，分别是：

- 标准输入

  文件描述符是0，stdio流为stdin，POSIX名称是STDIO_FILENO

- 标准输出

  文件描述符是1，stdio流为stdout，POSIX名称是STDOUT_FILENO

- 标准错误

  文件描述符是2，stdio流为stderr，POSIX名称是STDERR_FILENO





### 文件系统概述

文件描述符与文件并不是对应的关系，系统内核是通过3个数据结构维护着文件描述符的：

- 进程级的文件描述符表
- 系统级的打开文件表
- 文件系统的i-node表



针对每个进程，内核为其维护打开文件的描述符表（open file description），该表的每个条目都记录了单个文件描述符的相关信息，包括：

- 对打开文件句柄的引用
- 控制文件描述符操作的一组标志，目前只实现了close-on-exec标志。



针对所有打开的文件，内核会维护一个系统级打开文件表（open file table），表中每个条目是打开文件句柄（open file handle），该文件句柄存储了打开文件的所有信息，包括有：

- 文件的访问模式（即读写模式）
- 打开文件时所使用的状态标志，即open的flags参数
- 当前文件偏移量
- 与信号驱动I/O相关的设置
- 对该文件i-node对象的引用



而每个文件系统会对所有文件建立一个i-node表，表中的每一项具体信息包括：

- 文件类型和访问权限，例如常规文件或套接字类型
- 一个指向所持有锁的列表的指针
- 文件的各种属性，包括文件大小以及不同类型操作相关的时间戳



多个文件描述符可以指向同一个文件句柄（文件指针），而多个文件句柄可以指向同一个i-node项。

例如使用dup()复制文件句柄，或通过fork()子进程可以使不同文件描述符指向同一个文件句柄，它们会共享该文件句柄的偏移量、访问模式和状态标志。

通过多次调用open()操作同一个文件则会发生什么影响？这时是不同的文件句柄指向同一个i-node项，会发生竞争情况，无法保证数据的正确性，因此要注意这种情况的处理。

注：文件描述符标志，即close-on-exec标志为进程和文件描述符私有，对该标志的修改是不会影响到其他文件描述符的。



### 原子操作

- 原子操作

  原子操作指的是多个操作步骤会被当做一个不可分割的单元来进行执行。

  在linux中，某些完成多个操作的系统调用是支持原子性的，这是由内核实现的。

- 竞争状态

  竞争状态是指操作同个共享资源的多个进程或线程，其结果取决于一个无法预期的顺序。

  处于竞争状态下操作资源产生的结果也是不可预期的，因为这些进程获得CPU使用权的先后顺序是不可预料的。



关于I/O原子性常见操作：

- 以独占式的方式创建一个文件。

  在创建一个文件时，如果只指定了O_CREAT标志是无法保证独占式的，需要配合O_EXCL标志。同时指定O_CREAT和O_EXCL标识下，如果要打开的文件存在将会发生错误。

- 向文件尾部追加数据

  O_APPEND标志可以使文件偏移量的移动和数据写入纳入一个原子性操作，执行该标志可以保证数据写入的一致性。





### I/O Functions

```c
#include <sys/stat.h>
#include <fcntl.h>

int open(const char *pathname, int flags, [mode_t mode]);
```

打开一个文件描述符，如果调用失败会返回-1，并将errno设为对应的错误，具体的错误信息可以看手册说明，open()调用成功返回值是其进程未用文件描述符中的最小数值。

flags参数是位掩码，指定文件的访问模式、文件创建标志和文件状态标志，访问模式的常量有：

- O_RDWR

  以读写方式打开

- O_WRONLY

  以只写方式打开

- O_RDONLY

  以只读方式打开

文件状态标志是指已经打开文件的状态，可以通过fcntl()的F_GETFL和F_SETFL操作分别检索和修改这些标志，这些标志有：

- O_SYNC

  已同步方式写入文件

- O_NONBLOCK

  以非阻塞方式打开

- O_ASYNC

  当I/O操作可行时，产生信号signal通知进程。

- O_DSYNC

  提供同步的I/O数据完整性。

文件创建标志无法查询和修改，在创建文件之处就设定好的，例如：

- O_CREAT

  若文件不存在则创建。

- O_EXCL

  结合O_CREAT参数使用，如果文件已存在函数将调用失败。

- O_APPEND

  向文件尾部追加数据。

这些常量均可位或运算来进行组合，设置文件访问模式和状态标志。mode参数也是位掩码，它指定的文件的访问权限。



```c
#include <fcntl.h>

int creat(const char *pathname, mode_t mode);
```

create()系统调根据pathname参数内容创建并打开一个文件，若文件将会打开文件，清空文件内容并将长度置为0，成功调用会返回文件描述符。



```c
#include <unistd.h>

ssize_t read(int fd, void *buffer, size_t count);
```

从文件描述符fd指向的文件引用读取数据，count参数指定最多读取的字节数，buffer参数是用来存储读取数据的缓冲地址，缓冲区至少应该有count个字节。

调用成功返回实际读取到的字节数，碰到文件结尾返回0（EOF），出现错误返回-1。

注：read()能够从文件读取任何数据，因为读取的数据没有边界之分，如果需要在输入缓冲区结尾限制追加终止的空字符。



```c
#include <unistd.h>

ssize_t write(int fd, void *buffer, size_t count);
```

将数据写入一个打开的文件中。buffer参数是写入数据的缓冲地址，count是将写入的字节数。

如果调用成功会返回实际写入的字节数，该返回值可能会小于count参数值。这种情况一般是磁盘满了或者进程资源对文件大小的小指。

注：对磁盘文件进行I/O操作时，write()调用成功并不能保证数据已经写入磁盘，因为为了减少磁盘活动量和加快wirte()调用，内核会缓存磁盘的I/O操作，参见13章。



```c
#include <unistd.h>

int close(int fd);
```

关闭一个打开的文件描述符，并将其释放供进程使用。



```c
#include <unistd.h>

off_t lseek(int fd, off_t offset, int where);
```

每一个打开的文件，内核都会记录其文件偏移量，也叫做指针。文件偏移量是指write()或read()操作文件的位置，是以相对于文件起始点来表示，文件第一个字节的偏移量是0.

每次read()或write()都会调整文件偏移量，lseek()可以设置文件偏移量，offset参数设置文件偏移量，whence参数则表明应该以什么模式来解释offset参数：

- SEEK_SET

  将文件偏移量设置为从文件头部起始点开始的offset个字节

- SEEK_CUR

  相对于当前文件偏移量，将文件偏移量调整offset个字节

- SEEK_END

  将文件偏移量设置为起始于文件尾部的offset个字节，offset参数应该是从文件结尾的最后一个字符的下一个字符算起。

成功将返回文件偏移量，失败返回-1.







### fcntl()

```c
#include <fcntl.h>

int fcntl(int fd, int cmd, ...);
```

fcntl()系统调用可以对一个打开的文件描述符执行一系列控制操作，成功时返回的参数依赖cmd参数，失败时返回-1。

cmd参数表示支持的操作，常见的有：

- F_GETFL

  该命令表示用于获取文件的访问模式、打开标志和状态标志。

  由于访问模式和标志是以位掩码表示的，将一个状态标志和fcntl()的返回结果进行按位逻辑与运算，就可以判断出该标志是否有使用；

  而访问模式的3个常量不与状态标志中的单个比特位对应，因此需要将fcntl的结果与O_ACCMODE位掩码相与，然后再进行比较。

- F_SETFL

  该命令可以修改打开文件描述符的某些状态，允许更改的标志有O_APPEND、O_ASYNC、O_NONBLOCK、O_NOATIME和O_DIRECT，系统会忽略对其他标志的修改。

第三个参数以省略号表示，该参数会随着cmd参数而进行变化。



fcntl使用场景：

- 文件不是由程序打开的，例如标志输入和输出。
- 文件描述符是通过open()之后的系统调用，例如pipe()调用和socket()调用

对于这些场景，可以通过fcntl来操作文件描述符。



### dup()

文件描述符是可以进行复制的，系统提供了一些调用。

```c
#include <unistd.h>

int dup(int oldfd);
```

dup调用会复制一个文件描述符并返回一个编号值最低的未用文件描述符，俩个文件描述符都执行同一个打开的文件句柄，该函数成功返回新的文件描述符，失败返回-1。



```c
#include <unistd.h>

int dup2(int oldfd, int newfd);
```

dup2调用会复制oldfd文件描述符，该副本文件描述符的编号由newfd参数指定。dup2调用成功时返回指定编号的文件描述符，失败时返回-1。

如果newfd参数指定编号的文件描述符已存在，dup2函数会关闭该文件描述符，执行过程中会忽略关闭的错误，因此更安全的编码是如果newfd参数指定编号的文件描述符存在，要显示的关闭它。



```c
newfd = fcntl(oldfd, F_DUPFD, startfd);
```

fcntl()调用的F_DUPFD命令也可以用于赋值文件描述符，它会返回oldfd的一个副本，且会使用大于等于startfd的未用的最小文件描述符。







### pread/pwrite

pread()与pwrite()会在指定偏移量处进行文件I/O操作，而非文件的当前偏移量，且该系统调用不会改变文件的偏移量。

```c
#include <unistd.h>

ssize_t pread(int fd, void *buffer, size_t count, off_t offset);
ssize_t pwrite(int fd, const void *buf, size_t count, off_t offset);
```



pread()调用相当于将如下操作纳入原子操作：

```c
off_t orig;

orig = lseek(fd, 0, SEEK_CUR);	//存储当前文件的偏移量
lseek(fd, offset, SEEK_SET);	//指定要操作的偏移量
s = read(fd, buffer, len);		//读取数据
lseek(fd, orig, SEEK_SET);		//将偏移量至为操作之前的状态
```

pread()和pwrite()可以用于多线程应用中，在对同一个文件描述符执行I/O操作时，不会因为其他线程修改文件偏移量而受到影响，如果使用write()和read()来替代，将会引发竞争状态。



### readv()

readv()实现了分散输入，writev()实现了集中输出。这些系统调用可对多个缓冲区的数据进行I/O操作。

```c
#include <sys/uio.h>

ssize_t readv(int fd, const struct iovec *iov, int iovcnt);
```

readv()实现了分散输入的功能，调用成功将返回读取的字节数，碰到文件结尾返回0，失败返回-1。iov参数是一个iovec结构类型的数组，定义了一组用来传输数据的缓冲区，iovec结构体如下：

```c
struct iovec {
    void *iov_base;
    size_t iov_len;
}
```

其中iov_base是缓冲区的起始地址，iov_len参数指从缓冲区读取的字节数大小或写入缓冲区的字节数大小。

iovcnt参数指定iov数组的成员个数。



- 什么是分散输入?

  指从文件描述符fd所指向的文件读取一片连续的字节，然后将其分散放置在iov指定的缓冲区中，输入的过程会从第一个元素的缓冲区开始，依次填满每个缓冲区。

- readv()的原子性说明

  readv()是原子性的，从调用进程的角度来看，当调用readv()时，内核在fd所指向的文件与用户内存之间一次性的完成了数据转移，这意味着即使其他进程（线程）与其共享同一文件偏移量，且在调用readv()的同时企图修改文件偏移量，readv()所读取的数据仍然是连续性的。



example:

```c
struct iovec iovecs[2];

char buf1[10];
char buf2[5];

iovecs[0].iov_base = buf1;
iovecs[0].iov_len = sizeof(buf1);

iovecs[1].iov_base = buf2;
iovecs[1].iov_len = sizeof(buf2);

readv(fd, iovecs, 2);
```



### writev()

```c
#include <sys/uio.h>

ssize_t writev(int fd, const struct iovec *iov, int iovcnt);
```

writev()参数与readv()相同，它实现了集中输出的功能。

- 什么是集中输出

  集中输出指的是将多个缓冲区的数据拼接起来，然后以连续的字节序列写入到指定的文件中。

- 原子性操作

  writev()调用是原子操作，即所有数据是一次性地从用户内存传输到fd指向的文件中，因此在写入文件时，writev()会把所有请求数据连续写入到文件中，而不会再其他进程（线程）的影响下分散地写入文件。



### 截断文件

```c
#include <unistd.h>

int truncate(const char *pathname, off_t length);
int ftruncate(int fd, off_t length);
```

truncate()和ftruncate()系统调用将文件大小设置为length参数指定的值。

若文件长度大于参数length，调用将丢弃超出部分；若小于参数length，调用将在文件尾部添加一系列空字节或一个文件空洞。



俩个系统调用的差别在于如何指定操作文件，truncate()以路径名字字符串来指定文件，并要求可访问该文件，且对文件拥有写权限。若文件名为符号链接，那么调用将对其解引用。

ftruncate()调用操作的是以写方式打开的文件描述符，该系统调用不会修改文件偏移量。
