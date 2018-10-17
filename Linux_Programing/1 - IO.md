### I/O

I/O表示输入与输出，一个通用I/O模型的系统调用包括：

- 打开文件
- 关闭文件
- 读取文件
- 写入文件

```c
int open(const char *pathname, int flags, [mode_t mode]);
```

打开一个文件描述符，flags参数是位掩码，指定文件的访问模式，具体模式有：

- O_CREAT：若文件不存在则创建。
- O_EXCL：结合O_CREAT参数使用，如果文件已存在函数将调用失败。

- O_APPEND：向文件尾部追加数据。

mode参数在创建文件时时候，表示该文件的状态。

### 文件描述符

所有I/O操作的系统调用都是以文件描述符来表示，它是一个非负小整数。
文件描述符可以表示所有类型的已打开文件，包括普通文件、socket、管道（pipe）、FIFO、终端和设备等。



example：读写一个文件

```c
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>

#define BUF_SIZE 1024

int main(int argc, char const *argv[])
{
    int fd, openFlags, filePerms;
    char buffer[BUF_SIZE];

    openFlags = O_CREAT | O_RDWR | O_TRUNC;
    filePerms = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;

    fd = open("test.log", openFlags, filePerms);

    if (fd == -1) {
        perror("open error.");
    }

    write(fd, "qwerqwer", 8);
    lseek(fd, SEEK_SET, 0);

    while (read(fd, buffer, BUF_SIZE) > 0) {
        printf("%s\n", buffer);
    }

    close(fd);
    return 0;
}
```





### 文件空洞

一个文件的偏移量跨域了文件结尾，它仍然是可以进行I/O操作的，read()操作会返回0，表示文件结尾；write()操作可以在文件结尾处任意位置写入数据。

从文件结尾处到新写入的数据之间的这段空段被称为文件空洞，对于支持文件空洞的系统，文件空洞是不占据磁盘空间的，而对于不支持文件空洞的系统会以空字节写入文件。

```c
#include<stdio.h>
#include<unistd.h>
#include<fcntl.h>
#include<unistd.h>

int main(void)
{
    int fd, BUF_SIZE = 1024;
    char buffer[BUF_SIZE];

    fd = open("test.log", O_RDWR);
    if (fd == -1) {
        perror("open error.");
    }

    // lseek(fd, -1, SEEK_END);
    // read(fd, buffer, BUF_SIZE);
    int r = lseek(fd, 10, SEEK_END);
    printf("%d\n", r);
    // write(fd, "hello", 5);

    // lseek(fd, 0, SEEK_SET);
    // read(fd, buffer, BUF_SIZE);
    // printf("%s\n", buffer);

    close(fd);

    return 0;
}
```

优点：

- 可以让文件尽可能的占用连续的磁盘扇区，减少后续写入和读取文件时的磁盘寻道开销；
- 迅速占用磁盘空间，防止使用过程中所需空间不足。
- 后面再追加数据的话，不会需要改变文件大小，所以后面将不涉及metadata的修改。



### 原子操作

- 原子操作

  原子操作指的是系统调用所要完成的一系列动作将作为不可中断的操作，一次性加以执行。

- 竞争状态

  操作共享的进程或线程，其结果取决于一个无法预期的顺序，即这些进程获得CPU使用权的先后顺序。原子操作可以规避竞争状态。

一般在多线程（多进程）的程序中会出现竞争状态，例如多个进程向同个文件的尾部写入数据或者多个进程判断文件是否创建。

```c
if (lseek(fd, 0, SEEK_END) == -1) 
    exit(1);
if (write(fd, buffer, len) != len)
    exit(1);
```

在这段代码中，如果第一个进程执行到lseek()与write()之间，被执行相同代码的第二个进程打断，俩个进程会在写入数据之前将偏移量指向相同的位置，这时候就会出现竞争状态；

解决这一问题可以将文件偏移量的移动与写入操作纳入原子操作，在打开文件时加入O_APPEND可以保证这一点。







### fcntl

```c
#include <fcntl.h>

int fcntl(int fd, int cmd, ...);
```

fcntl()系统调用可以对一个打开的文件描述符执行一系列控制操作，成功时返回的参数依赖cmd参数，失败时返回-1。

cmd参数表示某种操作，它的操作类型有：

- F_GETFL

  该命令表示用于获取文件的访问模式和状态标志，这里的状态标志指的是open()系统调用的flags参数。

  获取文件的状态标志可以使用逻辑与运算符进行计算，因为文件的状态标志是位掩码表示；而获取文件的访问模式，需要将fcntl的结果与O_ACCMODE常量进行逻辑与运算，然后再进行比较，这是因为访问模式不与文件标志的位掩码对应。

  ```c
  #include <stdio.h>
  #include <unistd.h>
  #include <sys/stat.h>
  #include <fcntl.h>
  
  int main(int argc, char const *argv[])
  {
      int fd, flags, accessMode, rs;
  
      // fd = open(argv[1], O_RDONLY | O_SYNC);
      fd = open(argv[1], O_RDWR);
      if (fd == -1) {
          printf("%s\n", "open error!");
          return 1;
      }
  
      flags = fcntl(fd, F_GETFL);
      if (flags == -1) {
          perror("fcntl error.");
          return 1;
      }
  
      rs = flags & O_SYNC;
      printf("%x\n", rs);
  
      accessMode = flags & O_ACCMODE;
      printf("%d\n", accessMode == O_RDONLY);
      printf("%d\n", accessMode == O_RDWR);
      printf("%d\n", accessMode == O_WRONLY);
  
      return 0;
  }
  ```

- F_SETFL

  该命令可以修改打开文件描述符的某些状态，允许更改的标志有O_APPEND、O_ASYNC、O_NONBLOCK、O_NOATIME和O_DIRECT。

  ```c
  // 添加O_APPEND文件标志
  int flags;
  
  flags = fcntl(fd, F_GETFL);
  if (flags == -1) {
      perror("fcntl: ");
      return 1;
  }
  
  flags |= O_APPEND;
  if (fcntl(fd, F_SETFL, flags) == -1) {
      perror("fcntl: ");
      return 1;
  }
  ```


第三个参数以省略号表示，该参数可以设置为不同的类型。



使用场景：

- 文件不是由程序打开的，例如标志输入和输出。
- 文件描述符是通过open()之后的系统调用，例如pipe()调用和socket()调用



### 文件描述符与文件的关系

文件描述符与文件并不是一一对应的关系，系统内核是通过3个数据结构维护着文件描述符的：

- 系统级的文件描述符表
- 进程级的文件描述符表
- 文件系统的i-node表



针对每个进程，内核为其维护打开文件的描述符表（open file description），该表的每个条目都记录了单个文件描述符的相关信息，包括：

- 对打开文件句柄的引用
- 控制文件描述符操作的一组标志，目前只实现了close-on-exec标志。



针对所有打开的文件，内核会维护一个系统级的描述表格，称为打开文件表（open file table），表中每个条目称为打开文件句柄（open file handle），该文件句柄存储了打开文件的所有信息，包括有：

- 当前文件偏移量
- 打开文件时所使用的状态标志，即open的flags参数
- 文件访问模式
- 与信号驱动I/O相关的设置
- 对该文件i-node对象的引用



而每个文件系统会对所有文件建立一个i-node表，它的信息有：

- 文件类型（例如常规文件或套接字等）和访问权限
- 一个指向所持有锁的列表的指针
- 文件的各种属性，包括文件大小以及不同类型操作相关的时间戳



文件描述符表、打开文件表和i-node三者的关系可能有：

- 同个进程内不同文件描述符指向同一个打开的文件句柄，这种情况可能是通过dmp()、dmp2()或fcntl()形成的。

- 不同进程内的文件描述符指向同一个打开的文件句柄，这种情况可能在调用fork()后出现，进程之间是父子关系

- 不同进程内的文件描述符指向不同的打开的文件句柄，但是这些文件句柄都指向同一个i-node条目，这种情况是打开了同一个文件。



总结：

1. 俩个不同的文件描述符，若指向同一个打开的文件句柄，将共享同一个文件偏移量，因此队其中一个文件描述符修改了偏移量，另外一个文件描述符也会发觉这点，无论俩个文件描述符是否属于同一个进程。
2. 文件描述符标志（即close-on-exec标志）为进程和文件描述符私有，对这一标志的修改不会影响到同一进程或不同进程中的文件描述符。





### 复制文件描述符

```c
#include <unistd.h>

int dup(int oldfd);
```

dup调用会复制一个文件描述符并返回一个编号值最低的未用文件描述符，俩个文件描述符都执行同一个打开的文件句柄，该函数成功返回新的文件描述符，失败返回-1。



```c
#include <unistd.h>

int dup2(int oldfd, int newfd);
```

dup2调用会复制oldfd文件描述符，该副本文件描述符的编号由newfd参数指定。

如果newfd参数指定编号的文件描述符已存在，dup2函数会关闭该文件描述符，执行过程中会忽略关闭的错误，因此更安全的编码是如果newfd参数指定编号的文件描述符存在，要显示的关闭它。

dup2调用成功时返回指定编号的文件描述符，失败时返回-1。



fcntl()的F_DUPFD是复制文件描述符的另一接口，它会返回oldfd的一个副本，且会使用大于等于startfd的最小未用值作为描述符编号，该调用能保证新的描述符编号落在特定的分为内。

```c
newfd = fcntl(oldfd, F_DUPFD, startfd);
```





### pread与pwrite

pread与pwrite会在指定的位置进行文件I/O操作，而非始于文件的当前偏移量，且该系统调用不会改变文件的偏移量。

```c
#include <unistd.h>

ssize_t pread(int fd, void *buffer, size_t count, off_t offset);

ssize_t pwrite
```



pread()调用相当于将如下操作纳入原子操作：

```c
// 记录当前文件的起始偏移量
off_t orig;

orig = lseek(fd, 0, SEEK_CUR);
lseek(fd, offset, SEEK_SET);
s = read(fd, buffer, len);
lseek(fd, orig, SEEK_SET);
```



### 分散输入与集中输出

readv()和writev()分别实现了分散输入与集中输出的功能，这些系统调用可对多个缓冲区的数据进行I/O操作。

```c
#include <sys/uio.h>

ssize_t readv(int fd, const struct iovec *iov, int iovcnt);
ssize_t writev(int fd, const struct iovec *iov, int iovcnt);
```

- iov

  该参数是一个iovec结构类型的数组，定义了一组用来传输数据的缓冲区，iovec结构如下：

  ```c
  struct iovec {
      void *iov_base;
      size_t iov_len;
  }
  ```

  iov_base是缓冲区的起始地址，iov_len参数指从缓冲区读取的字节数大小或写入缓冲区的字节数大小。

- iovcnt

  该参数指定iov数组的成员个数



分散输入

分散输入指的是从文件描述符fd所指向的文件读取一片连续的字节，然后将其分散放置于iov指定的缓冲区中，输入的过程会从第一个元素的缓冲区开始，依次填满每个缓冲区。

readv()是原子性的，从调用进程的角度来看，当调用readv()时，内核在fd所指向的文件与用户内存之间一次性的完成了数据转移，这意味着即使其他进程（线程）与其共享同一文件偏移量，且在调用readv()的同时企图修改文件偏移量，readv()所读取的数据仍然是连续性的。

readv()调用成功将返回读取的字节数，若文件结束时将返回0。

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



集中输出

集中输出指的是将多个缓冲区的数据拼接起来，然后以连续的字节序列写入到指定的文件中。writev()实现了集中输出。



writev()调用是原子操作，即所有数据是一次性地从用户内存传输到fd指向的文件中，因此在写入文件时，writev()会把所有请求数据连续写入到文件中，而不会再其他进程（线程）的影响下分散地写入文件。



readv()与writev()的调用在于边界，它可以用俩种方案替代：

- 编码时开辟一块大的缓冲区，然后在进程地址空间的其他位置将数据复制过来，最后调用wirte或read操作所有数据。

  缺点是在用户内存空间分配缓冲区进行数据复制，效率低

- 通过多次调用read()或write()，该方案无法保证操作的原子性。





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



### 非阻塞I/O

在打开文件时指定O_NONBLOCK标志，目的有二：

- 若open()调用未能立即打开文件则返回错误，而非陷入阻塞。有一种属于另外，调用open()操作FIFO可能会陷入阻塞。
- 调用open()成功后，后续的I/O也是非阻塞的。若I/O系统调用未能立即完成，则可能会只传输部分数据，或者系统调用失败，并返回EAGAIN或EWOULDBLOCK错误。具体返回何种错误将依赖于系统调用。

socket、管道、FIFO、设备都支持非阻塞模式，只不过不能通过open()调用来获取管道和套接字的文件描述符，所以要启用非阻塞标志，需要使用fcntl()的F_SETFL。

更多内容参见44.9节和63章







### example

example：向一个文件写入数据

```c
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

int main(int argc, char const *argv[])
{
    int fd;
    int BUF_SIZE = 1024;
    char buffer[BUF_SIZE];

    fd = open(argv[1], O_WRONLY | O_CREAT | O_APPEND);

    if(fd == -1){
        perror("open error: ");
        return 1;
    }

    lseek(fd, 0, SEEK_END);
    write(fd, argv[2], strlen(argv[2]));
    close(fd);

    return 0;
}
```



