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

cmd参数表示操作，第三个参数以省略号表示，该参数可以设置为不同的类型。









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



