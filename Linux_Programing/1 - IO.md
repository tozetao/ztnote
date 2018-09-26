### I/O

I/O表示输入与输出，一个通用I/O模型的系统调用包括：

- 打开文件
- 关闭文件
- 读取文件
- 写入文件



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







