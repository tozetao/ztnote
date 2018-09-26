### I/O

I/O表示输入与输出，一个通用I/O模型的系统调用包括：

- 打开文件
- 关闭文件
- 读取文件
- 写入文件



文件描述符

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





文件空洞

