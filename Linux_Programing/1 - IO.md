### I/O

I/O��ʾ�����������һ��ͨ��I/Oģ�͵�ϵͳ���ð�����

- ���ļ�
- �ر��ļ�
- ��ȡ�ļ�
- д���ļ�



�ļ�������

����I/O������ϵͳ���ö������ļ�����������ʾ������һ���Ǹ�С������
�ļ����������Ա�ʾ�������͵��Ѵ��ļ���������ͨ�ļ���socket���ܵ���pipe����FIFO���ն˺��豸�ȡ�



example����дһ���ļ�

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





�ļ��ն�

