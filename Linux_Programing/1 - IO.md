### I/O

I/O��ʾ�����������һ��ͨ��I/Oģ�͵�ϵͳ���ð�����

- ���ļ�
- �ر��ļ�
- ��ȡ�ļ�
- д���ļ�

```c
int open(const char *pathname, int flags, [mode_t mode]);
```

��һ���ļ���������flags������λ���룬ָ���ļ��ķ���ģʽ������ģʽ�У�

- O_CREAT�����ļ��������򴴽���
- O_EXCL�����O_CREAT����ʹ�ã�����ļ��Ѵ��ں���������ʧ�ܡ�

- O_APPEND�����ļ�β��׷�����ݡ�

mode�����ڴ����ļ�ʱʱ�򣬱�ʾ���ļ���״̬��

### �ļ�������

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





### �ļ��ն�

һ���ļ���ƫ�����������ļ���β������Ȼ�ǿ��Խ���I/O�����ģ�read()�����᷵��0����ʾ�ļ���β��write()�����������ļ���β������λ��д�����ݡ�

���ļ���β������д�������֮�����οնα���Ϊ�ļ��ն�������֧���ļ��ն���ϵͳ���ļ��ն��ǲ�ռ�ݴ��̿ռ�ģ������ڲ�֧���ļ��ն���ϵͳ���Կ��ֽ�д���ļ���

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

�ŵ㣺

- �������ļ������ܵ�ռ�������Ĵ������������ٺ���д��Ͷ�ȡ�ļ�ʱ�Ĵ���Ѱ��������
- Ѹ��ռ�ô��̿ռ䣬��ֹʹ�ù���������ռ䲻�㡣
- ������׷�����ݵĻ���������Ҫ�ı��ļ���С�����Ժ��潫���漰metadata���޸ġ�



### ԭ�Ӳ���

- ԭ�Ӳ���

  ԭ�Ӳ���ָ����ϵͳ������Ҫ��ɵ�һϵ�ж�������Ϊ�����жϵĲ�����һ���Լ���ִ�С�

- ����״̬

  ��������Ľ��̻��̣߳�����ȡ����һ���޷�Ԥ�ڵ�˳�򣬼���Щ���̻��CPUʹ��Ȩ���Ⱥ�˳��ԭ�Ӳ������Թ�ܾ���״̬��

һ���ڶ��̣߳�����̣��ĳ����л���־���״̬��������������ͬ���ļ���β��д�����ݻ��߶�������ж��ļ��Ƿ񴴽���

```c
if (lseek(fd, 0, SEEK_END) == -1) 
    exit(1);
if (write(fd, buffer, len) != len)
    exit(1);
```

����δ����У������һ������ִ�е�lseek()��write()֮�䣬��ִ����ͬ����ĵڶ������̴�ϣ��������̻���д������֮ǰ��ƫ����ָ����ͬ��λ�ã���ʱ��ͻ���־���״̬��

�����һ������Խ��ļ�ƫ�������ƶ���д���������ԭ�Ӳ������ڴ��ļ�ʱ����O_APPEND���Ա�֤��һ�㡣







### fcntl

```c
#include <fcntl.h>

int fcntl(int fd, int cmd, ...);
```

fcntl()ϵͳ���ÿ��Զ�һ���򿪵��ļ�������ִ��һϵ�п��Ʋ������ɹ�ʱ���صĲ�������cmd������ʧ��ʱ����-1��

cmd������ʾ������������������ʡ�Ժű�ʾ���ò�����������Ϊ��ͬ�����͡�









example����һ���ļ�д������

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



