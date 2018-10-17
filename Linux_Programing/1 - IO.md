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

cmd������ʾĳ�ֲ��������Ĳ��������У�

- F_GETFL

  �������ʾ���ڻ�ȡ�ļ��ķ���ģʽ��״̬��־�������״̬��־ָ����open()ϵͳ���õ�flags������

  ��ȡ�ļ���״̬��־����ʹ���߼�����������м��㣬��Ϊ�ļ���״̬��־��λ�����ʾ������ȡ�ļ��ķ���ģʽ����Ҫ��fcntl�Ľ����O_ACCMODE���������߼������㣬Ȼ���ٽ��бȽϣ�������Ϊ����ģʽ�����ļ���־��λ�����Ӧ��

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

  ����������޸Ĵ��ļ���������ĳЩ״̬��������ĵı�־��O_APPEND��O_ASYNC��O_NONBLOCK��O_NOATIME��O_DIRECT��

  ```c
  // ���O_APPEND�ļ���־
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


������������ʡ�Ժű�ʾ���ò�����������Ϊ��ͬ�����͡�



ʹ�ó�����

- �ļ������ɳ���򿪵ģ������־����������
- �ļ���������ͨ��open()֮���ϵͳ���ã�����pipe()���ú�socket()����



### �ļ����������ļ��Ĺ�ϵ

�ļ����������ļ�������һһ��Ӧ�Ĺ�ϵ��ϵͳ�ں���ͨ��3�����ݽṹά�����ļ��������ģ�

- ϵͳ�����ļ���������
- ���̼����ļ���������
- �ļ�ϵͳ��i-node��



���ÿ�����̣��ں�Ϊ��ά�����ļ�����������open file description�����ñ��ÿ����Ŀ����¼�˵����ļ��������������Ϣ��������

- �Դ��ļ����������
- �����ļ�������������һ���־��Ŀǰֻʵ����close-on-exec��־��



������д򿪵��ļ����ں˻�ά��һ��ϵͳ����������񣬳�Ϊ���ļ���open file table��������ÿ����Ŀ��Ϊ���ļ������open file handle�������ļ�����洢�˴��ļ���������Ϣ�������У�

- ��ǰ�ļ�ƫ����
- ���ļ�ʱ��ʹ�õ�״̬��־����open��flags����
- �ļ�����ģʽ
- ���ź�����I/O��ص�����
- �Ը��ļ�i-node���������



��ÿ���ļ�ϵͳ��������ļ�����һ��i-node��������Ϣ�У�

- �ļ����ͣ����糣���ļ����׽��ֵȣ��ͷ���Ȩ��
- һ��ָ�������������б��ָ��
- �ļ��ĸ������ԣ������ļ���С�Լ���ͬ���Ͳ�����ص�ʱ���



�ļ������������ļ����i-node���ߵĹ�ϵ�����У�

- ͬ�������ڲ�ͬ�ļ�������ָ��ͬһ���򿪵��ļ�������������������ͨ��dmp()��dmp2()��fcntl()�γɵġ�

- ��ͬ�����ڵ��ļ�������ָ��ͬһ���򿪵��ļ������������������ڵ���fork()����֣�����֮���Ǹ��ӹ�ϵ

- ��ͬ�����ڵ��ļ�������ָ��ͬ�Ĵ򿪵��ļ������������Щ�ļ������ָ��ͬһ��i-node��Ŀ����������Ǵ���ͬһ���ļ���



�ܽ᣺

1. ������ͬ���ļ�����������ָ��ͬһ���򿪵��ļ������������ͬһ���ļ�ƫ��������˶�����һ���ļ��������޸���ƫ����������һ���ļ�������Ҳ�ᷢ����㣬���������ļ��������Ƿ�����ͬһ�����̡�
2. �ļ���������־����close-on-exec��־��Ϊ���̺��ļ�������˽�У�����һ��־���޸Ĳ���Ӱ�쵽ͬһ���̻�ͬ�����е��ļ���������





### �����ļ�������

```c
#include <unistd.h>

int dup(int oldfd);
```

dup���ûḴ��һ���ļ�������������һ�����ֵ��͵�δ���ļ��������������ļ���������ִ��ͬһ���򿪵��ļ�������ú����ɹ������µ��ļ���������ʧ�ܷ���-1��



```c
#include <unistd.h>

int dup2(int oldfd, int newfd);
```

dup2���ûḴ��oldfd�ļ����������ø����ļ��������ı����newfd����ָ����

���newfd����ָ����ŵ��ļ��������Ѵ��ڣ�dup2������رո��ļ���������ִ�й����л���ԹرյĴ�����˸���ȫ�ı��������newfd����ָ����ŵ��ļ����������ڣ�Ҫ��ʾ�Ĺر�����

dup2���óɹ�ʱ����ָ����ŵ��ļ���������ʧ��ʱ����-1��



fcntl()��F_DUPFD�Ǹ����ļ�����������һ�ӿڣ����᷵��oldfd��һ���������һ�ʹ�ô��ڵ���startfd����Сδ��ֵ��Ϊ��������ţ��õ����ܱ�֤�µ���������������ض��ķ�Ϊ�ڡ�

```c
newfd = fcntl(oldfd, F_DUPFD, startfd);
```





### pread��pwrite

pread��pwrite����ָ����λ�ý����ļ�I/O����������ʼ���ļ��ĵ�ǰƫ�������Ҹ�ϵͳ���ò���ı��ļ���ƫ������

```c
#include <unistd.h>

ssize_t pread(int fd, void *buffer, size_t count, off_t offset);

ssize_t pwrite
```



pread()�����൱�ڽ����²�������ԭ�Ӳ�����

```c
// ��¼��ǰ�ļ�����ʼƫ����
off_t orig;

orig = lseek(fd, 0, SEEK_CUR);
lseek(fd, offset, SEEK_SET);
s = read(fd, buffer, len);
lseek(fd, orig, SEEK_SET);
```



### ��ɢ�����뼯�����

readv()��writev()�ֱ�ʵ���˷�ɢ�����뼯������Ĺ��ܣ���Щϵͳ���ÿɶԶ�������������ݽ���I/O������

```c
#include <sys/uio.h>

ssize_t readv(int fd, const struct iovec *iov, int iovcnt);
ssize_t writev(int fd, const struct iovec *iov, int iovcnt);
```

- iov

  �ò�����һ��iovec�ṹ���͵����飬������һ�������������ݵĻ�������iovec�ṹ���£�

  ```c
  struct iovec {
      void *iov_base;
      size_t iov_len;
  }
  ```

  iov_base�ǻ���������ʼ��ַ��iov_len����ָ�ӻ�������ȡ���ֽ�����С��д�뻺�������ֽ�����С��

- iovcnt

  �ò���ָ��iov����ĳ�Ա����



��ɢ����

��ɢ����ָ���Ǵ��ļ�������fd��ָ����ļ���ȡһƬ�������ֽڣ�Ȼ�����ɢ������iovָ���Ļ������У�����Ĺ��̻�ӵ�һ��Ԫ�صĻ�������ʼ����������ÿ����������

readv()��ԭ���Եģ��ӵ��ý��̵ĽǶ�������������readv()ʱ���ں���fd��ָ����ļ����û��ڴ�֮��һ���Ե����������ת�ƣ�����ζ�ż�ʹ�������̣��̣߳����乲��ͬһ�ļ�ƫ���������ڵ���readv()��ͬʱ��ͼ�޸��ļ�ƫ������readv()����ȡ��������Ȼ�������Եġ�

readv()���óɹ������ض�ȡ���ֽ��������ļ�����ʱ������0��

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



�������

�������ָ���ǽ����������������ƴ��������Ȼ�����������ֽ�����д�뵽ָ�����ļ��С�writev()ʵ���˼��������



writev()������ԭ�Ӳ�����������������һ���Եش��û��ڴ洫�䵽fdָ����ļ��У������д���ļ�ʱ��writev()�������������������д�뵽�ļ��У����������������̣��̣߳���Ӱ���·�ɢ��д���ļ���



readv()��writev()�ĵ������ڱ߽磬�����������ַ��������

- ����ʱ����һ���Ļ�������Ȼ���ڽ��̵�ַ�ռ������λ�ý����ݸ��ƹ�����������wirte��read�����������ݡ�

  ȱ�������û��ڴ�ռ���仺�����������ݸ��ƣ�Ч�ʵ�

- ͨ����ε���read()��write()���÷����޷���֤������ԭ���ԡ�





### �ض��ļ�

```c
#include <unistd.h>

int truncate(const char *pathname, off_t length);
int ftruncate(int fd, off_t length);
```

truncate()��ftruncate()ϵͳ���ý��ļ���С����Ϊlength����ָ����ֵ��

���ļ����ȴ��ڲ���length�����ý������������֣���С�ڲ���length�����ý����ļ�β�����һϵ�п��ֽڻ�һ���ļ��ն���



����ϵͳ���õĲ���������ָ�������ļ���truncate()��·�������ַ�����ָ���ļ�����Ҫ��ɷ��ʸ��ļ����Ҷ��ļ�ӵ��дȨ�ޡ����ļ���Ϊ�������ӣ���ô���ý���������á�

ftruncate()���ò���������д��ʽ�򿪵��ļ�����������ϵͳ���ò����޸��ļ�ƫ������



### ������I/O

�ڴ��ļ�ʱָ��O_NONBLOCK��־��Ŀ���ж���

- ��open()����δ���������ļ��򷵻ش��󣬶���������������һ���������⣬����open()����FIFO���ܻ�����������
- ����open()�ɹ��󣬺�����I/OҲ�Ƿ������ġ���I/Oϵͳ����δ��������ɣ�����ܻ�ֻ���䲿�����ݣ�����ϵͳ����ʧ�ܣ�������EAGAIN��EWOULDBLOCK���󡣾��巵�غ��ִ���������ϵͳ���á�

socket���ܵ���FIFO���豸��֧�ַ�����ģʽ��ֻ��������ͨ��open()��������ȡ�ܵ����׽��ֵ��ļ�������������Ҫ���÷�������־����Ҫʹ��fcntl()��F_SETFL��

�������ݲμ�44.9�ں�63��







### example

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



