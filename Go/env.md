### GOROOT

GOROOT指定Go的安装目录。Go的安装包解压后会有以下文件夹：

- api

- bin

  存放标准命令文件（可执行文件），包含go、godoc、gofmt。

- pkg

  存放安装Go标准库后的所有归档文件

- src

  Go标准库的所有源码文件和Go标准工具。





### GOPATH

GOPATH指定用户工作区。用户存放我们编写的代码，它包含3个目录：

- src

  以代码包的形式组织并保存Go源码文件。

- pkg

  用于存放通过go install命令安装后的归档文件。

- bin

  用于存放通过go install命令安装后的可执行文件。



```ini
vi /etc/profile
export GOROOT=/usr/local/go
export PATH=$PATH:$GOROOT/bin
```

