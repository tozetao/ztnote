## UBUNTU系统 ##

### 工具 ###
wmware12、ubuntu16 32位系统

### vmware常用快捷键 ###
- Ctrl + Alt + Enter：全屏显示 
- Ctrl + Alt：释放vmware窗口 
- Ctrl + g：返回到vmware输入窗口

### wmware tools安装 ###
- 在客户端挂在CD驱动器
- 启动终端，使用tar命令解压安装程序
- vmware-install.pl安装

### vmware快照 ###
vmware快照系统，类似与备份功能，能够将某个时间节点的系统状态保存下来，方便你恢复，在菜单栏的虚拟机选项可以看到该功能。

### ubuntu快捷键 ###
Ctrl + Alt + t：呼出终端窗口
Ctrl + F1-F7：不同键位，呼出不同输出模式。

### ubuntu常用命令 ###
>apt命令

该命令是ubuntu系统的软件包管理命令，使用如下：
```
软件包安装
	apt-get install package
		安装软件包，package是软件包包名。
	apt-get install pageage --reinstall
		重新安装软件包
	
软件包搜索
	apt-cache search foo
		搜索和foo匹配的包
	apt-cache show foo
		显示和foo包相关的信息，例如描述、大小、依赖和冲突。
	
	apt-cache search pkgnames
		快速列出已安装的软件包。

	dpkg -l *foo*
		查找有foo字样的软件包，并且会列出是否安装信息。
	dpkg -L foo
		显示foo安装包的安装文件和路径。
	

软件包删除
	apt-get remove package
		删除已安装的软件包，会保留配置文件
	apt-get remove --purge package
		同上，但是不保留配置文件
	
软件包更新
	apt-get update 更新源
	apt-get upgrade package
		更新已安装的软件包
	apt-get check package
		检查软件包是否有损坏的依赖

apt-get source packagename
	下载该包的源代码
apt-cache depends packagename
	了解软件包使用依赖
apt-cache rdepends packagename
	查看该包被哪些包依赖
	
```

>挂载光盘

```
sudo mount /dev/sr0/ /mnt
```