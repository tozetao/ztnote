基础命令：

https://learn.microsoft.com/zh-cn/windows/wsl/basic-commands#run-a-specific-linux-distribution-from-powershell-or-cmd



```
安装
----------------
安装默认的linux ubuntu发行版本，仅当没有安装过才会生效。
wsl --install

安装指定的发行版
wsl --install -d <distroName>


查看
----------------
查看可用的发行版本
wsl --list --online

查看已经安装的linux发行版
wsl --list --verboase

检查wsl状态
wsl --status

检查wsl版本
wsl --version

设置
------------------
改变wsl版本。注：非常耗时
wsl --set-version <distribution name> <version>

设置默认的wsl版本
wsl --set-default-version <version>


设置默认Linux发行版
wsl --set-default <Distribution Name>


运行
------------------
运行特定的linux发行版
wsl --distribution <Distribution Name> --user <User Name>

以特定的用户运行
wsl --user <username>

关闭
-------------------
终止所有在运行的发行版。
wsl --shutdown

终止指定的发行版
wsl --terminate <Distribution Name>



导入、导出、挂在磁盘
```











改变docker的存储目录，也适用于其他系统

```
wsl -l -v

# 关闭所有wsl
wsl --shutdown

# 导出
wsl --export docker-desktop "d:\docker-desktop.tar"
wsl --export docker-desktop-data "d:\docker-desktop-data.tar"


# 删除原有数据
wsl --unregister docker-desktop
wsl --unregister docker-desktop-data

# 导入到新的磁盘
wsl --import docker-desktop "d:\wsl\docker-desktop" "D:\docker-desktop.tar" --version 2
wsl --import docker-desktop-data "d:\wsl\docker-desktop-data" "d:\docker-desktop-data.tar" --version 2

# 将原有docker目录剪切到你要放置的未知，创建软连接，改变docker数据目录
mklink /J "C:\Users\Administrator\AppData\Local\Docker" "D:\soft\Docker"

wsl --unregister Ubuntu-20.04
wsl --import Ubuntu-20.04 "d:\wsl\Ubuntu-20.04" "D:\ubuntu20.tar" --version 2
```



