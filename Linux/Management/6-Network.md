## Linux网络配置

### 1. 配置ip地址
### ifconfig
ifconfig命令用于查看当前系统的网卡信息，也可用于临时配置ip地址，类似于windows的ipconfig命令。

ifconfig输出说明
```
lo
	这是本地回环网卡的英文缩写，代表着当前计算机。
	ip地址是127.0.0.1，只是用于说明当前网络是正常的。

eth0
	代表着第一块网卡，如果有第二块则是eth1.
	link encap：Ethernet（以太网），指网络类型。
	Hwaddr：mac地址，物理地址。
	innet addr：当前计算机的ip地址。
	mask：子网掩码，255.255.255.0
	Bcast：当前计算机的广播地址，是192.168.1.255
	RX：接受包
	TX：发送包
	
```

##### 1.2 setup工具
setup是red hat公司开发的工具命令，如果不是redhat公司的系统，可能会不支持。
在敲击setup命令后，会出现一个图形界面供你操作，在这里你可以将虚拟linux主机作为局域网中的一台机器来配置网络。

关于DHCP，不太明白，回头查看下。
使用DHCP，如果你局域网有DHCP服务器，那么可以自动获取，如果没有的话，需要手工配置网络。
在局域网中，如果有发现了DHCP服务器，即自动分配ip地址服务，如果没有DHCP服务器，那么需要手工配置。

##### 1.3 网卡信息文件
linux系统中的网卡信息文件记录了本机的网络配置信息，通过修改该文件可以网卡信息。

在red hat系列的Linux系统中，该网络配置文件位于/etc/sysconfig/network-scripts/ifcfg-eht0

文件参数如下
```
DEVICE = eth0
	# 网卡设备名

BOOTPROTO = none
	# 是否自动获取ip地址，none、static、dhcp，要求局域网内有DHCP服务器。
	# 如果这里是DCHP，且局域网能自动获取，那么下面的参数就不用配置了。

HWADDR = 
	# mac地址，

NM_CONTROLLERD=yes，是否可以由network manager图形管理工具托管
		
ONBOOT=yes
	# 是否随网络服务启动，eth0生效。
		
TYPE=Ethernet
	# 类型为以太网。
		
UUID
	# 唯一识别码，在centos6以后，uuid异常重要，唯一识别码是一样的话，多台计算机是无法使用的。
	# 例如你克隆一个linux系统或者拷贝一个镜像系统，因为uuid一样，会造成俩个uuid完全一样，造成无法上网。

IPADDR
	# ip地址
NETMASK
	# 子网掩码
GATEWAY
	# 网关

PEERDNS
	# yes/no，是否使用DNS选项的值替代/etc/resolv.conf配置文件的值，默认yes
	# no，不更改/etc/resolv.conf的配置

DNS1
DNS2
	#DNS的配置	

IPV6INIT=no
	# ipv6没有启用
USERCTL=no
	# 不允许非root用户控制

注：
	在该文件中，要注意大小写问题，左边的是大写，右边的是小写。
	网卡设备名必须和文件名是一致的。
```

##### 1.4 主机名信息文件
主机名信息文件位于：/etc/sysconfig/network

配置如下：
- NETWORKING=yes，网络工作环境
- HOSTNAME=，主机名，在linux中并不是很重要，windows在局域网内是不能有相同ip和相同的主机名，linux则不同。设置后重启计算机则生效。

相关命令：
```
hostname命令 主机名
# 查看与临时设置主机名
```

##### 1.5 DNS配置文件
DNS配置文件地址位于：/etc/resolv.conf

该配置文件参数如下：
- nameserver：即dns地址
- search localhost：默认域名


## 网络相关命令与常识
```
ifconfig
	临时查看、配置网络命令

ifdown 网卡设备名
	禁用该网卡设备	

ifup  网卡设备名
	启用该网卡设备

netstat
	-t：列出tcp协议端口
	-u：列出udp协议端口
	-n：不使用域名与服务名，而使用ip地址和端口号
	-l：仅列出在监听状态的网络服务
	-a：列出所有的网络服务

	-tuln

	接受队列
		有多少个数据包在排列

	发送队列
	

Local Address
	0.0.0.0:3306，前面是本机地址，后面是监听端口
	127.0.0.1:631，

	22端口表示ssh服务是开启的

Foreign Address
	谁远程连接的地址

State
	状态，监听状态。tcp是

0.0.0.0
	这个地址并不是一个明确的ip地址，确切的说是一个集合，对于本机来说所有不清楚的主机和目的网络，即本机的路由里没有特定条目指明如何到达。

	对于本机来说，它就是一个收容所，所有不清楚的一律丢进去。


192.168.1.100
广播地址：192.168.1.255
DNS 127.0.1.1

```

## 虚拟机连接概念
虚拟机的网络连接方式：
1. 桥接：虚拟机和真实机进行通讯，虚拟机是利用真实机的网卡。
	配置最简单，根据真实机配置相同的网段就OK，会占用局域网一个ip地址

	我的虚拟机利用真实网卡跟真实机进行通讯，但是笔记本会经常有无线网卡和优先网卡。

	这时候就要设置虚拟机用哪个网卡来进行通讯。
	修改虚拟机桥接网卡的位置，

2. NAT：虚拟机利用虚拟网卡VM Adapter VMnet8来进行通讯。
	虚拟机只能和真实机进行相互，不能与局域网的其他机器交互。
	但是可以访问公网。

3. Host Only(仅主机模式)：虚拟机利用VM Adapter VMnet1虚拟网卡来进行通讯。
	虚拟机只能和真实机进行相互，不能与局域网的其他机器交互。
	只能跟真实机进行交互。


## VMware克隆linux问题
如果linux是通过光盘安装的，那么每个系统的UUID是不同的，但是克隆则会相同，这样会导致机器之间不同相互通讯。
解决方法：
- 删除eth0文件中mac地址行
- 删除网卡和mac地址绑定文件，/etc/udev/rules.d/70-p...
- 重启系统


a. 修改并配置ip地址
b. 启动网卡配置文件，将ONBOOT设置为yes，并重启网络服务。
c. 修改uuid，先删除网卡配置文件中的mac地址，再删除网卡和mac地址绑定文件(rm -rf /etc/udev/rules.d/70-persistent-net.rules)，最后重启系统

以上3个步骤是解决uuid重名和网卡配置。





## 其他

配置完系统的网卡信息后，需要配置虚拟机的桥接网卡信息。