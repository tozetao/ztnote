<?php
/*
iptables
	linux系统中防火墙的管理工具。

netfilter
	防火墙的真正实现，它是linux内核中实现包过滤的内部结构。


修改生效问题
	如果是通过命令来配置规则，通过/etc/rc.d/init.d/iptables.save保存设置，需要重启服务。

	如果通过修改配置文件的话，需要重启服务再save保存，因为save保存在服务重启的时候重新加载，这样就相当于回滚到上一次配置了。


命令使用
	input链设置：

	-A：指定链名，例如input、output、
	-p：指定协议
	-d：指定目标地址
	-j：指定动作，如accept()、reject、
	-dport：指定目标端口，destination port，
	-sport：指定源端口，source port

	example：
		iptables -A INPUT -p tcp --dport 22 -j ACCEPT   
		iptables -A OUTPUT -p tcp --sport 22 -j ACCEPT   


	/etc/rc.d/init.d/iptables save：将命令修改写入到/etc/sysconfig/iptables文件中。


iptables -F：清除预设表filter中所有规则链的规则
iptables -X：清除预设表filter使用者

 */