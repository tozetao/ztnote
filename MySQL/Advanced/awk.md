## awk入门 ##
测试环境：
mysql5.1、一张有1000万条记录的数据库表。

awk是一个小脚本，能方便的统计数据。

### mysql命令 ###
show status;
该命令用于显示mysql运行状态。

Queries：显示sql语句执行的次数。

Threads_connected：mysql当前打开的连接数的数量。
Threads_running：mysql激活的线程数，正工作中的。
这俩个参数组合起来就是有多少个连接正在工作。

show status能通过like关键字配合来查询指定参数的状况。