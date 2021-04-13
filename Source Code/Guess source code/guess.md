接入电话



3108

3109









## logic_sup

逻辑督程，它的启动函数是start_link([Type])，Type的值是在启动erlang运行时系统传递的，值是center或guess。



center是中央服节点，启动的子进程有：

- sys_cross

  维护中央服与游戏服的连接

- sys_code

  负责代码更新

- cross_xyxw

  十一选五进程

- db_1

  数据库模块

启动模式是one_for_one，每3秒内重启次数超过3次则关闭。





guess是游戏服节点，启动的子进程有：























## tcp_sup

督程





## cron_sup

督程





1. center和guess俩种启动方式有什么不同。
2. 搞清楚玩家怎么登陆
3. 彩金接口怎么接入