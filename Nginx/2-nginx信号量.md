## nginx信号量
### 1. nginx的信号控制
| 参数 | 说明 |
|-----|-----|
| TERM,INT | quick shutdown，快速杀死进程 |
| QUIT | Graceful shutdown，优雅的关闭进程，即等待请求结束再关闭|
| HUP | Configuration reload，Start the new worker processes with a new configuration Graceful shutdown the old worker processes,改变配置文件，平滑的重读配置文件 |
| USR1 | reopen the log files，重读日志文件，在日志按月/日分割时有用 |
| USR2 | Upgrade Executable on the fly 平滑的升级 |
| WINCH | Gracefully shutdown the worker processes 优雅关闭旧的进程(配合USR2来进行升级) |

关于usr2，

### 2. kill
kill -信号选项 主进程号

```
ps anx | grep nginx
# 使用上述命令可以看到nginx的master、worker俩个进程。
# 一般来说nginx会有1个主进程，多个子进程，主进程主要是用于协调子进程的响应任务以及处理子进程的生命周期


kill -INT pid
# 用于杀死

kill -HUP 4863

kill -USR1 nginx_pid
# nginx_pid即nginx的主进程id

kill -HUP `more /nginx/logs/nginx.pid`
# `more /nginx/logs/nginx.pid`将nginx的pid
```

### 3. access.log
该文件会一直记录访问源。
linux并不是通过文件名来查找文件的，而是通过i节点来对文件进行读写。

例如/logs/access.log文件，假设nginx服务一直开启并且在记录客户访问记录，这时候通过mv命令来改access.log文件的名字，那么nginx仍然会将记录一直记录在该文件中。

因为进程一直是通过i节点来读写文件的。

### 4. ./sbin/nginx
nginx的二进制执行文件，你可以通过-h来查看帮助说明。
一些命令的使用也是相同的道理。

## linux信号量
信号与信号量是不同的事务。
信号量是一种进程间的通信机制，用于防止出现因为多个程序同时访问一个共享资源而引发的一系列问题。

它通过生成并使用令牌来授权保证在任意时刻只有一个执行进程访问代码的临街区域，临界区域是指执行数据更新的代码需要独占式地执行，信号量就是这样的一种机制。