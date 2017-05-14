## Redis简介
redis是一个高级的key/value存储系统，与Memcached相比它能对数据持久化，所以redis才叫做存储系统，而不是缓存系统。

### 安装
- 下载源码解压
- 编译，make（redis帮我们配置了源码，所以不用configure）
- 安装，make PREFIX=/usr/local/redis install，这里安装并指定路径

注1：make需要根据系统位数编译，如果是32位系统，需要make 32bit
注2：PREFIX必须大写

### 启动
- 配置，这里从源码文件夹中拷贝一份配置文件放到安装目录中。
- 指定配置文件并启动，./bin/redis-server ./redis.conf

说明：通过配置文件的daemonize选项，该选项表示是否后台运行。

### 连接
执行redis客户端连接工具，./bin/redis-cli [-h localhost -p 6379]

### 安装目录说明
**bin目录**
可执行文件目录，分别有：
- redis-benchmark，性能测试工具
- redis-check-aof，日志文件检测工具（比如断电造成日志损毁，可以检测并修复）
- redis-check-dump，快照文件检测工具，效果如上
- redis-cli，客户端工具
- redis-server，服务端，redis服务进程

