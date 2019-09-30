### 进程配置

```ini
master_process on|off;
```

该指令处于全局作用域，表示是否以master/worker模式工作，默认on。

如果设置为off，则有master进程亲自处理请求，不启动worker；否则master进程只起管理worker进程的作用。



```ini
worker_processes n;
#example: worker_process 1;
```

Nginx worker进程的数量，默认为auto（1.9.10之后），处于全局作用域。通常设定为cpu逻辑核心数，这样可以发挥并行的优势。如果设置为auto，则有Nginx自动设置为cpu核心数。



```ini
worker_cpu_affinity n;
```

处于全局作用域，表示worker进程绑定cpu选项，默认不设置。

如果不设置则不同的worker会绑定到不同的cpu核心执行，这样会降低并发的效果。如果设置为auto（1.9.10），系统自动绑定到合适的cpu核心。也可以手动使用掩码设置。

例如4个worker设置为：worker_cpu_affinity 0001 0010 0100 1000;



```ini
worker_priority n;
```

worker的优先级，linux在新一轮的调度中，执行顺序和时间分片长度都与worker的优先级有关，值越小优先级越高。默认是0，取值范围是-20到20。



```ini
worker_rlimit_nofile n;
```

处于全局作用域，指定通过worker进程可以打开的最大文件的数值。





### events指令

以下介绍events指令相关的配置。

```ini
use method;
```

处于events指令作用域中，表示选择哪种IO复用方法。例如：select、epoll等。

```ini
worker_connections number;
```

处于events作用域，每个表示worker可以同时处理的最大连接数，默认1024。该值不能超过worker_rlimit_nofile的大小。

注：如果Nginx作为反向代理服务器，需要耗费2倍的连接数（服务器+客户端）



```ini
accept_mutex  on|off;
```

处于events指令的作用域内，1.11.3之前的版本默认值是on，之后默认值是off；

该选项是为了负载均衡设计，如果是on，nginx会使worker一个一个的接收请求，而不是一起唤醒去争抢该请求（惊群问题）。

但是实际上负载大的情况下处于休眠的worker进程也不多，关闭该选项能够加快tcp连接的建立速度，所以之后该选项默认关闭了。

```ini
multi_accept on|off; 
```

默认off。如果打开，则一个worker会同时accept多个连接。关于这个的讨论比较少，个人觉得是打开这个选项可能会导致负载很不平衡，但是对效率应该会略有提升。

```ini
accept_mutex_delay number; 
```

当accept_mutex开启时，若尝试accept失败，worker会最多等待number秒来重试。默认值500ms



### example

```conf

worker_rlimit_nofile 51200;

events
{  
	use epoll;
	worker_connections 51200;
	multi_accept on;
}
```

