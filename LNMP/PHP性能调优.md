### Opcache

```ini
# 每隔多长时间去检查脚本时间戳是否有更新，以秒为单位。
opcache.revalidate_freq=60

# 如果启动，Opcache会每隔revalidate_freq设置的描述检查脚本更新，如果禁用你需要重启web服务器来使文件系统更改失效。
opcache.validate_timestamps=0

# 允许缓存opcode文件的最大数
opcache.max_accelerated_files

# opcache可用的内存大小
opcache.memory_consumption=512
```







### Opcache file cache

让Opcache把opcode缓存缓存到外部文件中，这样PHP会在/tmp目录下Cache一些Opcode的二进制导出文件，可以跨PHP生命周期。

```ini
opcache.file_cache=/tmp
```



### HugePage

在系统中开启Hugepage，比如分配512个预留的大页内存：

```ini
sysctl vm.nr_hugepages=512
# 查看配置
# cat /proc/meminfo | grep Huge
```

然后在php.ini中加入:

```ini
opcache.huge_code_page=1
```

这样PHP会把自身的text段，以及内存分配中的huge都采用大内存页来保存，减少TLB miss, 从而提高性能。





HugePage相关测试：

> http://www.laruence.com/2015/10/02/3069.html

