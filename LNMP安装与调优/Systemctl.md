systemctl

> /etc/systemd/system/

ubuntu的systemctl的脚本存放在该目录中。





> /usr/lib/systemd/system

centos的systemctl脚本存在在这个目录中。





文档说明书：

https://wiki.archlinux.org/index.php/systemd_%28%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87%29#systemd_%E5%9F%BA%E6%9C%AC%E5%B7%A5%E5%85%B7





Type=forking

如果设置为forking，systemd预计ExecStart=配置的进程将会调用fork()作为其启动的一部分。









https://github.com/RedisBloom/RedisBloom/archive/refs/tags/v2.2.4.tar.gz





redis-cli -p pid shutdown

pid是redis的进程id