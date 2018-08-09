公平信号量

上个版本的基本信号量，当各个系统的时间不一致时就会出现不公平的情况，时间运行慢的系统可能会偷走时间运行快的系统的信号量。



需要的数据结构

一个计数器，用于对信号量进行排名

一个有序集合，存储信号量的超时时间，key是uuid，值是信号量的时间戳。

一个有序集合：存储信号量的计数器，key是uuid，值是计数器

这俩个集合存储着信号量拥有者



实现

- 从超时有序集合中移除过期的元素，即移除过期的信号量
- 使用超时有序集合与信号量有序集合进行交集计算，并将结果存储到信号量有序集合中。由于超时有序集合中移除了过期元素，所以交集计算的结果都是未过期的信号量
- 即计数器进行自增操作并将结果存储到信号量有序集合中，同时将时间戳存储到超时有序集合中。
- 在完成上述操作后，检查当前添加的标识符在信号量有序集合中的排名，如果排名在允许使用信号量个数限制下，则表示客户端取的信号量，相反客户端未取的信号量，需要从集合中移除相关的元素。

通过另外一个集合来计算信号量是否在可使用数量限制之内，就避免了因为各个系统时间戳不相同造成的不公平现象。

```php
function getRedis()
{
    return new Redis('127.0.0.1');
}

function acquire_fair_semaphore($key, $limit, $timeout = 10)
{
    $redis = getRedis();
    
    $uuid = unique();
    $now = time();
    
    $ownKey = $key . ':own';
    $counter = $key . ':counter';
    
    $redis->multi();
    
    $redis->zremRangeByScore($key, '-inf', $now - $timeout);
    $redis->zinterstore($ownKey, [$ownKey, $key]);
    
    $redis->incr($counter);
    $identify = $redis->exec()[2];
    
    $redis->zadd($key, $uuid, $now):
    $redis->zadd($ownKey, $uuid, $identify);
    
    $redis->zrank($ownKey, $uuid);
    $order = $redis->exec()[1];
    if($order <= $limit)
        return $uuid;
    
    $redis->zrem($key, $uuid);
    $redis->zrem($ownKey, $uuid);
    $redis->exec();
    return false;
}


function release_lock($key, $uuid)
{
    $redis = getRedis();
    $ownKey = $key . ':own';
    $redis->zrem($ownKey, $uuid);
    $redis->zrem($key, $uuid);
    return true;
}
```







