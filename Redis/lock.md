### Lock

锁是一种对数据进行排他性访问的实现，在操作锁时，首先对数据进行加锁并获得该锁，加锁成功后数据就有了排他性访问的能力，然后对数据执行一系列操作，最后释放锁给其他程序使用。

一般都是先获取锁，然后执行操作，最后释放锁的动作流程。



关于锁的分类

- 系统层面上的锁
- 语言层面上的锁
- 应用软件上的锁，例如MySQL的锁



## 分布式锁

分布式锁也有类似的先获取锁，执行操作，最后释放锁的过程，只是分布式锁的作用的范围不同。

分布式的范围并不是针对同一个进程的多个线程使用，也不是同一台机器上的多个进程使用，它是针对redis本身，由不同redis客户端进行获取锁和释放锁的。



### watch与锁

watch的本质是乐观锁，使用watch命令代替数据进行加锁，watch只会在数据被其他客户端抢先修改了的情况下通知执行该命令的用户，而不会阻止其他客户端对数据进行修改，所以watch命令被称为乐观锁。



watch命令只能监视key，保证事务执行时数据的一致性。

但是如果watch命令监视的key可以被频繁修改的话，那么会引起性能消耗，因为程序如果因为watch监视的key发生变动而执行失败，一般实现时会让程序反复尝试执行该事务，这种实现其实并不是很好的。



### 分布式锁的实现

需求：

- 能够正常的加锁与释放锁，保证在并发时程序能够并行的执行
- 要求有超时限制特性，如果获得锁的进行在指定的时限内未能完成操作，那么锁将会自动释放，避免死锁的发生



> 加锁的实现

加锁可以通过redis的set命令来实现，set命令的NX选项只允许设置不存在的key，如果一个key存在将会设置失败，同时redis的单个操作是原子级别的。

客户端会对要加锁的数据的key进行set NX操作，如果set成功表示加锁成功，否则表示加锁失败。如果加锁成功会返回uuid用于后续结果；如果加锁加锁失败的话会在一定超时内进行重试。

```php
function lock($key, $timeout = 50000)
{    
    $redis = getRedis();
    $start = time();
    $end = $start + $timeout;
    
    //uuid的生成自己实现
    $uuid = uuid();	
    
    while($start < $end)
    {
        if($redis->setNX($key, $uuid))
            return $uuid;
        
        sleep(0.2);
    }
    
    return false;
}
```



> 锁超时的实现

上述的代码中实现了加锁，但是没有实现锁的自动超时。如果程序在加锁成功后发生奔溃，不能释放锁的话会导致死锁，让其他程序会一直处于阻塞中。

因此程序在取的锁之后给锁加上过期时间，由redis自动删除超时的锁，以此来实现锁的超时。

同时为了确保客户端程序在取得锁奔溃之后锁能超时（程序可能在setNX和EXPIRE命令之间奔溃），程序在获取锁失败之后会检查锁的超时时间，并未没有超时时间的锁设置超时时间。

```php

function lock($key, $timeout = 50000)
{    
    $redis = getRedis();
    $start = time();
    $end = $start + $timeout;
    
    //uuid的生成自己实现
    $uuid = uuid();	
    
    while($start < $end)
    {
        if($redis->setNX($key, $uuid))
        {
            $redis->expire($key, $timeout);
            return $uuid;
        }
        else if($redis->ttl($key) === -1)
            $redis->expire($key, $timeout);
        
        sleep(0.2);
    }
    
    return false;
}
```



> 解锁

解锁的实现很简单，只要把加锁数据的key释放即可。

解锁时会判断传递进来的uuid是否与加锁的uuid一致，如果一致则删除，否则解锁失败。

使用watch命令监视代表锁的key，然后检查加锁的key的值与与解锁传递进来的值是否一致，确认值没有变化则删除，这一步可以防止程序错误而导致释放同一个锁多次。

```php
function releaseLock($key, $uuid)
{
    //监视key
    $redis = getRedis();
    $redis->watch($key);
    
    //判断uuid与key的值是否一致
    $values = $redis->mutli()->get($key)->exec();
    if($values && $values[0] === $uuid)
    {
        $redis->delete($key);
        return true;
    }
    $redis->unwatch();
    return false;
}
```



## 计数信号量

计数信号量是一种锁，能够限制一项资源最多能够被同时被多少个进程访问，通用用于限制能够同时使用的资源数量。

计数信号量也有加锁，解锁的操作流程，与分布式锁不同的是允许多个进程同时得到资源的锁，同时一个进程无法加锁成功是直接返回失败，而不会让其一直等待。



### 不公平信号量

> 加锁的实现

需要一个集合存储成功获取锁的成员，同时获取锁的成员也需要有超时时间来避免死锁，因此采用有序集合才实现信号量锁。

有序集合的成员是uuid，分值是当前时间戳，用以实现锁的过期时间，集合中成员的数量等于成功获取信号量锁的数量。

客户端申请加锁时会向有序集合中添加成员，如果所添加的成员的排名未超过允许同时使用的资源数量限制，那么表示加锁成功，否则加锁失败。添加的成员为uuid，分值为当前操作时间戳。



> 过期实现

由于有序集合的分值是时间戳，因此在每次客户端去获取资源锁的时候同时移除有序集合中过期的成员，这样可以避免死锁的发生。



代码实现：

```php
function acquireSemaphore($key, $limit, $timeout=10)
{
    $uuid = uniqid();
    $now = time();
    
    $redis = getRedis();
    $redis->multi();
    
    //移除过期的成员
    $redis->zremrangebyscore($key, '-inf', $now - $timeout);
    $redis->zadd($key, $uuid, $now);
    $redis->zrank($key, $uuid);
    $result = $redis->exec();
    if($result && $result[2] < $limit)
        return $uuid;
    
    $redis->zrem($key, $uuid);
    return false;
}
```

解锁的实现很简单，只需要根据uuid删除有序集和中的成员即可。

```php
function releaseLock($key, $uuid)
{
    $redis = getRedis();
    return $redis->zrem($key, $uuid);
}
```



缺陷

这是一个不公平的信号量，客户端在去获取信号量的时候，会假定每个进程访问到的系统时间是相同的，如果客户端是不同的机器，这一假设不成立。

例如对于系统A和系统B，系统A比系统B快10毫秒，如果系统A获得最后一个信号量，系统B只需要在这10毫秒内尝试获取锁就可以在A不知情的情况下抢占原本属于A的信号量，也就是说有可能会获取超出数量限制的信号量。



### 公平信号量

由于使用时间戳来进行排名会造成不公平信号量，因此需要一个计数器对每个信号量分配一个唯一的ID，根据该ID来进行排名。



使用的数据结构：

- 一个计数器

  为每个客户端获取信号量时分配一个唯一的ID

- 一个键是uuid，分值是时间戳的有序集合，用以实现过期时间

- 一个键是uuid，分值是计数器ID的有序集合，用以实现排名，限制同时可获取的信号量的数量。



> 加锁实现

客户端在获取一个信号量时，会经过以下逻辑：

- 移除时间戳有序集合中过期的信号量

- 对时间戳有序集合与计数器有序集合做交集操作，并将交集的过期存储到计数器有序集合中。

  因为在时间戳有序集合中移除了过期的信号量，而计数器有序集合过期的信号量没有移除，通过交集操作不仅移除了计数器有序集合中过期的信号量，也统一了俩个集合中的成员。

- 为信号量分配计数器ID，将计数器ID存储到计数器有序集合，将信号量的时间戳存储到时间戳有序集合中。

- 检查当前信号量在计数器有序集合中的排名，如果满足同时访问信号量数量的限制，则获取成功，返回uuid；否则获取失败，需要从集合中移除相关的元素

```php
function lock($key, $limit = 5, $expire = 10)
{
    $lock = acquireLock('counter', 10);

    if ($lock) {
        $timestampKey = 'lock:timestamp:' . $key;
        $countKey = 'lock:count:' . $key;

        $now = time();
        $uuid = uniqid();

        $redis = Singer::getInstance();

        // 信号量计数器
        $count = $redis->incr('count');

        $redis->multi();
        // 移除过期的信号量
        $redis->zRemRangeByScore($timestampKey, '-inf', time() - $expire);

        // 使用交集合并俩个集合
        $redis->zInter($countKey, [$timestampKey, $countKey], [0, 1]);

        // 添加时间戳、添加信号量的计数器
        $redis->zAdd($timestampKey, $now, $uuid);
        $redis->zAdd($countKey, $count, $uuid);
        $redis->zRank($countKey, $uuid);
        $result = $redis->exec();

        echo $result[4], "\n";

        // 判断信号量的排名是否超过限制
        if ($result[4] < $limit) {
            echo "lock success\n";
            releaseLock('counter', $lock);
            return $uuid;
        } else {
            releaseLock('counter', $lock);
            // 移除获取失败的信号量
            $redis->zRem($timestampKey, $uuid);
            $redis->zRem($countKey, $uuid);
        }
    }

    return false;
}

```

说明：由于获取信号量的计数器会发生竞争，因为进行加锁来操作信号量，消除竞争条件。redis加锁与解锁速度是非常快的，因此能够在同一时间获取多个信号量。



> 释放信号量

```php
function release($key, $uuid) {
    $timestampKey = 'lock:timestamp:' . $key;
    $countKey = 'lock:count:' . $key;
    $redis = Singer::getInstance();
    return $redis->multi()->zRem($timestampKey, $uuid)->zRem($countKey, $uuid)->exec();
}
```



> 刷新信号量

刷新信号量即增加信号量的存活时间，因为公平信号量区分开超时有序集合和信号量拥有者有序集合，所以只需要对超时有序集合进行更新，就可以立即刷新信号量的超时时间了。
