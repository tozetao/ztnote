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
        else if($redis->ttl($key) === -2)
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





考虑的问题

如何判断客户端已获得锁。

如何处理客户端获取锁之后奔溃的情况。

如何处理锁超时情况。



> 加锁的实现

需要一个集合存储成功获取锁的成员，同时获取锁的成员也需要有超时时间来避免死锁，因此采用有序集合才实现信号量锁。

有序集合的成员是uuid，分值是当前时间戳，用以实现锁的过期时间，集合中成员的数量等于成功获取信号量锁的客户端数量。

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





这是一个不公平的信号量。

客户端在去获取信号量的时候，会假定每个进程访问到的系统时间是相同的，这一假设在多主机环境下不成立；例如对于系统A和系统B，系统A比系统B快10毫秒，如果系统A获得最后一个信号量，系统B只需要在这10毫秒内尝试获取锁就可以在A不知情的情况下抢占原本属于A的信号量。



公平信号量

当各个系统的系统时间并不完全相同时，前面的基本信号量就会出现问题。时间运行慢的系统上的客户端可以偷走时间运行快的客户端已经取得的信号量，导致信号量变得不公平。



一个计数器：用于确保最先对计数器执行自增操作的客户端能够获得信号量。

一个有序集合：存储信号量拥有者，key是uuid，value是计算器产生的值。



从超时的有序集合中移除过期元素。

对超时有序集合、拥有信号量集合执行交集计算，并将计算结果保存到信号量拥有者有序集合里面，覆盖有序集合中原有的数据。

首先交集计算只会保留相同的元素，先移除过期元素再将俩个集合进行交集计算，那么拥有信号量的集合存储的就是未过期的元素了。





公平信号量

上个版本的基本信号量，当各个系统的时间不一致时就会出现不公平的情况，时间运行慢的系统可能会偷走时间运行快的系统的信号量。



需要的数据结构

一个计数器，用于对信号量进行排名

一个有序集合，存储信号量的超时时间，key是uuid，值是信号量的时间戳。

一个有序集合：存储信号量的计数器，key是uuid，值是计数器

这俩个集合都存储着信号量拥有者



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



刷新信号量

刷新信号量即增加信号量的存活时间，因为公平信号量区分开超时有序集合和信号量拥有者有序集合，所以只需要对超时有序集合进行更新，就可以立即刷新信号量的超时时间了。



公平信号量的竞争

这是因为redis的事务只是保存了多个操作的原子性。

多个事务仍然是可以并发执行的。





公平信号量的应用场景

