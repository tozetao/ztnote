### Lock

锁是一种对数据进行排他性访问的实现，在操作锁时，首先对数据进行加锁并获得该锁，加锁成功后数据就有了排他性访问的能力，然后对数据执行一系列操作，最后释放锁给其他程序使用。

一般都是先获取锁，然后执行操作，最后释放锁的动作流程。



关于锁的分类

- 系统层面上的锁
- 语言层面上的锁
- 应用软件上的锁，例如MySQL的锁



### watch与锁

watch的本质是乐观锁，使用watch命令代替数据进行加锁，watch只会在数据被其他客户端抢先修改了的情况下通知执行该命令的用户，而不会阻止其他客户端对数据进行修改，所以watch命令被称为乐观锁。



乐观锁的问题

watch命令监视的key，如果该key被频繁的改变可能会引起性能问题的。因为程序在完成一个事务的时候，可能会因为watch监视的key发生变动而执行失败，如果程序有实现反复尝试执行该事务，会导致事务反复的重试。



### 分布式锁

分布式锁也有类似的先获取锁，执行操作，最后释放锁的过程，只是分布式锁的作用的范围不同。

分布式的范围并不是针对同一个进程的多个线程使用，也不是同一台机器上的多个进程使用，它是针对redis本身，由不同redis客户端进行获取锁和释放锁的。



### Implement

1. 能够正常的加锁与释放锁，保证在并发时程序能够并行的执行

2. 要求有超时限制特性
   如果获得锁的进行在指定的时限内未能完成操作，那么锁将会自动释放，避免死锁



加锁实现

加锁可以使用redis的set命令配置NX选项来实现，set NX只允许设置不存在的key，同时redis的set操作是原子级别的。

在实现上使用set NX设置要加锁的key，值为一个uuid，如果设置失败会在一定时间内尝试，直到key设置成功或者时间过期；key设置成功即代表加锁成功，将返回加锁key的值，即uuid。

```php
function getRedis(){}

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



锁超时的实现

上述的代码中实现了加锁，但是没有实现锁的自动超时。如果程序在加锁成功后发生奔溃，不能释放锁的话会导致死锁，让其他程序会一直处于阻塞中。

程序在取的锁之后给锁加上过期时间，由redis自动删除超时的锁，以此来实现锁的超时。

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



解锁

使用watch命令监视代表锁的key，然后检查加锁的key的值与与解锁传递进来的值是否一致，确认值没有变化则删除，这一步可以防止程序错误而导致释放同一个锁多次。

```php
function releaseLock($key, $uuid)
{
    //监视key
    $redis = getRedis();
    $redis->watch($key);
    
    //判断uuid与key的值是否一致
    $redis->mutli();
    $t = $redis->get($key);
    $redis->exec();
    if($t === $uuid)
    {
        $redis->delete($key);
        $redis->exec();
        return true;
    }
    $redis->unwatch();
    return false;
}
```



### 计数信号量

计数信号量是一种锁，能够限制一项资源最多能够他同时被多少个进程访问，通用用于限制能够同时使用的资源数量。



与分布式锁的区别

计数信号量也有加锁，解锁的操作流程，不同的是允许多个进程同时得到资源的锁，同时一个进程无法加锁成功是直接返回失败，而不会让其一直等待。



考虑的问题

如何判断客户端已获得锁。

如何处理客户端获取锁之后奔溃的情况。

如何处理锁超时情况。





实现

使用有序集合实现，key是uuid，分数值是当前时间戳，用以实现锁的过期时间，存在的成员数量等于使用资源的客户端数量。

- 加锁

  客户端在获取锁的时候会往有序集合中添加成员，成员的排名意味着获取锁的当前客户端数量。

  如果成员的排名在限制使用资源的客户端数量之内，那么表示获取锁成功，返回uuid，否则删除已添加的成员并返回false。

- 过期实现

  在每次去获取资源锁的时候，移除有序集合中过期的成员

```php
function acquireSemaphore($key, $limit, $timeout=10)
{
    $uuid = uniqid();
    $now = time();
    
    $redis = getRedis();
    $redis->multi();
    
    //移除过期的成员
    $redis->zremrangebyscore($key, '-inf', $now-$timeout);
    $redis->zadd($key, $uuid, $now);
    $redis->zrank($key, $uuid);
    if($redis->exec()[-1] < $limit)
        return $uuid;
    
    $redis->zrem($key, $uuid);
    return false;
}

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