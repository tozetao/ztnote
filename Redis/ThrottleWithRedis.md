#### 初始化

```lua
local function reset()
    redis.call('hmset', KEYS[1], 'start', ARGV[2], 'end', ARGV[2] + ARGV[3], 'count', 1)
    return redis.call('expire', KEYS[1], ARGV[3] * 2)
end
```

当key不存在时会调用该函数，reset函数是桶的初始化函数。

ARGV分别为：

- ARGV[2]：当前时间戳，单位秒
- ARGV{3}：桶的持续时间，单位秒
- ARGV[4]：允许在桶的持续时间内访问的最大次数。

桶的数据结构是map，键分别为：

- start：当前时间戳
- end：失效时间戳，由当前时间戳 + 桶的持续时间。
- count：在桶的持续时间内的访问次数。

start和end标明了桶的有效范围。



```lua
if redis.call('EXISTS', KEYS[1]) == 0 then
    return {reset(), ARGV[2] + ARGV[3], ARGV[4] - 1}
end
```

这里的代码做初始化工作，当KEY（桶）不存在时，就进行初始化。

返回值{A, B, C}，A表示是否有效，1允许继续访问，0则禁止访问。B表示桶的失效时间点，C表示桶的剩余的可访问次数。



#### 访问次数的增加

```lua
if ARGV[1] >= redis.call('HGET', KEYS[1], 'start') and ARGV[1] <= redis.call('HGET', KEYS[1], 'end') then
    return {
        tonumber(redis.call('HINCRBY', KEYS[1], 'count', 1)) <= tonumber(ARGV[4]),
        redis.call('HGET', KEYS[1], 'end'),
        ARGV[4] - redis.call('HGET', KEYS[1], 'count')
    }
end
-- ARGV[1] >= redis.call('hget', KEYS[1], 'start')
-- 判断当前时间大于start值
-- ARGV[1] <= redis.call('hget', KYES[1], 'end')
-- 同时小于end值

-- 上面俩个条件用于判断当前时间戳是否在桶的持续时间内。

-- redis.call('hincrby', KEYS[1], 'count', 1) <= ARGV[4]
-- 在桶内的访问次数是否超过最大值

-- redis.call('hget', KEYS[1], 'end')
-- 返回桶的失效时间节点

-- ARGV[4] - redis.call('hget', 'KEYS[1]', 'count')
-- 在桶内的剩余访问次数

--  上面三个是返回值。
```

ARGV[1]是当前时间戳，精确到微秒。当桶存在时，那么只增加对桶的访问次数，







#### 实现原理

限流也就是希望限制相同请求来某个时间段内的访问次数，DurationLimiter使用哈希map来实现，键start与键end标识了这个时间段，就相当于划分出一个桶（段），统计在这个桶内的访问次数就能够达到限制的效果。

如果当前时间戳超出end，也就是桶失效了，这时候就需要调用reset函数来再次进行初始化了。