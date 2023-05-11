### 队列任务配置

- retry_after

  该选项指定了任务最多执行多少秒后就被当作失败重试，它应该设置为最长耗时的任务所对应的时间。



- queue

  默认值是default。如果你在分发一个任务没有指定要处理的队列，那么会被放到连接配置中queue属性所定义的队列。





### 进程配置

- sleep

  进程没有任务处理时的睡眠时间，如果进程处理的任务是很紧急的任务，那么休眠时间不要太长。

  单位秒。

- tries

  任务失败的最多重试次数。

  如果任务运行时抛出异常，这个任务就会自动释放回队列，这样它就能再重新运行了。tries就是定义重试运行的次数。

  警告：如果没有设置该值，任务就会永久重试。

- delay

  一个任务失败后，延迟多长时间重试。单位是秒。

- timeout

  进程处理一个任务的最长时间，超过该时间，进程就会被杀掉。

- quiet

  不输出任何内容。



### timeout与retry_after

timeout是管理进程的最大执行时间，retry_after是管理任务的最长执行时间。这俩项配置是不一样的，当时可以同时存在。

timeout应该永远比retry_after短几秒，这样能够保证队列进程总能在任务失败重试前就被杀死了，防止该任务被重复执行。


我们希望任务的出错重试次数是符合预期的，比如出错、发生异常。超时这种情况并不符合我们的预期，所以才这种设置。









### 命令

指定进程的链接

> php artisan queue:work redis

指定redis连接。

对应到代码需要调用onConnection方法。



指定进程处理的任务类型

> php artisan queue:work redis --queue=emails

指定处理emails类型的队列。

对应到代码需要调用任务实例的onQueue方法。







### 原理

任务进程在实现上是使用sleep定时睡眠，然后读取存储在redis中的队列任务，再处理任务。

本质上就是轮询的去读取redis来实现的，并没有什么特别之处。当然实现上可能复杂些。





设计

- 延迟队列（delay queue）

  以delayed标识，存储队列中的延时任务。延时任务指的是任务出错后，延迟多少秒再执行，由delay选项指定。

- 保留队列（reserved queue）

  充当一个中间层队列。

  当从任务队列中取出一个任务后，改变任务的附加数据（重试次数、可执行时间），会将任务再次放入该队列中。

  后续根据任务的执行结果再来处理该队列中的任务。

- 任务队列（default queue）

  存储队列进程要处理的任务







第一步：

从延迟队列（queue:default:delayed ）有序集合中获取可以处理的"延迟任务"，rpush到任务队列（queue:default）中。

执行语句：

> redis> eval "Lua脚本" 2 queues:default:delayed queues:default 当前时间戳

lua脚本：

```lua
-- 从delay队列中获取所有过期的任务
local val = redis.call('zrangebyscore', KEYS[1], '-inf', ARGV[1])

-- 如果数组中有值，我们将从第一个队列中删除这些值，然后将它们以100为一个块添加到目标队列中，这样可以非常安全地将所有适当的作业移动到目标队列上
if(next(val) ~= nil) then
    redis.call('zremrangebyrank', KEYS[1], 0, #val - 1)
 
    for i = 1, #val, 100 do
        redis.call('rpush', KEYS[2], unpack(val, i, math.min(i+99, #val)))
    end
end
```



第二步：

从reserved队列中获取已过期的任务，rpush到任务队列中。reserved队列是redis有序集合。

> redis> eval "Lua脚本" 2 queues:default:reserved queues:default 当前时间戳

使用的lua脚本同上一步操作脚本相同。



第三步：

从任务队列（queue:default）中获取一个任务，增加起尝试（attempts）次数，并将任务保存到reserved队列中，该任务的score值是当前时间 + 任务可执行时间（由retry_after选项配置）。

执行语句：

> redis> eval “Lua脚本” 2 queues:default queues:default:reserved 任务超时时间戳

lua脚本：

```lua
local job = redis.call('lpop', KEYS[1])
local reserved = false
 
if(job ~= false) then
    -- Increment the attempt count and place job on the reserved queue...
    reserved = cjson.decode(job)
    reserved['attempts'] = reserved['attempts'] + 1
    reserved = cjson.encode(reserved)
    redis.call('zadd', KEYS[2], ARGV[1], reserved)
end
 
return {job, reserved}
```



第四步：根据任务的执行结果有不同的处理效果。

如果成功执行上面获取的任务，就将该任务从reserved队列中删除，至此任务执行完毕。

执行语句：

> redis> zrem queue:default:reserved '具体任务'





如果任务执行失败，会出现俩种情况：

任务失败次数（attempts）未达到重试次数阈值，会将该任务从reserved队列中删除并添加到delayed队列中，score是该任务下一次执行的时间戳，即当前时间戳 + delay配置项

执行语句：

> redis> EVAL "Lua脚本" 2 queues:default:delayed queues:default:reserved "失败的任务" 任务延迟执行的时间戳

Lua脚本：

```lua
-- Remove the job from the current queue...
redis.call('zrem', KEYS[2], ARGV[1])
 
-- Add the job onto the \"delayed\" queue...
redis.call('zadd', KEYS[1], ARGV[2], ARGV[1])
 
return true
```



任务失败次数超过指定的重试阈值，就将该任务从queue:default:reserved中移除。

执行语句：

> ZREM queue:default:reserved "失败的任务"







总结：

reserved队列充当一个中间层，当从default队列中取出一个任务执行时，任务的重试次数，最大执行时间发生改变，这时我们会用reserved队列来保存已经发生过改变的任务。

而无论一个任务的执行成功还是失败，都会将该任务从reserved队列中删除。











### 关联的redis命令

> rpush key value [value...]

向存于key的列表尾部插入所有指定的值，如果key不存在则创建一个列表再进行push操作；如果key不是一个列表将会返回一个错误。

返回值：返回push操作后队列的长度。



> lpop key

移除并返回key对应的list中的第一个元素。

返回值：返回第一个元素的值，或者当key不存在时返回nil。



> zadd key [NX|XX] [CH] [INCR] Score Number [...]

将所有元素添加到键为key的有序列表中。元素是分数（Score）、成员（Number）对，可以指定多个。

如果key不存在则创建一个新的有序列表再添加元素，如果key存在，添加的成员不存在就作为一个新的元素加入，否则将会更新原有成员的分数。

- Score能够表示的整数范围

  Redis有序集合的分数使用双精度64位浮点数，能够表示的整数范围是-2^53到+2^53

- 排序规则

  有序集合中的每个Number都是唯一的，分数是按照递增的顺序排序。

- 相同分数的成元

  略...

返回值：如果是新增的元素返回新增元素的个数；如果是更新成员的分数，则返回分数值（字符串型）。



> zrem key member [member...]

从键为key的有序列表中删除指定的元素。

返回值：返回删除的成员个数。



> zremrangebyran key start stop

从键为key的有序集合中，移除指定排名区间内的所有元素。

start、stop俩个参数代表区间，排名都是从0开始，例如start = 0，stop = 1，表示移除第一个、第二个元素。

这些参数也可以是负值，表示从最高排名开始数。例如-1是分数最高的元素，-2是分数第二高的元素。

返回值：返回删除的元素个数。



> zrangebyscore key min max [withscores] [limit offset count]

获取key对应的有序集合中score在min和max之间的成员（包括min和max的成员）。min和max可以是-inf和+inf，这样就可以在不知道有序集合中最低分数和最高分数的时候使用zrangebyscore。

默认情况下区间的是闭区间，也可以通过给参数前面加（ 符号来表示开区间（小于或大于）。

返回值：默认返回区间中的成员，如果withscores参数存在，将会返回成员的分数。













### 更新代码

- 代码发生变化

  如果你的代码发生了变化，需要重启进程来更新代码。

  比如说项目的数据库更新了时区，那么进程中设计到数据库代码要进行更新，就需要重启进程。

- 子进程配置发生变化

  supervisord的子进程配置发生变化也需要重启。







