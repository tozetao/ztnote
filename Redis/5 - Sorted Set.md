## Sorted Set

有序集合是给每个成员设置一个分数（score）作为排序依据的集合，它维持着成员与分数之间的映射，有序集合不允许有重复成员；

有序集合提供的功能有：

- 基本的插入、查找与删除

- 提供指定分数或元素范围内的查询和排序
- 多个集合的交集、并集操作

注：在有序集合中，排名是从0开始计算的。



### zadd

- 时间复杂度

  O(logN)，N是有序集合的元素个数

- 语法

  > zadd key \[NX|XX\]  \[CH\]  [INCR]  score member [score member]

  NX：member必须不存在，才可以设置成功

  XX：member必须存在，才可以设置更改，用于更新

  CH：返回此次操作后，有序集合元素和分数发生变化的个数

  INCR：对score增加

将一个或多个member元素及其score值加入到有序集合中。

如果member元素已经是有序集合的成员，那么更新这个member的score值，并通过重新插入这个member元素来保证该元素在正确的位置上。

注：score可以是整数或双精度浮点数。



### zrem

删除成员

- 时间复杂度

  O(logN)

- 语法

  > zrem key member [member...]



### zcard

计算成员个数。

- 时间复杂度

  O(1)

- 语法

  > zcard key



### zscore

返回某个成员的分数。

- 时间复杂度

  O(1)

- 语法

  > zscore key member



### zrank

计算成员的排名。

返回有序集合中成员member的排名，其中有序集合成员按score值递增（从小到大）的顺序排列。

- 时间复杂度

  O(logN)

- 语法

  > zrank key member



### zrevrank

计算成员的排名。

返回有序集合中成员member的排名，其中有序集合成员按score值递减，从大到小的顺序排列。

- 时间复杂度

  O(logN)

- 语法

  > zrevrank key member



### zrange

返回指定排名范围内的成员。

- 时间复杂度

  O(logN+M)，N是有序集合的基数，M是结果集的基数

- 语法

  > zrange key start stop [withscores]
  >
  > withscores：让成员与它的socre值一起返回

返回有序集key中指定区间的成员，其中成员的位置按score值递增（从小到大）来排序，具有相同score值的成员按字典序来排列。

与zrange相反命令是zrevrange。



### zrangebyscore

返回指定分数范围内的成员，zrangebyscore的相反命令是zrevrangebyscore。

- 时间复杂度

  O(logN+M)

- 语法

  > zrangebyscore key min max \[withscores\]  [limit offset count]
  >
  > withscores：该选项会将有序集成员和score值一起返回

zrangebyscore可以返回指定score范围内的成员，包括min和max在内分数的成员。返回的成员会按照score值递增（从小到大排列），具有相同score值的成员按字典序排列。



闭区间与开区间

默认redis是闭区间的，区间的取值使用小于等于或大于等于，但是可以给min、max参数加上（符号或）符号，来表示开区间，即小于或大于。



### zincrby

增加成员的分数

- 时间复杂度

  O(logN)

- 语法

  > zincrby  key incrementmember

  为有序集合key的成员member加上增量increment



### zcount

返回指定分数范围内的成员个数

- 时间复杂度

  O(logN)，N为有序集的基数

- 语法

  > zcount key min max



### zremrangebyrank

删除指定排名内的升序元素

- 时间复杂度

  O(logN+M)，N是有序集的基数，M为移除的成员数量

- 语法

  > zremrangebyrank key start stop



### zremrangebyscore

删除指定分数内的成员

- 时间复杂度

  O(logN+M)，N是有序集的基础，M为移除的成员数量

- 语法

  > zremrangebyscore key min max



### zunionstore

计算给定的一个或多个有序集的并集，注意是并集而不是交集。

- 时间复杂度

  O(N)+O(M * logM)，N是给定有序集合基数的总和，M为结果集的基数

- 语法

  > zunionstore  destination  numkeys  key  \[keys...]  \[weights  weight...]  \[aggregate sum|min|mix]
  >
  > destination：存储并集结果
  >
  > numkeys：指定key的数量
  >
  > weights：为每个给定的有序集分别指定一个乘法因子，每个给定有序集的成员的score值在传递给聚合函数之前都会乘以该乘法因子
  >
  > arrregate：指定结果集的聚合方式，默认sum

在计算并集时是根据member元素的名字来进行判断，由于有序列表是有分数值（score），因此在合并过程中也需要对score进行处理，aggregate/weights选项负责处理score。

aggregate选项指定结果集分数的聚合方式，sum可以将所有集合中某个member的score之和作为结果集中该成员score的值；min会将所有集合中最小member的score的值作为结果集中该成员的值；max也是同样效果。

每个有序集合的成员在传递给聚合函数处理之前，可以通过weights选项改变每个成员的数值。



### zinterstore

计算给定的一个或多个有序集的交集，其中给定key的数量必须以numkeys参数指定，并将结果集存储到destination.

- 时间复杂度

  待定

- 语法

  > zinterstore  destination  numkeys  key  \[key...]  \[weights weight...]  \[aggregate sum|min|max]



### 内部编码

有序集合的内部数据结构有俩种：

- ziplist

  压缩列表，当有序集合的元素个数小于zset-max-ziplistentries配置（默认128个），且值小于zset-max-ziplist-value配置时（默认64字节），redis会用ziplist来作为有序集合的内部实现，ziplist可以有效的减少内存的使用

- skiplist

  跳跃表，当 ziplist 条件不满足时，有序集合会使用 skiplist 作为内部实现，因为此时ziplist的读写效率会下降。 



### 场景

- 点赞
- 积分系统
- 分页
- 排序