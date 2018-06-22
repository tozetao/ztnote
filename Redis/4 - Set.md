

## Set

集合（set）类型可以用于保存多个字符串元素，集合不允许有重复元素并且集合中的元素是无序的。

一个集合最多支持$2^{32}-1$个元素，集合支持增删改查，也支持交集、差集和并集操作。



### sadd

将一个或多个member元素添加到集合中，以存在于集合中的member元素将被忽略。

- 时间复杂度

  O(1)

- 语法

  > sadd key member [member...]



### srem

移除key中的一个或多个元素，不存在的元素将会被忽略，成功将返回移除元素的数量。

- 时间复杂度

  O(1)

- 语法

  > srem key member [member...]



### scard

计算元素个数。

- 时间复杂度

  O(1)，redis内部变量会记录集合元素的个数，不会进行遍历。

- 语法

  > scrad key



### sismember

判断member元素是否存在于集合中。

- 时间复杂度

  O(1)

- 语法

  > sismember key member 



### srandmember

随机从集合返回指定个数的元素

- 时间复杂度

  O(1)

- 语法

  > srandmember key [count]



### spop

从集合中随机弹出元素

- 时间复杂度

  O(1)

- spop key [count]



### smembers

返回集合set中的所有成员，不存在的key被视为空集合。

- 时间复杂度

  O(N)

- 语法

  > spop key [count]



### sinter

返回多个集合的交集

- 时间复杂度

  O(N*M)，这是最差情况，N为给定集合中基数最小的集合，M为给定集合的个数

- 语法

  > sinter key [key...]



### sunion

返回多个集合的并集。

- 时间复杂度

  O(N)，N是给定所有集合的成员数量之和

- 语法

  > sunion key [key...]



### sdiff

返回多个集合的差集

- 时间复杂度

  O(N)，N是所有给定集合的成员数量之和。

- 语法

  > sdiff key [key...]



### 内部数据结构

集合类型的内部数据结构实现有俩种：

- intset

  整数集合，当集合中的元素都是整数且元素个数小于set-maxintset-entries配置时（默认512）个，redis会选用intset作为集合的内部实现

- hashtable

  当集合类型无法满足intset集合的要求时，将会使用hashtable



### 场景

- sdd=Tagging（标签）
- spop/srandmember=Random item（生成随机数，比如抽奖）
- sadd+sinter=Social Graph（社交需求）