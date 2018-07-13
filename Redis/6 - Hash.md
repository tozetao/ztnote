## Hash

redis的散列可以存储键值对的数据，基本等价于HashTable。



### hset

设置散列中的一个键值对

- 语法

  > hset  key-name  key  name



### hget

获取散列中指定键的值

- 语法

  > hget  key-name  key



### hmget

从散列中获取一个或多个键的值

- 语法

  > hmget  key-name  key...



### hmset

在一个散列中设置一个或多个键值对

- 语法

  > hmset  key-name  key  value  [key  value]



### hdel

在一个散列中删除一个或多个键

- 语法

  > hdel  key-name  key...



### hlen

返回一个散列包含的键值对数量。

- 语法

  > hlen  key-name



### hexists

判断一个散列中，给定的键是否存在。

- 语法

  > hexists  key-name  key



### hkeys

返回散列中键的集合。

- 语法

  > hkeys  key-name



### hvals

返回散列中值的集合。

- 语法

  > hval  key-name



### hgetall

获取散列中的所有键值对

- 语法

  > hgetall  key-name



### hincrby

对散列中指定键的值加上一个指定的整数。

- 语法

  > hincrby  key-name  key  increment



### hincrbyfloat

对散列中指定键的值加上一个指定的浮点数。

- 语法

  > hincrbyfloat  key-name  key  increment

