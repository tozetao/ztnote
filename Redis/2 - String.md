## String

命令的操作可以分为单次操作和批量操作，批量操作的优先是节省网络时间，它的操作成本=1次网络时间+n次操作。



### set

用于设置一个键的值，如果键不存在则添加。

- 语法

  > set key value \[EX seconds] \[PX value]  [NX|XX]

  NX：键必须不存在才能设置成功，可用于分布式事物锁

  XX：键必须存在才能设置成功

redis还提供了set对应选项的命令，分别是：

- setnx

  相当于set NX，key必须不存在才允许设置

- setex

  相当于set EX，设置一个key时并设置它的生存时间

- psetex



### get

返回一个key的值，如果不存在返回nil

- 时间复杂度

  O(1)



### mset

批量设置多个键值对，mset是原子性操作，所以给定的key都是立即设置的，客户端不可能看到某些key更新而某些key未更新。

- 语法

  > mset key value [key value...]

- 时间复杂度

  O(N)，N是要设置key的数量



### mget

返回多个key的值，如果某些key不存在将返回nil。

- 时间复杂度

  O(N)，N是key的数量



### incr

incr是原子性操作，能够对key的值进行自增，结果有3种：

- 值不是整数返回错误
- 值是整数返回自增结果
- key不存在将会按照值为0进行自增，结果返回1

  incrby命令指定增加值的大小。



### decr

decr是原子性操作，能够对key 的值进行自减，结果又3种：

- 值不是整数返回错误
- 值是整数返回自减结果
- key不存在会当做0处理进行自减，结果返回-1

decrby命令可以指定减少值得大小



### append

如果key存在，添加的值会附加在原有值的后面，如果key不存在，相当于set命令

- 语法

  > append key



###setrange

将从start偏移量开始的子串设置为给定值。如果当前字符串的长度不能满足写入长度的要求，redis会使用null字符进行扩充，然后再执行写入或更新操作。

- 语法

  > setrange  key  offset  value

- example

  > add  content  'hello'
  >
  > setrange  content  10  "is my demo"



### getrange

处理字符串子串的命令，获取一个由偏移量start至偏移量end范围内的子字符串，在使用getrange命令读取字符串的时候，超出字符串长度的数据会默认为null字符。



### del

删除给定的一个或多个key。

- 语法

  > del key [key...]

- 时间复杂度

  O(N)，N是key的数量。

  如果删除的是字符串类型的key，时间复杂度O(1），如果删除的是集合或列表，时间复杂度是O(M)



### 内部数据结构

redis中字符串的内部数据结构有3种：

- int

  8个字节的长整数

- embstr

  小于等于39个字符的字符串

- raw

  大于39个字符的字符串

redis会根据值的类型和长度决定使用哪种数据结构。

```
set intkey 390
object encoding intkey	 //int
type intkey 			//string
```



