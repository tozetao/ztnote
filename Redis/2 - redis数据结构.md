## 字符串

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



### append

如果key存在，添加的值会附加在原有值的后面，如果key不存在，相当于set命令

- 语法

  > append key



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





## 列表

列表用于存储多个有序可重复的字符串，列表中的每个字符串可以称为元素，一个列表最多可以存储$2^{32}-1$个元素。

在redis中，列表提供了双端的push和pop操作，还能获取指定范围内的元素，指定索引的元素等。



### rpush

- 语法

  > rpush key value [value...]

- 时间复杂度

  O(1)

rpush可以将一个或多个值插入到列表的尾部(右边)。如果key不存在一个空的列表会被创建，当key存在但是非列表类型时会返回error



### lpush

向列表的头部插入一个或多个元素。



### linsert

向某个元素前或者后插入元素。

- 时间复杂度

  O(N)，N是寻找pivot过程中经过的元素数量。

- 语法

  > linsert  key  BEFORE|AFTER  pivot value

当pivot不存在列表中，不执行任何操作，返回-1；当key不存在时视为空列表，不执行任何操作。如果key不是列表类型返回一个错误。



### lrange

获取指定范围内的元素列表。

- 时间复杂度

  O(S+M），S是偏移量start，M是指定区间内元素的数量

- 语法

  > lrange key start stop

下标start和stop都是从0开始，也可以使用负数下标，-1表示列表最后一个元素，以此类推。

注1：stop下标也属于区间内的有效元素，例如有个列表有100个元素，lrange key 0 10会获取11个元素，包括索引10的元素。

注2：超出范围的下标不会引起错误，如果start的下标大于列表的元素个数，那么lrange返回空；如果stop的小标大于列表的元素个数，stop的值将设置为end。



### lindex

获取指定索引的元素。

- 时间复杂度

  O(N)

- 语法

  > lindex key index



### llen

返回列表的长度

- 时间复杂度

  O(N)

- 语法

  > llen key



### lpop

从列表的头部弹出一个元素

- 时间复杂度

  O(1)

- 语法

  > lpop key



### rpop

从列表的尾部弹出一个元素

- 时间复杂度

  O(1)

- 语法

  > rpop key



### lrem

删除指定元素。

- 时间复杂度

  O(N)

- 语法

  > lrem key count value

根据参数count的值，移除列表中与参数value相当的元素，count的值可以是以下几种：

- count>0：从表头开始搜索，移除与value相等的元素，移除个数是count值的大小
- count<0：同上，只不过是从表尾开始搜索
- count=0：移除表中所有与value相等的元素



### ltrim

按照范围修剪列表，就是说只保留区间内的元素，不在区间内的元素都会被删除。

- 时间复杂度

  O(N)，N是移除的元素个数

- 语法

  > ltrim key start stop



### lset

修改指定下标的元素。

- 时间复杂度

  O(N)

- 语法

  > lset key index value



### blpop

blpop是lpop的阻塞版本，b是blocking，锁定的意思。

这里的阻塞指的是当列表内没有任何元素弹出时，列表将被blpop命令阻塞，直到等待超时或者有可弹出元素为止。

注：多个连接使用阻塞命令才会发生阻塞现象。

- 时间复杂度

  O(1)

- 语法

  >  blpop key [key...] timeout

参数timeout如果是0，将会一直阻塞。

如果有多个key，会按照给定key的先后顺序依次检查各个列表，弹出第一个非空列表的头元素，并和弹出元素所在的列表的名字一起作为数据返给客户端。

与blpop对应的命令是brpop命令，该命令的表现结果与blpop相同。



### 内部编码

列表类型的内部数据结构有三种：

- ziplist

  压缩列表，当列表的元素个数小于list-max-ziplist-entries参数配置时（默认512），同时列表中每个元素的值都小于list-max-ziplist-value参数配置时（默认64字节），redis将会使用该数据结构

- linkedlist

  链表

- quicklist

  快速列表，3.2以上的版本才支持。

  



list数据类型可以用于模拟栈、队列、消息队列和有限集合等。

