## List

列表（list）用于存储多个可重复的字符串，列表中的每个字符串可以称为元素，一个列表最多可以存储$2^{32}-1$个元素。

在redis中，列表提供了双端的push和pop操作，还能获取指定范围内的元素，指定索引的元素等；通过列表可以模拟栈和队列结构来操作数据。



### rpush

- 语法

  > rpush key value [value...]

- 时间复杂度

  O(1)

rpush可以将一个或多个值插入到列表的尾部(右边)。如果key不存在一个空的列表会被创建，当key存在但是非列表类型时会返回error



### lpush

向列表的头部插入一个或多个元素。



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



### ltrim

按照范围修剪列表，就是说只保留区间内的元素，不在区间内的元素都会被删除。

- 时间复杂度

  O(N)，N是移除的元素个数

- 语法

  > ltrim key start stop





### llen

返回列表的长度

- 时间复杂度

  O(N)

- 语法

  > llen key



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



### lset

修改指定下标的元素。

- 时间复杂度

  O(N)

- 语法

  > lset key index value



## Block Command

### blpop

blpop是lpop的阻塞版本，b是blocking，锁定的意思。

这里的阻塞指的是当列表内没有任何元素弹出时，列表将被blpop命令阻塞，直到等待超时或者有可弹出元素为止。

注：多个连接使用阻塞命令才会发生阻塞现象。

- 时间复杂度

  O(1)

- 语法

  > blpop key [key...] timeout
  >
  > timeout：超时时间，如果是0将会一直阻塞

如果有多个key，会按照给定key的先后顺序依次检查各个列表，弹出第一个非空列表的头元素，并和弹出元素所在的列表的名字一起作为数据返给客户端。



brpop与blpop具有相同的效果，会在timeout时间内阻塞并等待可弹出的元素，不同的是它会从非空列表的右端弹出元素。



### rpoplpush

从源列表中弹出最右端的元素，同时将弹出的元素插入到目标列表的最左端中。

- 语法

  > rpoplush  source-key  dest-key  



### brpoplpush

从源列表中弹出最右端的元素，同时将弹出的元素插入到目标列表的最左端中，如果源列表为空，它会在timeout时间内阻塞直到有可弹出的元素

- 语法

  > brpoplpush  source-key  dest-key  timeout
  >
  > timeout：过期时间



对于blpop和brpoplpush命令，最常用在消息传递和任务队列中。



### 内部编码

列表类型的内部数据结构有三种：

- ziplist

  压缩列表，当列表的元素个数小于list-max-ziplist-entries参数配置时（默认512），同时列表中每个元素的值都小于list-max-ziplist-value参数配置时（默认64字节），redis将会使用该数据结构

- linkedlist

  双端链表

- quicklist

  快速列表，3.2以上的版本才支持。

  

list数据类型可以用于模拟栈、队列、消息队列和有限集合等。





