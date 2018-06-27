bitmaps是一个以位为操作单元的数组，数组中的值非0即1，数组的下标被称为偏移量。key所存储的值是字符串，只不过可以对该字符串进行位操作。



bitmaps的bit位是从0开始计数的。

bitmaps的bit位是从左到右排列设置的。

```
setbit bits 16 1

bitpos bits 1 0 2
#输出16

#bitpos命令的索引是按照字节数大小来当做索引的，0和2表示在3个字节大小中寻找，所以从第0索引字节到第2索引字节中寻找到16，证明了上面俩个观点
```





### setbit

- 时间复杂度

  O(1)

- 语法

  > setbit  key offset  value
  >
  > offset：偏移量，即要操作的位，从右往左计算偏移量的位
  >
  > value：该位上的值，0或者1

对key所存储的字符串值，设置或清除偏移量上的位bit，返回值是偏移量上原来存储的位的值。

key的字符串值是会自动伸展（grow）的以确保可以操作指定偏移量上的位，当字符串进行伸展时，空白位置以0补充。

offset参数必须大于等于0，小于2^32，bit限制映射在512m以内。

```
setbit test-bit 618 1
#生成619个位，因为是从0开始的，返回值0

set test-bit 618 0
#返回1
```



### getbit

对key所存储的字符串值，获取指定偏移量上位的值。

当 `offset` 比字符串值的长度大，或者 `key` 不存在时，返回 `0` 。 

- 时间复杂度

  O(1)

- 语法

  > getbit  key  offset



### bitcount

- 时间复杂度

  O(N)

- 语法

  > bitount   key  start  end

计算给定key的字符串中，被设置为1的bit位的数量。



### bitop

- 时间复杂度

  O(N)

- 语法

  > bitop  operation  destkey  key  [key...]

对一个或多个保存二进制位的字符串key进行位运算，并将结果保存到destkey上。

位运算有：

- bitop  and  destkey  key  [key...]

  逻辑与位运算

- bitop  or  destkey  key  [key...]

  逻辑或

- bitop  xor  destkey  key  [key...]

  逻辑异或

- bitop  not  destkey  key 

  逻辑非，逻辑非是取反，所以只针对一个key的值进行操作

在处理不同长度的字符串时，较短的那个字符串所缺少的部分全部当做0来进行运算，空的key也会看做是包含0的字符串序列。



注：bitop的复杂度是O(N)，当处理大型矩阵或进行大数据量的统计时，最好将任务分发到子节点进行处理，避免阻塞主节点。



### bitpos

计算bitmaps中第一个值为bit的偏移量

- 复杂度

  O(N)

- 语法

  > bitpos  key  bit  \[start]  [end]
  >
  > bit：要搜索的值，非0即1
  >
  > start|end：搜索的起始位置和结束位置，单位是字节

该命令会返回字符串中第一个设置为1或0的bit位。

注：返回的值是从0开始的， 即使使用start参数指定开始的位也是一样的。



如果在空字符串或0字节的字符串中查询bit为1的位，结果将返回-1；如果在bit位全为1的字符串中查找bit为0的位，将返回最右边的第一个空位。

例如在一个字节0xff的字符串中，寻找bit为0的位将返回8，因为0-7位都是1，可以将字符串看成是右边有无数个0来处理。



### bitfield

- 时间复杂度

  每个子命令的复杂度为O(1)

- 语法

  > bitfield  key  \[get  type offset]  \[set  type  offset  value]  \[incrby  type  offset  increment]  \[overflow  wrap | sat | fail]

bitfield命令可以在一次调用中同时对多个位范围进行操作，它接受一系列待执行的操作作为参数，并返回一个数组作为回复，数组中的每个元素就是对应操作的执行结果。



注：bitfield命令最大支持64位的有符号整数和63位长的无符号整数。



bitfield支持的子命令有：

- get <type> <offset>
- set <type>  <offset>
- incrby <type> <offset> <increment>



```c
bitfield bits set u4 10 15
#在偏移量10的bit位上，设置一个无符号4位长度的整数

bitfield bits get u4 10
#输出15

bitfield bits get u5 10
#输出30
#15的二进制：1111，获取5位无符号整数将得到11110，11110等于10进制的30

bitfield bits get u6 10
#输出60

bitfield bits get u3 10
#输出7
#在偏移量10上获取3位无符号整数，得到111，等于10进制的7
```



位图的bit位是从0开始作为索引的，所以偏移量和整数长度与字节边界对其时，位图表示二进制的方式与大端表达法一致，但是在会有没有对其的情况。

例如有个32位的位图，所有bit位预设为0，在偏移位7上设置一个5位无符号整数23（二进制10111），在位图中是存储为：00000001 | 01110000





作用

能够将很多小的整数存储在一个长度较大的位图中，或者将一个非常庞大的键分割为多个较小的键来进行存储，从而非常高效的使用内存。







位图相关

http://blog.getspool.com/2011/11/29/fast-easy-realtime-metrics-using-redis-bitmaps/

https://www.zhihu.com/question/268937734/answer/344013478

https://www.zhihu.com/question/51583361/answer/152084476

https://blog.csdn.net/wenqiang1208/article/details/76724338



只要数据是整型类型的，且id增长的，都可以考虑redis的bitmaps来处理

位图算法

https://blog.csdn.net/wenqiang1208/article/details/76724338

https://www.cnblogs.com/protected/p/6626447.html

https://www.cnblogs.com/biyeymyhjob/archive/2012/08/14/2636933.html

https://www.cnblogs.com/yjf512/archive/2010/11/04/1868899.html

锁

http://www.cnblogs.com/yjf512/p/6597814.html