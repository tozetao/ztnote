## Geo

geospatial的意思，代表地理空间。



### geoadd

将给定的空间元素添加到键中，这些键会以有序集合的形式被存储到键里面，从而使得像georadius和georadiusbymember这样的命令在之后可以通过位置来进行查询。

- 时间复杂度

  O(logN)，N是键包含的元素个数

- 语法

  > geoadd  key  longitude  latitude  member  \[longitude  latitude  member...]
  >
  > key：键的名字
  >
  > longitude：经度
  >
  > latitude：维度
  >
  > member：成员

- 返回值

  返回新添加到键里面的空间元素个数，已添加的键元素将被忽略计数



example：

> http://api.map.baidu.com/lbsapi/getpoint/index.html    #一个获取经纬度的地址
>
> ```
> GEOADD tour 116.404412 39.915046 TianAnMen 116.20003 40.002428 XiangShan
> # 输出(integer) 2
> 
> GEOADD tour 116.362444 39.977552 BeijingFilmAcademy 116.404269 39.909179 ChairmanMaoZedongMemorialHall 116.404412 39.915046 TianAnMen
> # 输出(integer) 2，已有的元素不计数
> ```



### geopos

从键里面返回给定元素的位置（经纬度），因为getpos命令接收可变数量的元素作为输入，所以返回的结果是数组。

- 时间复杂度

  O(logN)，N是键里面元素的数量

- 语法

  > geopos  key member [member...]

- 返回值

  getpos命令返回一个数组，数组中每一项都是由俩个元素组成：经度和纬度；如果给定的元素不存在时，对应的数组项为空。



### geodist

获取俩个地理元素之间的距离。

- 时间复杂度

  O(logN)

- 语法

  > geodist  key  member1  member2  unit
  >
  > unit：指定单位，可以是m、km、mi、ft

- 返回值

  返回来个元素之间的距离，如果某个元素不存在，将返回null

geodist在计算时会假设地球为完美的球星，在极限情况下这一假设会造成0.5%的误差。



### georadius

获取指定位置范围内的地理信息位置的集合。

- 时间复杂度

  O(N + logM)，N是指定半径范围内的位置元素的数量，而M是返回位置元素的数量

- 语法

  > georadius  key  longitude  latitude  radious  m|km|ft|mi  \[withcoord]  \[withdist]  \[withhash]  \[asc|desc]  \[COUNT  count]  \[STORE  key]  \[STOREDIST  key]

georadious命令会以给定的经纬度为中心，返回与中心的距离不超过给定最大距离的所有地理位置元素。

选项说明：

- withdist

  返回位置元素的同时，将位置元素与中心之间的距离也一并返回，返回的距离和用户给定的范围单位保持一致。

- withcoord

  将位置元素的经纬度也返回

- withhash

  以 52 位有符号整数的形式， 返回位置元素经过原始 geohash 编码的有序集合分值。 这个选项主要用于底层应用或者调试， 实际中的作用并不大。 



命令默认返回未排序的元素，可以指定排序方式：

- asc

  根据中心的位置，按照从近到远返回位置元素

- desc

  根据中心的位置，按照从远到进返回位置元素



在默认情况下，georadius会返回所有匹配的位置元素，虽然可以通过<COUNT  count>获取前N个匹配元素，但是因为命令在内部可能会需要对所有匹配的元素进行处理，所以在对一个非常大的区域进行搜索时，即使使用count选项获取少量元素，命令的执行速度也会变慢。



- STORE key

  将返回结果的地址位置信息保存到指定的键

- STOREDIST  key

  将返回结果离中心节点的距离保存到指定键 



georadius会返回一个数组，如果没有使用任何参数选项，将返回一个匹配范围内的位置元素的线性列表；如果有使用参数选项的话，将返回一个二维嵌套数组，内层的每个子数组就表示一个元素。

example：

> georadius  tour  116.385871  39.871977  20  km  withcoord  withdist