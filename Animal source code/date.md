### GMT时间



### UTC时间

世界时间，



### 时区



erlang的默认时区是是二号那么？



### UNIX时间戳

UNIX时间或称POSIX时间是UNIX或类UNIX系统使用的时间表示方，从UTC时1970年1月1日0时0分0秒起至现在的总秒数.





### 函数

> erlang:now().

返回元组{MegaSecs，Secs，MicroSecs}，这是自1970年1月1日00:00 GMT（零小时）以来经过的时间（如果底层操作系统提供的话），否则会选择其他时间点。

该函数可以保证对该BIF的以下调用返回连续递增的值，因此，erlang:now/0的返回值可以用来生成唯一的时间戳。如果在快速机器上的紧缩循环中调用它，节点的时间可能会变得偏斜。

只有在底层操作系统的时区信息配置正确的情况下，它才能用来检查本地的时间。

- MegaSecs：兆秒

- Secs：秒

- MicroSecs：微秒

注：该函数在新版中已经弃用。



> erlang:timestamp()

返回当前Erlang系统时间，格式为{MegaSecs, Secs, MicroSecs}。这个格式与os:timestamp/0和已被废弃的erlang:now/0使用的格式相同。

- MegaSecs：兆秒

- Secs：秒

- MicroSecs：微秒

获取总毫秒数：1 000 000 000 * MegaSecs + Secs * 1000 + MicroSecs div 1000.

1642969105953279



erlang:timestamp()存在的原因纯粹是为了简化现有的采用这种时间戳格式的代码的使用。当前的Erlang系统时间可以通过使用erlang:system_time/1更有效地以你选择的时间单位来检索。



erlang:timestamp()的BIF相当于：

```erlang
timestamp() ->
    ErlangSystemTime = erlang:system_time(microsecond),
    MegaSecs = ErlangSystemTime div 1000_000_000_000,
    Secs = ErlangSystemTime div 1000_000 - MegaSecs*1000_000,
    MicroSecs = ErlangSystemTime rem 1000_000,
    {MegaSecs, Secs, MicroSecs}.
```

不过，它使用的是原生实现，不在堆上建立垃圾，性能稍好。



注：这个时间不是一般情况下的单调增加的时间。更多信息，请参见《用户指南》中关于时间纠正模式的文档。

note：This time is not a monotonically increasing time in the general case. For more information, see the documentation of time warp modes in the User's Guide.





> erlang:system().

以本地时间单位返回当前的Erlang系统时间。

调用 erlang:system_time() 等价于 erlang:monotonic_time() + erlang:time_offset() 。





> erlang:monotonic_time().

返回当前Erlang单调时间，以本地时间为单位。这是一个从某个未指定的时间点开始的单调增长的时间。

这是一个单调递增的时间，但不是一个严格的单调递增时间。也就是说，连续调用erlang:monotonic_time/0可以产生相同的结果。

不同的运行时系统实例会使用不同的未指定的时间点作为其Erlang单调时钟的基础。也就是说，比较不同运行时系统实例的单调时间是没有意义的。不同的运行时系统实例也可以将这个未指定的时间点放在不同的相对运行时系统开始。它可以放在未来（开始时的时间为负值）、过去（开始时的时间为正值）或运行时系统开始时（开始时的时间为零）。运行时系统启动时的单调时间可以通过调用erlang:system_info(start_time)来检索。









### examples

erlang:timestamp()转日期（本地时间）

```erlang
{{Year, Monthe, Day}, {Hour, Min, Second}} =  calendar:now_to_local_time(erlang:timestamp()).
{{Year, Monthe, Day}, {Hour, Min, Second}} = calendar:local_time().
```



erlang:timestamp()转日期（世界时间）

```erlang
calendar:now_to_universal_time(erlang:now()).
```



获取时间戳

```erlang
{MegaSecs, Seconds, _} = erlang:timestamp().
MegaSecs * 1 000 000 + Seconds.	%% 当前时间戳
```









时间戳转日期

日期转时间戳





计算星期几

```erlang
calendar:day_of_the_week(2020, 11, 4);
```



计算月份的天数

```erlang
calendar:day_of_the_month(2020, 1, 1).
```



秒转时间

```erlang
calendar:seconds_to_daystime(Seconds) -> {Days, Time}
```

将指定的秒数转换为日、时、分、秒。时间总是非负数，但如果参数Seconds是赋树，则Days是负数。

- Seconds：秒数

- Days：天数
- Time：时间，它是一个{Hour, Minute, Second}的元组。



时间转秒

```erlang
calendear:time_to_seconds(Time) -> secs_per_day()
```

- Time：时间，{Hour, Minute, Second}的元组
- secs_per_day()：0到86400，因为该函数只能计算1天内的时间











```erlang
unixtime(ms) ->
    {S1, S2, S3} = erlang:timestamp(),
    trunc(S1 * 1000000000 + S2 * 1000 + S3 / 1000);
unixtime(zero) ->
    {M, S, MS} = erlang:timestamp(),
    
    %% 当前时间转本地日期时间，Time是{Hour, Minute, Second}的元组
    {_, Time} = calendar:now_to_local_time({M, S, MS}),
    
    %% 将当前时间戳 - 一天的秒数 = 当天0点时间戳
    M * 1000000 + S - calendar:time_to_seconds(Time);
unixtime({zero, Ts}) ->
    %% 当天0点时间戳
    Base = unixtime(zero),
    
    case Ts > Base of
        %% Base - Ts = 多出来的时间，Ts并不是一个0点时间戳，所以在除以86400的时候，如果有小数则表示不满足一天，因为要向上取整多减去一天。
        false -> Base - util:ceil((Base - Ts) / 86400) * 86400;
        
        %% (Ts - Base) div 86400 = 俩个时间戳相差的天数。注：div会舍去小输
        %% 算出当天0点时间戳与Ts相隔的天数，然后作为秒数+Base
        true -> (Ts - Base) div 86400 * 86400 + Base
    end;
unixtime({next_day, Ts}) ->
    unixtime({zero, Ts}) + 86400;
unixtime({next_time, DayTs}) ->
    Now = unixtime(),
    NextT = next_diff(DayTs),
    Now + NextT.
```





https://www.cnblogs.com/me-sa/archive/2012/05/17/erlang-calendar-date-time.html

https://blog.csdn.net/mycwq/article/details/15813753?utm_source=blogxgwz2