a：

![](D:\study\repositories\ztnote\problems\images\rs.1.png)

![](D:\study\repositories\ztnote\problems\images\rs2.png)

这是 FPM 下的雪花，也就是，有一个重复，帮我找找 bug 在哪里







b：

eval 不需要，直接incr就够了呀，共享内存应该可用，几年前和鸟哥有讨论过这个问题好像。这里就不是事务了，你直接把这个干掉只保留incr试一下。



a：嗯，之前忘记可以直接incr所以这么干，这是优化点后面改。偶尔会重复一个，帮我看看是为啥

b：while 部分命中要加sleep

a：之前加了usleep，但是一想，不睡似乎没啥问题，就去掉了

b：你想想，过期1000不够，因为定时器不精准，你测1000万次。你在calseq函数打个log，3个参数

key,exists=true|false，value=xxx，这样你分析重复id的log就明白了。我看着bug点是return 1的部分。应该这里你其实发生了脑裂，一部分情况值由代码控制，一部分又由redis控制，设计上来讲就是垃圾设计。



