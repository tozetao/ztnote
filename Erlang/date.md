erlang:now()

返回值是唯一且单调递增的。



os:timestamp()

这个函数返回的是操作系统认为的时间，没有任何时间校验。对这个函数的俩次不同调用的结果并不能保证是不同的。



时区问题
