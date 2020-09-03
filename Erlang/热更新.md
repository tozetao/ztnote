## 热更新

code模块负责erlang的热更新，code模块是Erlang Code Server提供给外面的接口，职责就是把编译好的模块加载到运行时环境中。



### 代码的加载

运行时系统有俩种启动模式，embedded和interactive，默认是interactive，这俩种模式在加载编译代码上有很大的不同。

- embedded

  该模式受应用场景的限制，模块的加载都是显式指定code server加载模块的顺序。

- interactive

  系统启动时只会加载运行时需要的模块，其他的代码模块都是第一次在使用时动态加载。当调用一个函数发现模块没有加载时，code server就会去搜索并加载它。

code server维护了一个搜索代码路径的列表，通常被称作代码路径（code path）。类似set_path(Path)、get_path()等方法都是用于管理代码路径的。

Kernel和Stdlib相关的文件夹都会首先加载，后面的模块如果出现和OTP模块重名，就会被OTP中同名的模块覆盖，换句话说kernel和stdlib中的模块是不会被覆盖的。







### 版本的切换

代码版本有俩个概念，当前版本代码current和老版本代码old。一个模块被加载时，版本状态就会变成current，再有一个版本过来加载，之前的版本就会变成old，新加载的变成current。

这时候俩个版本的代码同时存在，新的请求会使用current版本的代码，而old版本的代码还被会使用是可能还有其他模块调用它，比如函数中有一个timer:sleep导致进程在这个函数中驻留。

这时如果再一次进行更新，就会有第三个实例被加载，code server就会终止还驻留在old版本代码依赖的进程，然后第三个实例称为current，之前current版本的代码被标记为old。



值得注意的是触发热更新需要使用完全限定方式调用（fully qualified function call），例如：

```erlang
-module(m).
-export([loop/0]).

loop() ->
    receive
        code_switch ->
            m:loop();
        Msg ->
            ...
            loop()
    end.

%% 这段代码要使进程更新代码，向其发送code_switch消息。
%% 这个进程会使用完全限定方式调用m:loop()函数，并且更新为current版本的代码。


%% 注：m:loop()必须导出。
```













### 示例

```erlang
soft_purge(Module) -> boolean()
```

如果没有进程使用模块旧版本的代码，则清除该模块旧版本的代码；

如果有进程在使用该模块旧版本的代码，则模块旧版本的代码清除失败而返回false，否则返回true。



```erlang
purge(Module) -> boolean()
```

清除模块被标记为旧版本的代码。不同的是，如果有进程在使用该模块的旧代码，[code:purge/1](https://erldoc.com/mod/code/soft_purge_1.html?search=sof&i=0#) 会先杀死执行进程，再执行清理操作。



example：通过code:purge/1和code:load_file(File)俩个函数来进行热更新。

```erlang
reload(Module) ->
    code:purge(Module),
    
    case code:load_file(Module) of
        {module, Module} ->
            io:format("ok");
        _ ->
            io:format("reload error")
	end.
```







引用

https://www.cnblogs.com/me-sa/archive/2011/10/29/erlang0010.html

https://blog.csdn.net/zhangzhizhen1988/article/details/8005563



beam_bc模块。

该模块为BEAM编译器创建的文件提供一个接口。所使用的格式是"EA IFF 1985"交换格式文件标准的变体，它将数据分成快。

块数据可以作为二进制或复合项（erlang数据项）返回。当数据块是由names（原子）而不是标识符（string）引用时会返回复合项。可识别的names和相应的标识符如下：



