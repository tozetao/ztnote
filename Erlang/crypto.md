### 描述

crypto提供了加密函数的API。

这个模块的API处于比较低的底层，在public_key/3中可以找到一些相应的API，在更高的抽象层次上，应该使用crypto模块来实现。



### 依赖

crypto的实现使用nifs（Native Implement Function）来链接OpenSSLs加密库，并且在OpenSSLs 0.9.8之前的老版本中只能使用有限的功能。

FIPS模式至少需要使用1.0.1版本和支持FIPS的OpenSSLs，建议使用OpenSSL项目官方支持的版本。



### 配置

这里应该是crypto的全局配置。

不是很懂以后再看。







```erlang
hash(Type, Data) -> Digest
```

- Type

  哈希算法

- Data

  待加密的数据

- Digest

  binary，混淆后的哈希值

使用Type指定的哈希算法计算Data的哈希值。

如果所选的算法类型不被crypto库支持，则会引发一个error:notsup异常。







```erlang
format(Format) -> ok
format(Format, Data) -> ok
format(IoDevice, Format, Data) -> ok
```

将Data中的数据项按照Format写入到标准输出上。Format包含要拷贝到输出设备上的明文字符，以及用于格式化的控制序列。

控制序列的一般格式是~F.P.PadModC。

- C

  字符C决定了要使用的控制序列的类型，它是必须的字段。所有的F、P、Pad和Mod都是可选的。例如，要使用#来代替Pad，使用默认值来代替F和P的默认值，可以写成~..#c

  控制序列的类型在下面介绍。

- F

  F是打印参数的字段宽度。负值表示参数在字段中左对齐，否则右对齐。如果没有指定字段宽度，则使用所需的打印宽度。如果指定的字段宽度太小，则整个字段用*字符填充。

- P

  P是打印参数的精度。如果没有指定精度，则使用默认值。精度的解释取决于控制序列。

  对于字符串，进度是要截取的字符数。

  对于浮点型，精度是指保留的小数位

  ...

- Pad

  Pad是填充字符。该字符用于填充参数的打印表示，使其符合指定的字段宽度和精度。只能指定一个填充字符，只要适用，它就会同时用于字段宽度和精度。默认的填充字符是''（空格）。

- Mod

  Mod是控制序列修饰符。这是一个或多个改变数据解释的字符。当前的修饰符是 t，用于 Unicode 翻译，以及 l，用于停止 p 和 P 检测可打印字符。

如果F和P是*字符，则使用Data中的对应数据项作为值。example：

```erlang
%% 宽度9，精度5，不满足宽度则用0填充
io:format("~*.*.0f", [9, 5, 3.1415926]).
```

要想使用*字符来标识Pad，必须将其作为参数传递：

```erlang
io:fwrite("~*.*.*f~n",[9, 5, $*, 3.14159265]).
```





可用的控制序列有：

- B

  以2-36作为进制来格式化数据，默认为10进制数。对于负整数，会打印一个负号破折号。

  对于B类型的控制序列，在控制序列中P表示进制数。

```erlang
%% 以16进制来格式化，F是默认值，会以合适的宽度来显示数据。
io:format("~.16b", [31]).

%% 以16进制来格式化，宽度2，右对齐，不满足宽度2的补0，
io:format("~2.16.0b", [31]).
```





```erlang
Str1 = "中国".
Str2 = <<"中国"/utf8>>.
Str3 = binary_to_list(Str2).

io:format("~ts", [Str1]).	%% 输出中国
io:format("~ts", [Str2]).	%% 输出中国
io:format("~ts", [Str3]).	%% error
```

字符t是unicode翻译修饰符，它能够识别unicode列表和utf8编码的二进制数据。

如果数据是utf8编码的列表，那么它不能够识别。