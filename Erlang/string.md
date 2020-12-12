我们知道erlang可以用列表或二进制来表示字符串，对于中文或者其他语言也是一样的。

对于任意字符集数据，erlang默认是转为unicode编码（长整数格式）的列表来存储的，比如：

```erlang
S1 = "中国". %% 输出[20013,22269]
```



也可以指定成utf8编码格式来存储来存储数据。

```erlang
S2 = <<"中国"/utf8>>.	%% 输出<<228,184,173,229,155,189>>
```



说明：unicode是码表，utf8是针对unicode的一种可变字符编码。



### unicode模块

```erlang
characters_to_list(Data) -> Result
```

将一个字符集数据转换为unicode列表。

- Data

  字符串数据，可以是utf8编码的二进制数据，也可以unicode编码的列表。

example：

```erlang
Str = "中国",
unicode:characters_to_list(Str), 	%% [20013,22269]

Str1 = <<"中国"/utf8>>,		%% 不能写成<<Str/utf8>>
unicode:characters_to_list(Str1).	%% [20013,22269]
```





```erlang
characters_to_binary(Data) -> Result
```

将字符串数据转换成utf8编码的二进制数据。





注意：普通的erlang:list_to_binary()、erlang:binary_to_list()会有编码问题。回头看下文档验证下。

