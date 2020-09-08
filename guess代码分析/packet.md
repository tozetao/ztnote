## packet.erl

该模块包含WebSocket协议的封包和解包，以及前后端自定义数据包协议的封包和拆包



## WebSocket

### packet/3

该函数会将数据打包成符合WebSocket协议的数据包。



### unpacket/2

该函数用于对websocket数据帧进行解包。

```erlang
%% Len表示数据的字节长度，Bin是由4个字节的MaskKey和Len个字节的Data组成。
unpacket(Bin, Len) ->
    <<Mask:4/binary, Payload:Len/binary>> = Bin,
    unpacket(Payload, Mask, <<>>).

unpacket(Payload, <<Ma:8, Mb:8, Mc:8, Md:8>> = Mask, Acc) ->
    case size(Payload) of
        0 ->
            Acc;
        1 ->
            <<A:8>> = Payload,
            <<Acc/binary, (Ma bxor A)>>;
        2 ->
            <<A:8, B:8>> = Payload,
            <<Acc/binary, (Ma bxor A), (Mb bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = Payload,
            <<Acc/binary, (Ma bxor A), (Mb bxor B), (Mc bxor C)>>;
        _Other ->
            <<A:8, B:8, C:8, D:8, Data/binary>> = Payload,
            Acc1 = <<Acc/binary, (Ma bxor A), (Mb bxor B), (Mc bxor C), (Md bxor D)>>,
            unpack(Data, Mask, Acc1)
    end.
```

unpacket/2主要解析WebSocket数据帧的包体内容。

- Bin

  二进制数据，由4个字节MaskKey和Len个字节的数据组成

- Len

  传递的包体长度



## 数据包格式

这里的数据包指的是前后端传递的业务数据，格式是前后端协商自定义的。

请求数据的格式如下：

```erlang
<<DataSize:16, DataStatus:8, Flag:32, Cmd:16, NewBin/binary>>
```

- DataSize

  表示NewBin数据的字节长度，即数据大小

- DataStatus

  数据状态，目前有俩种状态，未处理（即原始二进制数据），压缩处理（压缩过的数据）

- Flag

  未知

- Cmd

  协议号

- NewBin

  protobuff编码过的数据

响应数据的格式如下：

```erlang
<<ErrorID:16, DataSize:16, DataStatus:8, Flag:32, Cmd:16, NewBin/binary>>
```

响应的数据格式只是多了一个16位的ErrorID，其他与请求数据格式一样。



## protobuff

所有的protobuff结构体的名字都是以下形式组成：

请求结构体：定义前端要传递的数据，由m\_协议号\_tos组成。

响应结构体：定义后端要响应的数据，由m\_协议号\_toc组成。



### unpack/3

使用protobuff解包

```erlang
unpack(Cmd, DataStatus, Bin) ->
    %% 预处理数据，例如解压缩或解码
    NewBin = handl_data_in(DataStatus, Bin),
    
    %% 拼接Cmd对应的protobuff结构体，生成请求结构体的名称
    String = "decode_m_" ++ erlang:integer_to_list(Cmd) ++ "_tos",
    
    %% 将String转换成原子
    Fun = list_to_atom1(String),
    
    %% 对二进制数据解码
    Data = all_pb:Fun(NewBin),
    {ok, Cmd, Data}.
```

使用protobuff对客户端传递的数据进行解包。

- Cmd

  决定要调用的模块和函数，Cmd除以100取整得到的结果是决定要调用的模块，Cmd自身决定要调用的函数（或者说协议）。

- DataStatus

  数据状态，数据可能被压缩或者编码过，该字段标识数据被如何处理过。

- Bin

  用protobuff编码过的数据。

这个函数简单的说，就是通过Cmd得到协议对应的protobuff文件，然后使用该protobuff文件来对数据解码。



以下是内部函数：

```erlang
%% 根据Status对数据进行处理，可能解压缩或解码
handl_data_in(Status, Data) ->
    case Status of
        ?RAW_DATA ->
            Bin;
        %% 解压数据
        ?COMPRESSION_DATA ->
            uncompress_data(Data);
        ?ENCRYPTION_DATA ->
			encrypt_data(Data)
	end.

compress_data(Data) ->
    zlib_compress(Data).

encrypt_data(Data) ->
    %% 未实现
    Data.

%% 将字符串转换成原子
list_to_atom1(List) when is_list(List) ->
    case catch(list_to_existing_atom(List)) of
        {'EXIT', _} ->
            erlang:list_to_atom(List);
        Atom ->
            Atom
    end.
```



### pack/3

pack/3函数会根据协议号，使用对应的protobuff结构体进行打包，并返回前后端指定格式的数据包。

```erlang
pack(Cmd, Data, Flag) ->
    case Data of 
        null ->
            pack_empty(Cmd, Flag);
        _ ->
            pack_proto(Cmd, 0, Data, Flag)
    end.

%% 空数据时的打包
pack_empty(Cmd, Flag) ->
    BB = <<0:16, 0:16, 0:8, Flag:32, Cmd:16>>,
    {ok ,BB}.

%% 使用proto对数据进行打包
pack_proto(Cmd, DataStatus, Data, Flag) ->
    %% 根据Cmd协议号，组成proto编码的函数名字
    String = "encode_m_" ++ erlang:integer_to_list(Cmd) ++ "to_c",
    
    %% 将String转换成原子，即要调用的方法名
    Fun = list_to_atom1(String),
    
    %% 进行打包
    PbList = all_pb:Fun(Data),
    
    NewBin = handl_data_out(DataStatus, Bin),
    NewBin = Bin,
    ErrorID = ?NO_ERR_ID,
    
    DataSize = erlang:byte_size(NewBin),
    BB = <<ErrorID:16, DataSize:16, DataStatus:8, Flag:32, Cmd:16, NewBin/binary>>,
    {ok, BB}.
```



内部函数

```erlang
handl_data_out(DataStatus, Data) ->
	case DataStatus of
		?RAW_DATA ->
			Data;
		?COMPRESSION_DATA->
			compress_data(Data);
		?ENCRYPTION_DATA ->
			encrypt_data(Data)
	end.

%% 压缩数据
compress_data(Data) ->
	zlib:compress(Data).
```

