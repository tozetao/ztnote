base24编码

base24能够完美的表示59位二进制编码的内容。



假设生成的激活码长度为13位字符，字符只能是ABCDEFGHJKLMNPQRSTUVWXYZ这24个字符，那么所能够生成的上线为24^13 - 1，约等于log2(24^13) = 59.6，也就是说13位的字符能够完美表示59位二进制编码的内容。

注解：这里的意思是说通过base24编码，可以将59位的二进制映射成13个字符串？



而要生成4亿个激活码需要29个bit，log2(4亿)=28.6，接近29个bit，使用int存储占32bits，远小于59个bit，因此可以被13位的base24编码。







在此规划下bit的定义：

| 6bits标记位 | 48bits数据位 | 5bits校验位 |

- 标记位

  写入用于表示的兑换物、失效时间等一些离线简单验证的信息。

- 数据位

  这个激活码的基础数据部分

- 校验位

  对前面54bits的校验，可以是简单的hash然后再对2^5求余的余数。

因为我们是用int 32bits来表示激活码数字，那么有什么方便的加密解密方式，将32bit存储的激活码编号转换成48位的数据位呢?

在这里使用RC4加密算法，通过将激活码数字的原始32bits编码 + 6bits标记位内容 + 填充0扩充到48位bits，随机生成一个密钥，再通过RC4加密，得到48bits的密文，这个密文就是我们要写入数据为的内容。

有了密文后就能计算出校验位，最后就能够生成59bits的兑换码，再通过base24编码就能够得到13位长度的兑换码。





解密过程：客户端先验证校验位是否正确，正确再请求服务器。服务器取出数据位，解密得出激活码编号和兑换物内容，验证该激活码编号是否使用就可以了。

激活码编号是一个纯32位无重复的整数，用来奖励主键是非常高效方便的。





了解base24编码

了解RC4加密算法

https://www.zhihu.com/question/29865340







### base64原理

base64表示用64个可显示字符表示所有ASCII字符，64个标识符用6个bits即可表示。而ASCII字符有256个，要用8个bits才能够表示。

那么6和8的最小公倍数是24，24/6=4，24/8=3，也就是说同样的一段bits，需要用4个base64才能表示3个ASCII的字符。

在计算机系统中字节是存储数据的最小单位，所以才有以下base64的编码规则：

1. 将待转换的字符串每三个字符分为一组，每个字节占8bit，一共24个bit。
2. 将上面24个bit分为4组，每组6个bit。
3. 在每组前面填充俩个0，这样每组6个bit就变成8个bit，总共32个二进制位，即4个字节。
4. 由于只是填充0，每组的数据大小不变，根据每组值的大小去获取base64对应的字符，最后的内容即使编码后的内容了。



### base24

base24是使用24个字符来表示ASCII。

ASCII码用8个bits表示，但是base24的24个字符如果用4个bits表示会有8个字符未使用到，而用5个bits表示又会多出8个未知的字符。

所以这里折中一下，用4个bits来分段，第一段bits用16个字符表示，第二段bits用剩下的8个字符表示，也就是说使用2个字符来对应ASCII的1个字符。



编码规则：

假设设置一个ASCII字符的2进制表示：

> | 4bits | 4bits |

1. 将ASCII值右移4位，得到高位的4个bits。这4个bits的值可以直接获取对应base24的字符。
2. 接着再通过&运算符，得到低位的4个bits。
3. 通过23 -（低位bits的值）计算出使用剩下8个字符中的哪个字符。

最终就通过base24的2个字符来表示ASCII的1个字符。



首先会有一个编码后的字符串，我们要做的是解码。

解码规则：























请记住，一个nonce不能在一个特定的键上使用超过一次。

Context对象维护算法的当前状态，以便在流场景中使用。因此一个同时执行加密和解密的应用需要俩个上下文（Context）。

decrypt方法是encrypt方法的别名，这是为了让人容易理解。

通过调用setCounter方法，可以对密钥流进行寻找操作，其中计数是以64字节为单位的块。