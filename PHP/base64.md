## base64简介
base64是用于传输8bit字节代码的一种编码方式，要求把每三个8bit的字节转换为四个6bit的字节（3*8 = 4*6 = 24），然后把6bit再添两位高位0，组成四个8bit的字节，也就是说，转换后的字符串理论上将要比原来的长1/3。

编码后的数据格式是大小写字母、数字、=号、+号和/号，所以有大小写字母并且有1个或2个等号结尾的字符串，大体是base64编码后的数据。

因为base64编码后的特殊字符在URL中有特殊含义，+表示空格、/是路径划分符号、=是参数对应的值，所以base64编码后的数据不适合放在URL中传输。

URL传输解决方案：
```php
public function unsafe_base64_encode($str)
{
	$str = base64_encode($str);
	$str = str_replace(['+', '/', '='], ['-', '_', ''], $str);
	return $str;
}

public function unsafe_base64_decode($str)
{
	$str = str_replace(['-', '_'], ['+', '/'], $str);
	$mod4 = strlen($str) % 4;
	echo 'mod4: ' . $mod4, '<br/>';
	if($mod4){
		$str .= substr('====', $mod4);
	}
	return base64_decode($str);
}
```

