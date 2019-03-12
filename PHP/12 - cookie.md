cookie是将数据保存在客户端的一种技术，优点是数据存储在客户端，方便下次对客户端进行处理，缺点是cookie容易被窃取和伪造。



### 原理

cookie是http头的一部分，如果有设置一个cookie那么在响应请求时会有Set-Cookie响应头，表示客户端需要存储服务器响应的cookie；如果客户端的cookie没过期那么在发出请求时会将cookie携带过来，会有Cookie请求头。 

php设置cookie是使用setcookie()函数，由于cookie属于响应头的一部分，所以在设置的时候要在输出数据之前调用，具体用法看手册。



### 生命周期
setcookie()函数的第三个参数决定了cookie的生命周期，0或者省略表示在回话结束后过期（即浏览器窗口关闭，这里的关闭是指整个窗口的关闭，并不是指关闭某个窗口选项卡）。

该参数是一个unix时间戳，例如：time()+30秒表示在30秒后过期



### 作用域

cookie的作用域，一个cookie被设置后，只允许被当前脚本所在的文件夹和其子文件夹内的脚本访问，同级其他目录和父级目录是无权限访问的。

example:
```php
setcookie('all','10090',time()+3600,'/');
# 设置根目录作用域
```



### 跨域

默认情况下cookie是只对当前域名有效的，例如当前站点是www.example.com，在www.example.com站点下的cookie是不能在admin.example.com站点下生效的。

cookie跨域指的是允许cookie在所有一级域名下的不同二级域名之间共享cookie数据，在php中跨域只需要设置setcookie()函数的第5个参数即可。

example：
```php
setcookie('name', 'lisi', 0, '/', 'example.com');
# 设置在一级站点下通用
```
注：字符串.com这种形式的网址就是一级域名，在一级域名之前以.增加其他字符串，就是二级域名，例如：baidu.com是一级域名，3w.baidu.com是二级域名。




### cookie与数组
setcookie()函数无法直接保存数组。

例如：
```php
$arr=array(1,2,3,4,5);
setcookie('arr',$arr);//错误的。

//通过这种形式，将数组信息保存到cookie
setcookie('arr[num1]',1);
setcookie('arr[num2]',2);
//下次请求的时候，php会当作数组来处理。
```
