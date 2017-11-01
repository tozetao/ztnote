### PHP文件锁
- LOCK_SH：共享锁
当前程序获得共享锁时，其他程序也能够获得该文件的共享锁，并允许读取文件，
但是其他程序无法获取独占锁，必须等待获得共享锁的程序释放该锁。

- LOCK_EX
独占锁，当前程序获得独占锁时，其他程序必须等当前程序释放独占锁时，才能获取锁。

注：当前程序加锁时，其他程序可以不获取锁直接操作(读写)文件，不通过锁的方式其他程序时不会阻塞的。

```c
//page1.php
$handler = fopen('file.txt');
//加共享锁
flock($handler, LOCK_SH);
while(!feof($handler))
{
	echo fgets($handler);
}
sleep(5);
//解锁
flock($handler, LOCK_UN);


//page2.php
$handler = fopen('file.txt');
//加共享锁
flock($handler, LOCK_EX);
while(!feof($handler))
{
	echo fgets($handler);
}
sleep(5);
//解锁
flock($handler, LOCK_UN);
```
在页面1先打开page1页面，同时在另外一个页面打开page2页面，page2页面的程序必须等待page1页面程序释放锁后才能执行。
page2的程序获取文件独占所，程序发生了阻塞，无法执行。



### 并发下的一些测试
- 从文件中读取库存数量，测试并发下库存减少的情况
```c
// 并发下，递减数字的demo
// ab -n 200 -c 10
// 发出200个请求，10个并发请求同时请求。

function record($data = null)
{
	if(!empty($data))
	{
		$date = date('Y-m-d H:i:s', time());
		file_put_contents('log.txt', $date . '：', FILE_APPEND);
		file_put_contents('log.txt', $data, FILE_APPEND);	
	}
	file_put_contents('log.txt', "\n", FILE_APPEND);
}

$filename = './content.txt';
$handle = fopen($filename, 'a+b');

if(!$handle)
	record('fopen error');

$store = fgets($handle, 1024);	//库存数量
record('current store: ' . $store);

if($store > 0)
{
	$result = ftruncate($handle, 0);	//清空文件内容
	if(!$result)
		record('ftruncate fail');
	
	$store--;
	record('--store: ' . $store);
	$result = fwrite($handle, $store);	//写入库存
	if(!$result)
		record('write fail');
}
fclose($handle);	//关闭
```
多个线程取得文件流后，同时对该文件进行写入，这时候写入的内容是不正确的，因此上述的代码content.txt文件中的值写入是错误的，并不会正确递减，record()函数写入日志的时候也能发现，写入的内容是随机穿插的，并不会按照你理想的顺序进行写入。

### 扩展：对比mysql的锁机制
