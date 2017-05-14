<?php
/*
	LOCK_EX：写锁，独占型
		获得一个文件的写锁后，程序会处于阻塞状态。
		其他获取读锁、写锁的程序处理一个队列的阻塞等待状态，只有在当前程序释放写锁，其他程序才能取得锁。

	LOCK_SH：读锁，共享型
		共享型指的是其他程序可以获得该读锁，同时读取该文件的数据，但是无法获得写锁，获取写锁的程序会处于阻塞状态。
	
	说明：当前程序处于加锁状态中，其他程序是无法读写文件的。


	共享锁
		加锁后，读锁仍然可以获取共享锁，写锁会处于阻塞状态。

	排他锁
		加锁后，一切获取锁的程序都会处于阻塞状态。
 */

// example：加锁并睡眠10秒
$fileName = './content.txt';
$handle = fopen($fileName, 'r+b');
if(!$handle){
	exit('open error');
}

if(flock($handle, LOCK_SH)){
	// fwrite($handle, "this is demo\n");
	while(!feof($handle)){
		echo fgets($handle);
	}
	sleep(10);
	fwrite($handle, 'end');
	flock($handle, LOCK_UN);	//释放共享读锁
}else{
	echo "Couldn't get the lock!";
}

fclose($handle);


// example：尝试获得锁
$fileName = './content.txt';
$resource = fopen($fileName, 'r+b');	//只读方式打开

if(!$resource){
	exit('fopen error');
}

flock($resource, LOCK_EX);	//	获取一个读锁
while (!feof($resource)) {
	$c = fgets($resource);
	echo $c, '<br/>';
}

fclose($resource);

/*

防止商品超卖
	1. 将商品库存放进缓存中，收到一个请求，加排他锁。
	2. 做业务处理，成功库存减少1，失败库存不变，
	3. 处理成功释放锁资源。
	在库存到0的时候，表示商品秒杀结束，拒绝其他用户请求，同时更新数据库商品库存。


ab.exe
	-n：一共发出的请求次数。
	-c：同一个时间点的并发请求，例如-c 10指同时发出10个请求。
 */




// 并发下，递减数字的demo
// ab -n 200 -c 10
// 发出200个请求，10个并发请求同时请求。
/*
function record($data = null){
	if(!empty($data)){
		$date = date('Y-m-d H:i:s', time());
		file_put_contents('log.txt', $date . '：', FILE_APPEND);
		file_put_contents('log.txt', $data, FILE_APPEND);	
	}
	file_put_contents('log.txt', "\n", FILE_APPEND);
}

$filename = './content.txt';
$handle = fopen($filename, 'a+b');

if(!$handle){
	record('fopen error');
}

$store = fgets($handle, 1024);	//库存数量
record('current store: ' . $store);

if($store > 0){
	$result = ftruncate($handle, 0);	//清空文件内容
	if(!$result){
		record('ftruncate fail');
	}
	$store--;
	record('--store: ' . $store);
	$result = fwrite($handle, $store);	//写入库存
	if(!$result){
		record('write fail');
	}
}
fclose($handle);	//关闭

// 多个线程取得文件流后，同时对该文件进行写入，这时候写入的内容是不正确的，因此上述的代码content.txt文件中的值写入是错误的，并不会正确递减
// record()函数写入日志的时候也能发现，写入的内容是随机穿插的，并不会按照你理想的顺序进行写入。

 
// example：加锁下的测试
function record($data = null){
	if(!empty($data)){
		$date = date('Y-m-d H:i:s', time());
		file_put_contents('log.txt', $date . '：', FILE_APPEND);
		file_put_contents('log.txt', $data, FILE_APPEND);	
	}
	file_put_contents('log.txt', "\n", FILE_APPEND);
}

$filename = './content.txt';
$handle = fopen($filename, 'a+b');

if(!$handle){
	record('fopen error');
}

$locked = flock($handle, LOCK_EX);	//排他锁
if($locked){
	$store = fgets($handle, 1024);	//库存数量
	record('current store: ' . $store);

	if($store > 0){
		$result = ftruncate($handle, 0);	//清空文件内容
		if(!$result){
			record('ftruncate fail');
		}
		$store--;
		record('--store: ' . $store);
		$result = fwrite($handle, $store);	//写入库存
		if(!$result){
			record('write fail');
		}
	}else{
		record('store < 0');
	}
	flock($handle, LOCK_UN);	//解锁
}else{
	record('lock fail');
}
fclose($handle);	//关闭
*/


// exampel：数据库库存的更新测试
function record($data = null){
	if(!empty($data)){
		$date = date('Y-m-d H:i:s', time());
		file_put_contents('log.txt', $date . '：', FILE_APPEND);
		file_put_contents('log.txt', $data, FILE_APPEND);	
	}
	file_put_contents('log.txt', "\n", FILE_APPEND);
}

$db = new Mysqli('120.24.71.47', 'xproot', 'xproot');
var_dump($db);
exit;
$result = $db->set_charset('utf8');
if(!$result){
	record($db->error);
	exit;
}

$result = $db->select_db('test');
if(!$result){
	record($db->error);
	exit;
}

// 开启事务
$db->begin_transaction();

$mysqli_result = $db->query('select * from goods');
$row = $mysqli_result->fetch_object();
$store = $row->store;
record('current: ' . $store);

// 可以看到，在开启事务后，如果select语句在前面，当前线程会获得一个行级的乐观锁，这时候其他线程也是可以得到这个悲观锁的，这样在下面的更新语句中，即使是获得排他锁，也会造成一个获取同样库存数量的排他锁队列，更新成功后，库存也是会出现问题的，变成负数。

// 可以观察log.txt文件，因为写入的日志数据是乱的，可以证明有多个线程获取到悲观锁，得到同样的数据。
// 测试成功，注意数据库字段要为无符号，否则该字段一直是0，看不出效果。

// 如果这里是先更新再查询，先获取排他锁的话，那么则是以队列形式来运行的。

if($store >= 0){
	$affected_rows = $db->query('update goods set store=store-1');	
	if($affected_rows){
		record('update success');
		$db->commit();
	}else{
		record('update fail');
		$db->rollback();
	}
}

record();
$db->close();


// if($mysqli_result->num_rows){
// }

/*
	如果是select语句，将返回mysqli_result对象，查询结果集。
	如果是update、delete、insert，将返回mysqli_stat对象。

	
	解决方案是先获得排他锁，
 */