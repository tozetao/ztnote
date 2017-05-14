<?php
// 在各个memcached节点中插入1000条数据
// 引入配置文件，遍历各节点插入1000条数据

set_time_limit(0);

require('./Config.php');

$diser = new $_dis();
$mem = new Memcache();

foreach ($_mservs as $k => $v) {
	$diser->addNode($k);
}

for($i=0; $i<10000; $i++){
	$key = 'key' . sprintf('%04d', $i);;

	$value = 'value' . $i;
	$serv = $_mservs[$diser->lookup($key)];
	// $mem->connect($serv['host'], $serv['port'], 2);
	$mem->pconnect($serv['host'], $serv['port'], 2);	//打开的是长连接
	$mem->add($key, $value, 0, 0);

	usleep(3000);
}

echo 'full';

// foreach ($_mservs as $key => $mserv) {
// 	$mem->connect($mserv['host'], $mserv['port'], 5);
// 	for($i=0; $i<1000; $i++){
// 		$mem->add('key'.$i, 'value'.$i, 0, 0);
// 	}
// 	echo $key, '号服务器初始化完毕', '<br/>';
// }