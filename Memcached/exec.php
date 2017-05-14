<?php 
// 模拟减少一台服务器并请求
require('./Config.php');

$mem = new Memcache();
$diser = new $_dis();

foreach ($_mservs as $k => $v) {
	$diser->addNode($k);
}

// 模拟减少一台服务器
$diser->delNode('A');

for($i=1; $i<=100000;$i++){
	$i = $i % 10000 + 1;	//

	$serv = $_mservs[$diser->lookup($key)];	//根据key获取后端节点
	$mem->pconnect($serv['host'], $serv['port'], 2);

	// 从缓存中获取数据，null则重新添加
	if(!$mem->get('key'.$i)){
		$mem->add('$key'.$i, 'value'.$i, 0, 0);
	}
	usleep(3000);
}

