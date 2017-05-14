<?php
// 统计各个节点的平均命中率
require('./Config.php');

$mem = new Memcache();
foreach ($_mservs as $key => $mserv) {
	$mem->connect($mserv['host'], $mserv['port'], 5);
	$res = $mem->getStats();
	$gets += $res['cmd_get'];
	$hits += $res['get_hits'];
}
if($gets > 0){
	$rate = $hits/$gets;
}

echo $rate ;