<?php
// 配置文件，配置memcached的节点信息
$_mservs = array();
$_mservs['A'] = array(
	'host' => '192.168.1.125',
	'port' => 11211
);
$_mservs['B'] = array(
	'host' => '192.168.1.125',
	'port' => 11212
);
$_mservs['C'] = array(
	'host' => '192.168.1.125',
	'port' => 11213
);
$_mservs['D'] = array(
	'host' => '192.168.1.125',
	'port' => 11214
);
$_mservs['E'] = array(
	'host' => '192.168.1.125',
	'port' => 11215
);

$_dis = 'Moder';	//Consistent or Moder