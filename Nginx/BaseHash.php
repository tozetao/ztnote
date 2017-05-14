<?php
/*
普通hash分布
	代码很简单，一个Hash函数，根据所需要的key，将他md5后取前8位，然后经过Hash算法返回一个整数。将这个整数对服务器总数求模。得到的就是服务器列表的编号。这种方式的缺点是服务器数量改变后，同一个key不同hash，将取不到值了。
 */
function test($key='name'){
    $md5 = substr(md5($key), 0, 8);
    $seed = 31;
    $hash = 0;
    for($i=0; $i<8; $i++){
        $hash = $hash * $seed + ord($md5[$i]);
    }
    return $hash & 0x7FFFFFFF;
}

$memcacheList = array(
        array('host'=>'192.168.1.2', 'port'=>6379),
        array('host'=>'192.168.1.3', 'port'=>6379),
        array('host'=>'192.168.1.4', 'port'=>6379),
        array('host'=>'192.168.1.5', 'port'=>6379),
);
$key = 'username';
$value = 'lane';
//根据KEY获取hash
$hash = $this->test($key);
$count = count($memcacheList);
$memcache = $memcacheList[$hash % $count];
$mc = new Memcached($memcache);
$mc->set($key, $value);
