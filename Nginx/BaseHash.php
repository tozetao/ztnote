<?php
/*
��ͨhash�ֲ�
	����ܼ򵥣�һ��Hash��������������Ҫ��key������md5��ȡǰ8λ��Ȼ�󾭹�Hash�㷨����һ������������������Է�����������ģ���õ��ľ��Ƿ������б�ı�š����ַ�ʽ��ȱ���Ƿ����������ı��ͬһ��key��ͬhash����ȡ����ֵ�ˡ�
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
//����KEY��ȡhash
$hash = $this->test($key);
$count = count($memcacheList);
$memcache = $memcacheList[$hash % $count];
$mc = new Memcached($memcache);
$mc->set($key, $value);
