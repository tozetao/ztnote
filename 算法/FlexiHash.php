<?php

/*
一致性Hash分布
	一致性Hash尽管也会造成数据的丢失，但是损失是最小的。
    将2的32次方-1想象成一个圆环，服务器列表在上面排列。根据key通过hash算法求得在圆环上的位置，那么所需要的服务器的位置在key的位置前面最近的一个（顺时针）。
 */
class FlexiHash{
    //服务器列表
    private $serverList = array();
    //是否排序
    private $isSort = false;

    /**
     * Description: Hash函数，将传入的key以整数形式返回
     * @param string $key
     * @return int
     */
    private function myHash($key){
        $md5 = substr(md5($key), 0, 8);
        $seed = 31;
        $hash = 0;
        for($i=0; $i<8; $i++){
            $hash = $hash * $seed + ord($md5[$i]);
        }
        return $hash & 0x7FFFFFFF;
    }

    /**
     * Description: 添加新服务器
     * @param $server
     */
    public function addServer($server){
        $hash = $this->myHash($server);
        if(!isset($this->serverList[$hash])){
            $this->serverList[$hash] = $server;
        }
        $this->isSort = false;
        return true;
    }

    /**
     * Description: 删除指定服务器
     * @param $server
     * @return bool
     */
    public function removeServer($server){
        $hash = $this->myHash($server);
        if(isset($this->serverList[$hash])){
            unset($this->serverList[$hash]);
        }
        $this->isSort = false;
        return true;
    }

    /**
     * Description: 根据要操作的KEY返回一个操作的服务器信息
     * @param $key
     * @return mixed
     */
    public function lookup($key){
        //将指定的KEYhash出一个整数
        $hash = $this->myHash($key);
        if($this->isSort !== true){
            krsort($this->serverList);
            $this->isSort = false;
        }
        foreach($this->serverList as $key=>$server){
            if($key <= $hash){
                return $server;
            }
        }
        return array_pop($this->serverList);
    }
}
//使用方法
$mc = new FlexiHash();
$mc->addServer('192.168.1.2');
$mc->addServer('192.168.1.3');
$mc->addServer('192.168.1.4');
$mc->addServer('192.168.1.5');

echo 'KEY=key1时，操作的服务器为：'.$mc->lookup('key1').'<br>';
echo 'KEY=key1时，操作的服务器为：'.$mc->lookup('key2').'<br>';
echo 'KEY=key1时，操作的服务器为：'.$mc->lookup('key3').'<br>';
echo 'KEY=key1时，操作的服务器为：'.$mc->lookup('key4').'<br>';
echo 'KEY=key1时，操作的服务器为：'.$mc->lookup('key5').'<br>';