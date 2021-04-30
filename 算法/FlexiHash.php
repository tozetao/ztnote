<?php

/*
һ����Hash�ֲ�
	һ����Hash����Ҳ��������ݵĶ�ʧ��������ʧ����С�ġ�
    ��2��32�η�-1�����һ��Բ�����������б����������С�����keyͨ��hash�㷨�����Բ���ϵ�λ�ã���ô����Ҫ�ķ�������λ����key��λ��ǰ�������һ����˳ʱ�룩��
 */
class FlexiHash{
    //�������б�
    private $serverList = array();
    //�Ƿ�����
    private $isSort = false;

    /**
     * Description: Hash�������������key��������ʽ����
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
     * Description: ����·�����
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
     * Description: ɾ��ָ��������
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
     * Description: ����Ҫ������KEY����һ�������ķ�������Ϣ
     * @param $key
     * @return mixed
     */
    public function lookup($key){
        //��ָ����KEYhash��һ������
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
//ʹ�÷���
$mc = new FlexiHash();
$mc->addServer('192.168.1.2');
$mc->addServer('192.168.1.3');
$mc->addServer('192.168.1.4');
$mc->addServer('192.168.1.5');

echo 'KEY=key1ʱ�������ķ�����Ϊ��'.$mc->lookup('key1').'<br>';
echo 'KEY=key1ʱ�������ķ�����Ϊ��'.$mc->lookup('key2').'<br>';
echo 'KEY=key1ʱ�������ķ�����Ϊ��'.$mc->lookup('key3').'<br>';
echo 'KEY=key1ʱ�������ķ�����Ϊ��'.$mc->lookup('key4').'<br>';
echo 'KEY=key1ʱ�������ķ�����Ϊ��'.$mc->lookup('key5').'<br>';