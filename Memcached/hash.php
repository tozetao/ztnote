<?php
/**
 * 一致性哈希算法实现
 */


// 将字符串转成0-2^32整数的接口
interface hash{
	public function _hash($str);
}

// key分布在节点机器上
interface distribution{
	// 找节点服务器功能
	public function lookup($key);

	// 添加节点功能
	// public function addNode($node);
}

/**
* 取模分布式算法
*/
class Moder implements hash,distribution
{
	protected $_nodes = array();	//节点数组
	protected $_cnt = 0;	//节点个数

	// 新增一个节点
	public function addNode($node)
	{
		if(in_array($node, $this->_nodes)){
			return true;
		}
		$this->_nodes[] = $node;
		$this->_cnt += 1;
		return true;
	}

	public function delNode($node)
	{
		if(!in_array($node, $this->_nodes)){
			return true;
		}

		$key = array_search($node, $this->_nodes);
		unset($this->_nodes[$key]);
		$this->_nodes = array_merge($this->_nodes);	//重新对索引排序
		$this->_cnt -= 1;	//节点减1
		return true;
	}

	public function lookup($key)
	{
		$key = $this->_hash($key) % $this->_cnt;
		return $this->_nodes[$key];
	}

	public function _hash($str)
	{
		// 将字符串转成32位无符号整数
		return sprintf('%u', crc32($str));
	}
}

/**
* 一致性哈希算法实现
*/
class Consistent implements hash,distribution
{
	protected $_nodes = array();
	
	//每个服务器的虚拟节点个数
	protected $_virnum = 64;

	protected $_virNodes = array();


	public function _hash($str)
	{
		// 将字符串转成32位无符号整数
		return sprintf('%u', crc32($str));
	}

	// 查找key的所属服务器节点
	public function lookup($key)
	{
		$point = $this->_hash($key);
		$node = current($this->_virNodes);
		foreach ($this->_virNodes as $k => $v) {
			if($k >= $point){
				$node = $v;
				break;
			}
		}
		return $node;
	}

	public function addNode($node)
	{
		// $this->_nodes[$this->_hash($node)] = $node;
		// 如array(13亿 => true)
		
		for($i=0; $i<$this->_virnum; $i++){
			$temp = $node . '-' . $i;
			$this->_virNodes[$this->_hash($temp)] = $node;
		}
		
		$this->nodeSort();
	}

	// 循环所有虚拟节点，根据值来删除服务器节点
	public function delNode($node)
	{
		foreach ($this->_virNodes as $k=>$v) {
			if($node == $v){
				unset($this->_virNodes[$k]);
			}
		}
	}

	protected function nodeSort()
	{
		ksort($this->_virNodes, SORT_REGULAR);
	}

	public function getNodes()
	{
		print_r($this->_virNodes);
	}
}


// $con = new Consistent();

// echo $con->_hash('a');
// $con->addNode('a');
// $con->addNode('b');
// $con->addNode('c');

// $con->getNodes();
// echo $con->_hash('name'), "\n";
// echo $con->lookup('name'), "\n";

// echo $con->_hash('age'), "\n";
// echo $con->lookup('age'), "\n";

// echo $con->_hash('address'), "\n";
// echo $con->lookup('address'), "\n";

// Moder测试
$moder = new Moder();
$moder->addNode('a');
$moder->addNode('b');
$moder->addNode('c');
$key = 'name';
echo $moder->_hash($key), "\n";
echo $moder->lookup($key), "\n";