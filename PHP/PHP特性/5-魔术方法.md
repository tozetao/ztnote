## 概述
php所提供的重载是指动态的创建类属性和方法，通过魔术方法实现，当调用当前环境下未定义或不可见的类属性或方法时，魔术方法会被调用。

注：所有的重载方法必须公有的

### 1. 属性重载(动态创建属性)
__set(string $name, mixed $value)
	对不可访问(或不存在)的属性赋值时，该方法会被调用。

__get(string $name)
	读取不可访问的属性时，该方法会调用

__isset(string $name)

__unset(string $name)

$name：要操作的变量名。
$value：要操作的变量值。
```php
example：
class PropertyTest{
	private $data = array();

	public $declared = 1;
	// 只有在类外部调用，重载才会发生
	private $hidden = 2;

	public function __set($name, $value)
	{
		$this->data[$name] = $value;
	}

	public function __get($name)
	{
		if(array_key_exists($name, $this->data)){
			return $this->data[$name];
		}
		$trace = debug_backtrace();
		trigger_error(
			'Undefined property via __get(): ' . $name .
            ' in ' . $trace[0]['file'] .
            ' on line ' . $trace[0]['line'],
            E_USER_NOTICE);
		return null;
	}

	public function __isset($name)
	{
		return isset($this->data[$name]);
	}

	public function __unset($name)
	{
		unset($this->data[$name]);
	}

	public function getHidden()
	{
		return $this->hidden;
	}
}

$obj = new PropertyTest();
$obj->a = 1;
echo $obj->a, "\n\n";

var_dump(isset($obj->a));
unset($obj->a);

var_dump(isset($obj->a));

echo "\n\n";
echo $obj->getHidden();

```
### 2. 方法重载(动态创建方法)
__call(string $name, array $arguments)
在对象中调用一个不可访问的方法时，__call()会被调用。

__callStatic(string $name, array $arguments)
用静态方式调用一个不可访问的方法时，__callStatic会被调用。

$name，方法名。
$arguments，一个枚举数组，包含着要传递给$name方法的参数。

```php
example：
class MethodTest{
	public function __call($name, $arguments){
		echo $name . "  " . implode(', ', $arguments) . "\n";
	}

	public static function __callStatic($name, $arguments){
		echo "Calling static method '$name' "
             . implode(', ', $arguments). "\n";
	}
}
$obj = new MethodTest();
$obj->runTest('in object context');
MethodTest::runTest('in static context');
```


