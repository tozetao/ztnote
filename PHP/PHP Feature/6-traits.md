## traits
php5.4新增的特性，traits一种代码复用机制，为了减少单继承语言的限制，能在不用层次结构内独立的类中复用method。

traits于class类似，但它仅是用于为class组合功能，自身是无法实例化的。

example:
```php
namespace Web;

trait ezcReflection{
	public function getReturnType(){
		echo __METHOD__;
	}
}

class User{
	use ezcReflection;
}

$user = new User();
$user->getReturnType();
```

### 1. 优先级
traits定义的方法是可以被覆盖的，优先顺序是当前类的成员覆盖trait的方法，而trait的方法会覆盖基类的方法。
	
### 2. 多个trait
```php
trait first_trait{
	public function first_method()
	{
		echo 'first method';
	}
}

trait second_trait{
	public function second_method(){
		echo 'second method';
	}
}

class MyClass{
	use first_trait, second_trait;
}

$obj = new MyClass();
$obj->first_method();
$obj->second_method();
```
	
### 3. 冲突
多个trait可能会插入同名的方法，