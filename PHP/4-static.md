## 后期静态绑定
php5.3.0的新特性，用于解决self、__CLASS__关键字的局限性，self调用静态方法是取决于代码所在的上下文，无法实现取决于运行时环境的动态调用。

简单的说，self写在哪里，实际调用的就是当前代码所在上下文的类，而static::不被解析为当前方法所在的类，而是解析成运行时所在的类。例如在父类定义static，在通过子类直接或间接用到static时，static代表的是子类。

static与this很相似，PHP的父类方法被子类集成后，子类能通过重写父类方法，来实现不同子类相同行为的一个不同实现，这个在强类型语言中叫做多态，PHP是弱类型语言，所以是没有多态的。

### static模拟多态
```php
class Base{
	public function say()
	{
		echo __METHOD__, '<br/>';
	}

	public function callSelf()
	{
		self::say();
	}

	public function callStatic()
	{
		static::say();
	}
	
	// 实现运行时对象
	public static get_instann(){
		return new static();
	}
}

class Son extends Base
{
	public function say()
	{
		echo __METHOD__, '<br/>';
	}
}

$son = new Son();
$son->say();
$son->callSelf();	//self的局限性，它的调用时取决于上下文环境
$son->callStatic();

var_dump(Son::getInstance());	// object(Son)#2 (0) { }
```
static和this很像，不同的是static可以用于静态属性和静态方法。

### this与static的调用域
```php
class A {
    private function foo() {
        echo "success!\n";
    }
    public function test() {
        $this->foo();
        static::foo();
    }
}

class B extends A {
}

class C extends A {
    private function foo() {
        echo 'C success!', "\n";
    }
}
	
$b = new B();
$b->test();		// success! success!

$c = new C();
$c->test();   // success!	fatal error，在A的上下文中调用C的foo私有方法
```
this与static::都是代表着该运行时对象所属的类，this会在同一作用范围内尝试调用私有方法，static会在运行时所属的类中调用私有方法。
说明：私有的方法是要隐藏起来不被外部调用的，所以在父类中去调用子类的私有方法是一种很奇怪的行为，因为是在父类的上下文中去调用子类的私有方法，会失败的。

### example3
```php

class A {
    public static function foo() {
        static::who();
    }

    public static function who() {
        echo __CLASS__."\n";
    }
}

class B extends A {
    public static function test() {
        A::foo();
        parent::foo();
        self::foo();
    }

    public static function who() {
        echo 'b ' . __CLASS__."\n";
    }
}

class C extends B {
    public static function who() {
        echo 'c ' . __CLASS__."\n";
    }
}

B::test();
C::test();
```
通过类名显式的去调用父类使用了后期静态绑定的方法，static关键字则代表显式调用的类名.