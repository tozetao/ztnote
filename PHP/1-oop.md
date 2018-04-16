## 类与对象 
面向对象的三个特性：封装、继承、多态。
所谓的面向对象开发是指在程序中通过模拟现实中个体的交互来进行开发，它要对需求进行提炼，这里有一个类和对象的概念。

### 1. 类

类是对一类事物共性的一个总结，是一个概念上的对象。
比如人类，有自己的特点和行为，特点的体现比如有眼睛/耳朵/鼻子等特点，行为有跑/跳/说话等，所以对事物的特点和行为的总结，成为类。

在代码中，我们用class来代表类，类的特点被叫做属性，用变量来模拟，而行为被叫做方法，用函数来模拟。

### 2. 对象
对象是一个个具体的实例，例如人类会有各种不同的人，像张三/李四等等，这些就是具体的实体，就叫做对象。

每个实体对象都是由类来生成的，因此对象都有自己的属性和方法。

### 3. 构造方法
在进行实例生成的时候，构造函数就会被调用，一般用于做初始化工作。

构造函数在继承中的表现：
子类在继承父类的时候，如果有重新定义自身的构造方法，那么php是不会隐式的去调用父类的构造方法的，必须显示的去调用，这个特性也适用于其它方法（无论是析构函数还是魔术方法）。
```php
class BaseClass{
	public function __construct()
	{
		echo 'base class', "\n";
	}
}

class SubClass extends BaseClass{
	public function __construct()
	{
		parent::__construct();
		echo 'sub class', "\n";
	}
}

class OtherSubClass extends BaseClass
{
	
	function __construct()
	{
		echo 'other sub class', "\n";
	}
}

$baseClass = new BaseClass();	//baseclass
$subClass = new SubClass();	//base class，sub class
$otherSubClass = new OtherSubClass();	//other sub class
```

### 4. 析构函数
在某个对象的所有引用都被删除或当对象被显式销毁时才执行。
```php
example：
class Destructable{
	function __construct(){
		print "in __construct\n";
		$this->name = 'Destructable';
	}

	function __destruct(){
		print "Destorying " . $this->name . "\n";
	}
}
$obj = new Destructable();
```
注：子类的析构函数也不会隐式的去调用父类的析构函数，需要自己手动去调用。

### 5. 访问权限的控制
类和方法是有访问权限的，权限的控制是通过在前面添加关键字 public（公有），protected（受保护）或 private（私有）来实现的。

- public：公有，能被任意访问
- protected：受保护的，只能被自身和继承的对象所访问
- private：私有，只能由自身所定义的类访问

### 6. 继承
继承一个类，子类会继承父类所有公有的、受保护的属性和方法。
PHP没有多继承，但是可以实现多个接口。

继承对象模型：
子类的对象，其实是有继承父类的私有对象和熟悉的，你可以输出子类的对象查看结构，可以看到父类的私有属性也是会继承的，
只不过因为PHP的私有权限控制实现是通过调用者属性（方法）的类与执行的上下文代码的所属类是否一致来判断的，如下所示：

```php
class Human{
    private $wife='小甜甜';  //该属性仅限于Human类内访问。
    public $age=22;
    public function cry(){
        echo '55555';
    }
    public function show(){
        echo $this->wife;
    }
}

class Stu extends Human{
    private $wife='凤姐'; //该属性仅限于Stu类内访问。
    public $height=175;

    public function show(){
        parent::show();
        echo $this->wife;
    }
}

// $stu=new Stu();
// var_dump($stu);	//可以看到私有属性
// $stu->show();		//输出小甜甜/凤姐
```
PHP的private实现会导致一个bug：同一个类的不同实例能互相访问对象的私有成员
```php
class Test{
	private $name;

	function __construct($name)
	{
		$this->name = $name;
	}

	private function bar(){
		echo 'accessed the private method';
	}

	public function baz(Test $other){
		$other->name = 'hello';
		var_dump($other->name);

		$other->bar();
	}
}
$test = new Test('test');
$test->baz(new Test('other'));	//可以看到，other实例对象的name属性被改变了，私有方法也被调用了
```

### 7. 多态
PHP因为是弱类型的语言，因此没有多态这个概念，但是仍然可以实现。


### 8. 重写
子类继承父类的时候，可以重新对父类实现过的方法进行重新实现（protected/public），private的成员是没有重写这个概念的，因为你无法去覆盖它。

example：
```php
class Human{
	public function say($name){
		echo $name."，吃了吗？";
	}
}
class Stu extends Human{
	public function say(){
		echo "且可恼。。。。";
	}
}
$s=new Stu();
$s->say();	//会调用子类的方法
$s->say('zhangsan');
```
注：子类重写父类的方法，重写的方法访问权限一定要大于或等于父类方法的访问权限，public > protected 

### 9. 静态属性与静态方法
被static关键字修饰过的属性和方法，就叫做静态属性或静态方法。

静态属性/静态方法是属于类的，这意味着它们并不会因为对象的实例化而影响，调用也是使用类来进行调用，例如：
```php
class Human{
	static $count;
	static function test(){}
}

echo Human::$count;
echo HUman::test();
```

static的属性不会被继承，但是static的方法会被继承。
```php
class Person extends Human{}

echo Person::$count;	//waring
echo Person::test();
```

static的内存表示方式：类的静态代码会提前存储在内存的代码区，而静态属性会存储在内存的静态变量区域。

注：对象能调用静态方法，但是不能调用静态属性。


### 10. final
final是最终的意思，在php中可用于修饰类/方法名，但是不能修饰属性，
常量的定义是通过const关键字来定义的。

final修饰的类不能被继承，final修饰的方法不能被重写。
```php
final class Human{}
class Person extends Human{}

$person = new Person();	//无法被继承
```

### 11. abstract
abstract可以修饰类和方法，所修饰的类无法实例化，只能被继承，所修饰的方法只有名字而没有方法体。

抽象类只能被继承，子类必须实现父类的抽象方法。
抽象类的意义在于规范团队协作能力，如果有不同的团队去实现不同的方案，那么有必要对不同的团队进行一些规范，相同功能该如何命名

### 12. interface
专门用于规范整个项目的设计结构的工具称之为接口。
接口的方法必须是抽象且公有的，接口可以有常量。

接口的意义：接口是比抽象类还抽象的"类"(与类结构相似)，接口纯粹用来规范整个项目的结构的。

```php
interface test{
}

# 多接口的实现
interface A{}
interface B{}
class Person implements A,B{}

# 接口可以被继承
interface A extends B{}
```

### 13. 常量
通过const关键字定义，常量是固定的，无法被修改。
常量能 类名::常量名 的形式来访问。

### 13. static/parent/this/self
- static：除了定义静态属性和静态方法，可以用来做后期静态绑定
- self：对类自身的引用，通过::操作符能访问类的静态变量、静态方法、常量和普通方法
	 parent：	代表父类自身，例如new parent()，通过::操作符访问父类的静态变量、静态方法、常量和普通方法
- this：一个对象在调用方法的时候，其方法内部有一个伪变量（$this），该变量指向调用当前方法的实例对象的引用
```php
class Human{
    public $name = 'zhangsan';

    static $count = 10;

    public function test()
    {
        echo 'test';
    }

    public static function outputCount()
    {
        echo self::$count;
    }

    public function main()
    {
        echo self::$count;  //调用静态变量
        self::test();   //调用普通方法
        self::outputCount();    //调用静态方法
    }
}
```



### PHP相对其他语言没实现的概念
重载指的是：存在多个同名的方法，但参数的个数/类型不同，传入不同的参数，则调用不同的方法；在PHP中是没有重载的实现的，Java有。

