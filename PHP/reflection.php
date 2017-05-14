<?php
/*
Callback(回调类型)
    version：>= 5.4
    Callback用于指定回调类型，一些函数如call_user_func()或usort()可以接受用户自定义的回调函数作为参数。

    这里的回调函数不止是函数，也能是对象的方法、包括静态方法。

    传递
        1. 简单的php函数将以string类型传递其名称。
        2. 传递一个对象的方法，要将对象的方法作为数组传递，下表0是对象，1是方法名
        3. 静态方法的传递：ClassName::methodName

*/
// function my_call_func(){
//     echo 'my call func';
// }

// class MyCall
// {
//     public function test(){
//         echo 'MyCall::test';
//     }
// }
// call_user_func('my_call_func'); //调用函数
// echo "\n";
// $myCall = new MyCall();
// call_user_func([$myCall, 'test']);

/*
反射
ReflectionClass
    ReflectionClass报告了一个类的有关信息。
    getMethods()
        获取类的方法的一个数组。

class Test{
    private $test = "";
 
    public function setTest($test)
    {
        $this->test = $test;
        return $this;
    }
 
    public function getTest()
    {
        return $this->test;
    }

    public function run(){
        $class = new ReflectionClass('test');
        $methods = $class->getMethods();
        var_dump($methods);
    }
}

$class = new ReflectionClass('test');
$methods = $class->getMethods();
var_dump($methods);
// var_dump($methods[0]);
*/

// class Demo{
//     public function test(){
//         echo 'is test';
//     }

//     public function __call($name, $arguments){
//         var_dump($name);
//         echo "\n";
//         var_dump($arguments);
//     }

//     public static function __callStatic($name, $arguments){
//         var_dump($name);
//         echo "\n";
//         var_dump($arguments);
//     }
// }

// $demo = new Demo();
// $demo::fuck('xiaoli', 25);



/**
* 
*/
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

class Person extends Human{
    public function main()
    {
        parent::outputCount();  //调用静态方法
        parent::test();     //调用普通方法

    }
}

$human = new Person();
$human->main();

// self，parent 和 static这三个特殊的关键字是用于在类定义的内部对其属性或方法进行访问的。 
class MyClass{
    const CONST_VALUE = ' a const value';

    public function test()
    {
        echo 'test parent', "\n";
    }

    public function __construct()
    {
        echo 'MyClass', "\n";
    }

    public static function test_parent()
    {
        echo 'test_parent', "\n";
    }
}

class OtherClass extends MyClass{
    public static $my_static = 'static var';

    public static function static_func()
    {
        echo 'static func', "\n";
    }

    public function __construct()
    {
        parent::__construct();
        echo 'sub class', "\n";
    }

    public static function doubleColon()
    {
        echo parent::CONST_VALUE . "\n";
        parent::test_parent();
        // parent::__construct();   //为实例化，无法访问

        self::static_func();
        echo self::$my_static . "\n";
    }
}
$classname = 'OtherClass';
$classname::doubleColon();
$o = new OtherClass();   //父类的构造方法被调用
OtherClass::doubleColon();