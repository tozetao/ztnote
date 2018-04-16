### 类的自动加载

- spl_autoload_register()

  编写自己的加载类函数，并将其注册到autoload队列中，也就是说可以编写类的自动加载的多种实现方式。


- __autoload()

  提供类的自动加载



### namespace

命名空间是解决类、函数重名的一种机制，命名空间会将代码划分出不同的空间，每个空间的常量、函数和类是互不影响的。

命名空间可以影响类（抽象类和traits）、常量、函数和接口。

#### 声明

```php
namespace Article;
//声明一个Article空间

namespace Admin/User;
//声明一个子空间

//同一个文件可以声明多个不同的空间
```



#### 使用

类允许通过3种方式来引用。

- 不包含前缀的类名称



example：使用案例

声明一个Comment类，放置在Article命名空间下，文件命名comment.php

```php
<?php
namespace Article;

/**
* Comment
*/
class Comment
{
	public function show()
	{
		echo 'show';
	}
}
```

配合use关键字，便可以优雅的加载一个类。

```php
function __autoload($class)
{
	$class = __DIR__ . '/' . str_replace('\\', '/', strtolower($class)) . '.php';
	
	if(!is_file($class))
		throw new Exception("class not found.", 1);
		
	include $class;
}

use \Article\Comment;
$comment = new Comment();
$comment->show();
```

当new一个对象时，php会使用use声明的类名来加载类文件。

输出自加载类的$class参数，可以发现值是Article\Comment，所以可以用文件系统相似的方式来定义命名空间，以此划分不同的类文件。







### 2.子空间
	命名空间的调用语法像文件路径一样是有道理的，它允许我们自定义子空间来描述各个空间之间的关系。
	子空间还可以定义很多层次，比如说Blog\Article\Archives\Date。
	
	例如article和MessageBoard这两个模块其实都是处于同一个blog，用命名空间来表达他们之间的关系如下所示：
	//example：
		//我用这样的命名空间表示处于blog下的article模块
		namespace Blog\Article;
		class Comment { }
	
	
		//我用这样的命名空间表示处于blog下的message board模块
		namespace Blog\MessageBoard;
		class Comment {}
	
	个人理解：我们可以以文件路径的格式来定义命名空间，方便描述各个空间的关系，也容易调用。

5. 调用和引入文件
	5.1 调用
		例如我有一个common_inc.php脚本文件，里面有一些好用的函数和类：
			function getIP() {}
			class FilterXSS {}

		在一个命名空间里引入这个脚本，脚本里的元素不会归属到这个命名空间。如果这个脚本里没有定义其它命名空间，它的元素就始终处于公共空间中：
	 
		//example：
			namespace Blog\Article;
			class Comment {}
	
			namespace Blog\MessageBoard;
			class Comment {}
	
			$c = new Comment();	//调用当前空间的类
			$ac = new \Blog\Article\Comment();	//调用Blog\Article空间的类
	
			var_dump($ac);
			include('./common_inc.php');
	
			// $demo = new Demo(); //出现致命错误：找不到Blog\Article\FilterXSS类
			$demo = new \Demo(); //正确
			demo();	//函数与常量不需要\
	
	5.2 公共空间
		不加 \ 是代表当前命名空间，以 \ 开头则代表公共空间。
		除了自定义的元素(上面的公共文件)，还包括PHP自带的元素，都属于公共空间。

		要提一下，其实公共空间的函数和常量不用加 \ 也可以正常调用（不明白PHP为什么要这样做），但是为了正确区分元素，还是建议调用函数的时候加上\

6. 名称术语
	在说别名和导入之前，需要知道关于空间三种名称的术语，以及PHP是怎样解析它们的。官方文档说得非常好，我就直接拿来套了。

	6.1 非限定名称，或不包含前缀的类名称
		例如 $comment = new Comment()，如果当前命名空间是Blog\Article，Comment将被解析为Blog\Article\Comment。如果使用Comment的代码不包含在任何命名空间中的代码（全局空间中），则Comment会被解析为Comment。

	6.2 限定名称，或包含前缀的名称
		例如 $comment = new Article\Comment()，如果当前的命名空间是Blog，则Comment会被解析为Blog\Article\Comment。如果使用Comment的代码不包含在任何命名空间中的代码（全局空间中），则Comment会被解析为Comment。

	6.3 完全限定名称，或包含了全局前缀操作符的名称
		例如 $comment = new \Article\Comment();。在这种情况下，Comment总是被解析为代码中的文字名(literal name)Article\Comment。

		\：在namespace作为全局前缀操作符，意为全局空间中。
	
		其实可以把这三种名称类比为文件名（例如 comment.php）、相对路径名（例如 ./article/comment.php）、绝对路径名（例如 /blog/article/comment.php），这样可能会更容易理解。

	example：
		//创建空间Blog
		namespace Blog;
		class Comment { }

		//非限定名称，表示当前Blog空间
		//这个调用将被解析成 Blog\Comment();
		$blog_comment = new Comment();
	
		//限定名称，表示相对于Blog空间
		//这个调用将被解析成 Blog\Article\Comment();
		$article_comment = new Article\Comment(); //类前面没有反斜杆\
	
		//完全限定名称，表示绝对于Blog空间
		//这个调用将被解析成 Blog\Comment();
		$article_comment = new \Blog\Comment(); //类前面有反斜杆\
	
		//完全限定名称，表示绝对于Blog空间
		//这个调用将被解析成 Blog\Article\Comment();
		$article_comment = new \Blog\Article\Comment(); //类前面有反斜杆\
	
		//创建Blog的子空间Article
		namespace Blog\Article;
		class Comment{}

7. 别名和导入
	别名和导入可以看作是调用命名空间元素的一种快捷方式。PHP并不支持导入函数或常量。
	它们都是通过使用use操作符来实现：
	example：
		namespace Blog\Article;
		class Comment{}

		//创建一个BBS空间（我有打算开个论坛）
		namespace BBS;
		//导入一个命名空间
		use Blog\Article;
	
		//导入命名空间后可使用限定名称调用元素
		$article_comment = new Article\Comment();	//在bbs空间查找不到元素则到blog\article查找
		var_dump($article_comment);
	
		//为命名空间使用别名
		use Blog\Article as Arte;
		//使用别名代替空间名
		$article_comment = new Arte\Comment();
	
		//导入一个类
		use Blog\Article\Comment;
		//导入类后可使用非限定名称调用元素
		$article_comment = new Comment();
	
		//为类使用别名
		use Blog\Article\Comment as Comt;
		//使用别名代替空间名
		$article_comment = new Comt();

	example：//错误，引入同类名
		namespace Blog\Article;
		class Comment {}

		namespace BBS;
		class Comment {}
		Class Comt {}
	
		//导入一个类
		use Blog\Article\Comment;
		$c = new Comment();	//非限定名称，代表当前空间，代表BBS\Comment()。
		$article_comment = new Comment(); //引入的Comment类与当前空间的Comment发生冲突，程序产生致命错误

8. 动态调用
	PHP提供了namespace关键字和__NAMESPACE__魔法常量动态的访问元素，__NAMESPACE__可以通过组合字符串的形式来动态访问：
	example：
		namespace Blog\Article;

		const PATH = '/Blog/article';
		class Comment {}
	
		//namespace关键字表示当前空间
		echo namespace\PATH; ///Blog/article
		$comment = new namespace\Comment();
	
		//魔法常量__NAMESPACE__的值是当前空间名称
		echo __NAMESPACE__; //Blog\Article
		//可以组合成字符串并调用
		$comment_class_name = __NAMESPACE__ . '\Comment';
		$comment = new $comment_class_name();

9. 字符串形式调用问题
	9.1 转义
		namespace Blog\Article;
		class name { }

		//我是想调用Blog\Article\name
		// $class_name = __NAMESPACE__ . "\name"; //错误，在双引号中\表示转义，\n将被转义为换行符
		$class_name = __NAMESPACE__ . "\\name";	//正确
		$name = new $class_name(); //发生致命错误

	9.2 不会认为是限定名称
		PHP在编译脚本的时候就确定了元素所在的空间，以及导入的情况。而在解析脚本时字符串形式调用只能认为是非限定名称和完全限定名称，而永远不可能是限定名称。
		
		namespace Blog;
		//导入Common类
		use Blog\Article\Common;
	
		//我想使用非限定名称调用Blog\Article\Common
		$common_class_name = 'Common';
		//实际会被当作非限定名称，也就表示当前空间的Common类，但我当前类没有创建Common类
		$common = new $common_class_name(); //发生致命错误：Common类不存在
	
		//我想使用限定名称调用Blog\Article\Common
		$common_class_name = 'Article\Common';
		//实际会被当作完全限定名称，也就表示Article空间下的Common类，但我下面只定义了Blog\Article空间而不是Article空间
		$common = new $common_class_name(); //发生致命错误：Article\Common类不存在
	
		namespace Blog\Article;
		class Common {}


自动加载与命名空间
	在实例化一个对象的时候，php会在当前脚本文件中查找该对象的类，如果查找不到，则会调用__autoload()方法。

	在写面向对象的程序时，经常要为每个类定义一个脚本文件，需要使用的时候会在每个脚本文件开头写include列表。
	例如：include "/Blog/Model/UserModel.class.php";
	
	在最新的php版本中，引入了命名空间的概念，你只需要根据所定义的命名空间的路径来创建类(脚本文件)，引入的时候通过use关键字引入即可。
	
	好处是避免了类的同名。
	因为在实例化的时候，会根据引入类的命名空间路径去寻找类文件所在。
	

 */
 */
namespace Blog\Model;

use Blog\Model\Auth;
// include "/autoload.php";	//正确的写法

// 你将__autoload()函数定义在Blog\Model空间下，那么就属于当前命名空间。
// 因此加载非当前空间下的类的时候，该函数不会执行，
// 因为包含一个公共空间下的自动加载函数，这样才会执行自动加载方法。
// 下面的代码是错误的
function __autoload($class){
	echo 'autoload: '.$class;	//__autoload()被覆盖了，不会执行。
}

/**
* UserModel
*/
class UserModel{
}

$userModel = new UserModel();
$auth = new Auth();	

