<?php
/*
1. 路由
	app/Http/routes.php，该文件定义应用中的大多数路由，这个文件加载了App\Providers\RouteServiceProviders类。

	1.1 基础路由
		Route::get('/hello', function(){
			return 'hello';
		});
		// 请求方式是get方法才能正确响应

		Route::post('foo/bar', function(){})
		Route::put('foo/bar', function(){})
		Route::delete('foo/bar', function(){})
		
		match
			match方法可以指定要匹配的请求方式。
		any
			any会响应所有请求类型。

		url()
			辅助函数，为路由生成url。

	1.2 路由参数
		Route::get('/param/{name}/by/{age?}');
		{name}：必选参数
		{age?}：可选参数
		
		在这里可以添加正则表达式来限制参数，如下所示：
		Route::get('user/{name}/{age}', function(){
			// code
		})->where(['age' => '[0-9]+', 'name' => '[a-z]+']);

		pattern()
			该方法能对全局参数进行限制。
	
	1.3 方法欺骗
		http表单并没有put和delete方法的请求，我们可以添加隐藏_method字段指定请求方法。

	1.4 路由命名
		通过as关键字为一个路由命名，可以更方便的产生URL与重定向特定路由。
		example：
			Route::get('/aliasTest', ['as'=>'profile', function(){
				return 'test alias';
			}]);	//命名一个at的路由

			$url = route('profile');	//通过别名产生路由URL
			redirect()->route('profile');	//通过别名重定向

	1.5 路由分组
		有的时候许多路由会有公用的需求，例如中间件、命名空间、url区段等，可以利用路由群组套用这些相同的属性到多个路由上。
	
		Route::group(['middleware' => 'test'], function(){
			Route::get('/update', function(){});
			Route::get('/delete', function(){});
		});
		// middleware指向了多个中间件，test对应一个中间件，在Kernel文件中注册。
		// 例如访问example.com/update路由的时候，test中间件就会执行。

		Route::group(['prefix' => 'laravelacademy'], function(){
			Route::get('/update', function(){});
			Route::get('/delete', function(){});
		});
		// url区段
		// 定义了一个路由前缀分组，能通过 yoursite.com/laravelacademy/update来访问

	注：RouteServiceProviders内置包含了App\Http|Controllers命名空间，让我们不必使用完整的命名空间来注册控制器。

2. CSRF
	2.1 原理和防范
		跨域请求伪造，简单的说CSRF攻击就是就是创建一个ajax按钮或者表单来针对你的网站发出的一个请求，如果是钓鱼网站，会话的表单中包含敏感信息，这样对用户是很危险的。
	
		解决：CSRF token
			a. 服务器发给客户端一个token
			b. 客户端提交的表单携带这个token
			c. 如果token不合法，服务器拒绝请求

		注：这个token是不能被访问到的。

	2.2 laravel的解决方法
		laravel通过为每个用户Session创建了一个CSRF token，该token用于验证登录用户和发起请求者是否是同一人。
		
	2.3 从CSRF验证中排除指定url
		并不是所有请求都要避免csrf攻击，比如在第三方API获取数据的请求。
		在VerifyCsrfToken中间件的$except属性的数组中，添加要排除的路由。

3. 中间件
	中间件看做是装饰器，在请求到达最终动作之前对请求进行过滤和处理，例如：用户认证、日志、维护模式、开启session或csrf验证等，所有的中间件都位于App\Http\Middleware目录中。

	3.1 创建

	3.2 前置和后置中间件

	3.3 注册中间件
		app/Http/Kernel.php用于注册中间件。
		$middleware属性列表注册的中间件，作用于应用的所有http请求。

		$routeMiddleware属性列表注册的中间件，用于指派指定的路由，例如：
		Route::get('foo/bar', ['middleware' => 'test', function(){
			// code
		}])
		// 为foo/bar路由指定一个test中间件。

4. 控制器
	4.1 控制器中间件
		通过路由文件配置：
			Route::get('profile', ['middleware' => 'auth', 'uses' => 'UserController@index']);
		或者在控制器中指定中间件：
			$this->middleware('auth');

	4.2 隐式控制器
		定义单一路径来处理控制器的每一项行为，控制器的方法名以请求类型为前缀命名。

		Route::controller(uri, controllerNmae)
			uri：
				这个参数是控制器要处理的base URI。
			controllerNmae：
				这个参数是控制器名称。
		
		example：
			控制器方法：IndexController::getUser()

			路由定义：Route::controller('admin/index', 'admin\IndexController');

			访问：example.com/admin/index/user

			也可以定义控制器中的一些路由，传入第三个参数即可。
			Route::controller('admin/index', 'Admin\IndexController', [
				'getIndex' => 'user.index'
			])
			route('user.index')
			这样就能用route()函数使用别名来产生url。


	4.3 RESTful风格控制器
		Route::resource()来定义资源控制器的路由。
		
		通过4个http动词来操作资源
		get：获取资源
		post：新建资源
		put：更新资源
		delete：删除资源

		example：
		Route::resource('cate', 'CateController');

		请求方法  	方法  	  	路由名称	  	路径
		get  		index   	cate:index 		/cate
		
		get 		create 		cate:create 	/cate/create
		用于显示创建一个资源的页面
		post 		store 		cate:store 		/cate
		创建资源

		get  		edit  		cate:edit 		/cate/{id}/edit
		用于显示一个更新资源的页面
		put/patch 	update 		cate:update 	/cate/{id}
		更新

		delete 		destory 	cate:destory	/cate/{id}
		删除

		get  		show  		cate:show 		/cate/{id}
		

	4.4 依赖注入和控制器
		构造函数注入，通过反射解析出类名，再进行对象的注入。
			use App\Repositories\UserRepositories;
			public function __construct(UserRepositories $users){
				$this->users = $users;
			}

		方法注入
			use Illuminate\Http\Request;
			public function index(Request $request){}
		
		注：如果你的控制器方法有路由参数，只需要在其他依赖之后列出参数即可。
	
	4.5 路由缓存
		部署的时候使用，提供访问速度。

5. Http请求
	在laravel中，将请求实例封装成Request对象，Illuminate\Http\Request。
	通过依赖注入，在控制器中就可以使用该对象了。
	
	有很多实用的方法，看手册，包括获取cookie、获取上传文件等。

6. 响应
	响应基本内容、重定向、json、jsonp、xml，本质其实就是设置响应头、响应类型和响应内容。

	6.1 基本响应
		最基本的响应是在路由中返回字符串，例如：
		Route::get('/', function(){
			return 'hello world';
		});

		Illuminate\Http\Response，该实例对象提供响应视图、附加cookie、设置响应头等接口。
		
		设置响应头
			return (new Response($content, $status))
	              ->header('Content-Type', $value);
	    响应视图
	    	response()->view('hello');

	6.2 重定向
		看手册，很多种重定向的方法

	6.3 其他响应
		json、jsonp、建立文件下载响应	
	
	6.4 通过Response组件输出与通过echo、print输出的区别
		无论是输出文件、视图、json、jsonp，laravel的http组件都会自己合理选择响应头、代码等等来输出，而echo则是php引擎自己输出的，php会以默认响应头、文件格式来进行输出。

		为了响应正确的状态吗、响应头，所以才有http响应组件。

7. 视图
	视图文件位于/resources/views目录下。
	
	7.1 传递数据
		view('page')->with('name', 'zhangsan');
		view('page', $data);	//$data必须是一个数组，在视图中直接以键去访问即可。

	7.2 共享数据
		// 通过辅助方法，view()没有参数的时候，
		// 其实返回的是Illuminate\Contracts\View\Factory 合约 (contract) 的实现 (implementation)。
		view()->share('data', [1,3,4]);
		
		//通过View的Factory，Illuminate\Contracts\View\Factory 合约 (contract)
		View::share('data', [1,2,4,5]);

		注：通常我们应该在服务提供者的boot()方法中写入这些代码。

	7.3 视图组件
		视图组件就是在渲染视图的时候，要调用的闭包或者类方法。
		如果你想在每次渲染某些视图时绑定特定的数据，视图组件可以完成这个功能。

		View::composer(viewName, mixed exector)
			viewName
				要注册组件的视图名。
			exector
				第二个参数是闭包或者要调用的类方法。

		视图组件的绑定可以用服务提供者来绑定。

8. Blade模板
	laravel
	提供的一个模板引擎，blade使用的是模板继承(template inheritance)和区块(sections)来实现模板引擎，所有的blade模板都要以.blade.php为后缀名。
	
	8.1 区块指令
		@extends()
			指定要继承的Blade布局页面
		
		@yield(name)
			定义一个name的可重写区块

		@section('sidebar')
		@show
			定义一个区块

		@section()
		@stop
			重写一个区块

		@parent
			引用所继承Blade布局页面区块内容

		如果一个视图继承@extends了一个Blade页面布局，那么子视图可以重写Blade页面布局的区块内容。

	8.2 输出数据
		{{  }}


9. url生成
	url()、URL::to()
		相对于/public来生成地址。

		$url = url('admin/hello');	//http://localhost/lar_blog/public/admin/hello
		// 如果你的访问网址是http://localhost/lar_blog/public/admin/tag

	route()、URL::route()
		就是使用路由别名来生成url，参数是以?xx=xx携带的。
	
	action()、URL::action()
		根据控制器的动作来生成URL，对于以注册路由的控制器动作，参数是以?x=xx携带的，对于隐式控制器，参数是以x/x/x来携带的。	

		$url = action('FooController@method');



10. validate
	1. ValidatesRequests
		laravel的基类控制器使用了ValidatesRequests trait，在check请求参数的使用，只需要在控制器中使用laravel写好的验证方法即可。
		example：
			$this->validate($request, [
				'name' => 'required|max:5'
			])
		如果验证未通过，将会重定向到上一次的位置，并且所有验证信息都会自动一次性放到session当中。

		1.1. 错误信息，$errors
			$errors变量是Illuminate\Support\MessageBag的一个实例。
			该变量会通过web中间件组中的Illuminate\View\Middleware\ShareErrorsFromSession中间件绑定到视图，如果使用了该中间件，那么$errors变量在视图中总是有效，从而方便你随时使用。

			注：laravel会总是从session中检查错误信息，如果有的话会自动绑定到视图，错误信息存储到errors变量中。
	
		1.2. 自定义错误格式
			http://laravelacademy.org/post/3279.html
	
	2. Validator
		使用Validator门面创建validation对象，也是实现验证的一种方式，具体看手册。
		example：
			$validator = Validator::make($request->all(), [
				'name' => 'required|min:5'
			]);
	
	3. 替换错误信息
		在语言包中替换语言文件。
		或者
		Validator::make($data, [rules], ['required' => ':attribute is needed'])

		具体看手册

	resources/lang/en
		该目录下放着 验证提示信息、分页标签信息、，你可以创建自己的语言文件
		语言文件包。

Form Request
	在laravel中，每个请求都会被封装成一个Request对象，Form Request对象就是包含验证逻辑和授权逻辑的请求对象。
	
	Illuminate\Foundation\Http\Request;	
	Illuminate\Foundation\Http\FormRequest;