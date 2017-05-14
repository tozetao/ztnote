<?php
/*
1. Server Providers(服务提供者)
	将laravel的ioc容器与服务组件关联起来的连接器，就叫做服务提供者。
	
	ioc容器负责注册与实例化服务对象，供上层使用，而服务提供者负责提供服务并绑定到服务器容器中，更准确的说服务提供者是为ioc容器绑定对象的生产代码。

	ioc容器说明：
		在注入一个对象的时候，ioc容器首先要有注册这个对象，这样容器才能够实例出来。
		如何实例化，由服务提供者绑定的生产代码决定。

	1.1 ServiceProviders
		Illuminate\Support\ServiceProvider;
		服务提供者的基类，所有的服务提供者都要继承自它，并实现register()方法。

		register()
			实现服务组件绑定到ioc容器的方法。
			example：
				function register(){
					$this->app->singletion('app\Http\Service', function(){
						return new Service();
					});
				}
		boot()
			启动方法，这个方法会在所有的服务提供者注册后才使用，这能让你使用框架中其他所有已注册过的服务。

	1.2 注册提供者
		所有的服务提供者都在config/app.php配置文件的providers数组注册。

	1.3 缓载提供者
		如果你的提供者仅用于绑定注册到服务容器，那么可以延缓该服务提供者的加载，在真正需要该服务的时候进行加载。

2. laravel服务容器
	服务容器即ioc容器，负责管理类依赖关系的一个容器。在laravel中，服务容器与已注册过的服务提供者绑定在一起，在服务提供者内部，可以通过$this->app来访问服务容器实例，或者通过App facade访问容器。

	2.1 注册基本解析器
		以闭包回调的方式注册，通常包含一个key(一般用类名)和一个有返回值的闭包。
		$this->app->bind('FooBar', function(){
			return new FooBar($app['something']);
		});

	2.2 注册单例
		$this->app->singleton('FooBar', function(){
			return new FooBar($app['something'])
		});

	2.3 注册实例
		$foobar = new FooBar();
		$this->app->instance('FooBar', $foobar);
	
	2.4 解析
		//通过app容器的make()方法实例化
		$this->app->make('FooBar');
		
		// 也能想访问数组一样获取对象，因为实现了php的ArrayAccess接口。
		$this->app['FooBar'];	

		// type-hint，类型指定，通过注入的方式来实例化对象(构造方法、普通方法的注入)
		namespace App\User as UserRespository;
		function __construct(UserRepository $users){
			// 其实就是通过反射获取方法参数的类型，再来实例化。
		}

	2.5 将接口绑定到实现
		简单的说，注入的实例对象是接口的一种实现，通过接口来约束行为，这样你可以注入该接口实现的不同对象。

		上下文绑定：回头测试下，有俩个类实现同一个接口，但是要注入不同实现的实例对象。

	2.6 容器事件
		容器在解析一个对象的时候，会出发一个回调事件。

3. Contracts
	3.1 what
		laravel的Contracts是一组定义了框架核心服务的接口。例如Quene contract定义了队列所需要的方法，而Mailer contract定义了发送e-mail需要的方法。
	
	3.2 why
		在laravel框架中，每个contract都提供了一个对应的实现，所有的contracts都放在各自的github仓库中，可以作为一个低耦合的扩展包让其他开发者使用。

		之所以要为每个服务组件定义接口，一个是约束组件的行为，通过定义接口能很简单的决定一个服务需要多少功能，另外一个是为了降低代码的强耦合。

		example：
			class Repository{
				protected $cache;

				public function __construct(App\Cache\Memcache $cache){
					$this->cache = $cache;
				}

				public function find($id){
					return $this->cache->find($id);
				}
			}
			// 上面的类中，代码跟缓存是强耦合的，如果底层要换一种缓存技术，例如Redis，我们就必须修改这个类

			namespace App\Orders;

			use Illuminate\Contracts\Cache\Repository as Cache;

			class Repository{
				protected $cache;

				public function __construct(Cache $cache){
					$this->cache = $cache;
				}
			}

			// Cache类定义了缓存的接口，具体如何实现，我们不需要知道。

	3.3 使用
		通过laravel的服务容器注入要使用的Contracts使用即可。

4. Facades
	Facades是提供一个静态接口给应用程序从服务容器中访问对象的类，简单的说就是能快速的从ioc容器中获取绑定的服务对象。

	Illuminate\Support\Facades\Facade类是让这个机制可以运作的原因，Laravel的facades和自定义的facades，都必须继承自Facade类。
	
	4.1 用法
		Laravel的Facades(门面)类都位于全局命名空间，其实这里引入的是门面类的别名，在app/config/app.php的aliales数组变量中，能看到每个别名都对应真正的门面类。

		在Illuminate\Support\Facades目录下的门面类实现了Facade基类。

		use Cache;
		class PhotosController extends Controller{
			public function index(){
				Cache::get('photos');
			}
		}
		Cache::get()
		1. 通过Cache别名找到门面类，在config\app.php文件的aliases属性能查看到。
		2. 该门面类实现了Facade基类的接口，这个接口返回服务容器绑定的名称。
		3. Cache::get()执行的静态方法，laravel会从服务容器解析被绑定的cache(在服务提供者绑定的对象)对象，并通过该对象执行被请求的方法。

	模拟一个类，提供一个静态方法，并将该静态方法映射到真正的方法上。


5. 请求的生命周期
	public/index.php，该文件是所有请求的入口文件。
	1. 
		加载bootstrap/autoload.php文件，将Composer产生的自动加载器进行载入，
	2.
		加载botstrap/app.php文件，实例化应用程序(服务器容器)的对象。
	3. 
		请求会被http核心或终端核心处理，视请求的类型：http核心处理http请求，终端核心处理命令行请求。
		http核心继承自Illuminate\Founation\Http\Kernel类，


	加载Composer自动加载器 
	=> 实例化服务器容器对象 
	=> http核心处理请求，定义启动器数组、中间件数组，在处理请求之前执行，最后handle()方法接受一个Request并返回一个Response()。

6. 目录结构
	根目录
		app目录：包含应，例如空程序核心代码。
		bootstrap：包含框架启动文件和自动加载配置的文件。
		config：配置文件目录。
		database：包含数据库迁移文件与数据库填充文件
		public：入口文件，资源文件(css/jss/图片)等。
		resource：视图，原始资源文件
		storage：包含编译后的Blade模板，基于文件的session、文件缓存或者其他框架产生的文件。
		vendor：包含Composer依赖的模块。
		tests：自动化测试

	app目录
		应用程序的内容存放于app目录下，默认情况下，这个目录在App命名空间下并通过Composer使用PSR-4自动加载标准自动加载。