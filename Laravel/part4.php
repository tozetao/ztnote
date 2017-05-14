<?php
/*

1. DB对象
	命名空间：\DB。
	该对象能通过原生SQL语句、查询构造器来操作数据库。

2. 查询构造器
	Query Builder，以调用方法的形式来构造sql语句，进而操作数据库。

	2.1 Builder实例
		DB::tables(table_name)
			通过DB对象获取Illuminate\Database\Query\Builder实例，该对象提供了查询构造器的所有操作。
	
	2.2 方法
		查询
		新增
		更新
		删除

		聚合函数

		条件判断

		外连接

3. Eloquent
	4.1 ORM
		Object-Relational Mapping(对象关系映射)，它的作用是在关系型数据库和业务实体对象之间作一个映射，这样我们在操作具体的业务对象时，就不需要再去和复杂的sql语句打交道，只需要简单的操作对象的属性和方法。
		ORM实现方式：ActiveRecord：模型与数据表一一对应；DataMapper：模型与数据表完全分离。
		
		laravel的Eloquent ORM是通过ActiveRecord来实现的，每个Eloquent模型类都对应数据库中的一张表，通过模型类的方法实现对数据库的增删改查。

	4.1 Illuminate\Database\Eloquent\Model
		Eloquent模型继承自Model类，模型类与数据表对应，每个模型实例对象则对应一条具体的记录。
	
	4.2 查询
		查询、条件查询、连接查询、聚合查询

		all()
			取出所有记录。

		find()

		where()

		whereRaw()

		count()

		chunk()
			拆分查询，适用于大批量查询结果，不会消耗大量内存。

	4.3 批量赋值
		$fillable
			白名单，定义了允许批量赋值的属性，不可与$grarded重复定义

		$guarded
			黑名单，定义了批量赋值要过滤的属性，不可与$fillable重复定义。

		create()
			能批量赋值并插入数据库，这里的批量赋值是指能接受一个数组(表单对象)来创建模型并自动赋值，再插入数据库。

	4.4 新增、更新
		save()
			将模型插入数据库，如果模型有具体的id，则会更新该模型记录。

		update()

		目前，都是通过条件查询再进行更新，后续再测试。
		注：如果有updated_at字段，会默认更新的。

	4.5 删除、软删除
		
	4.6 一些属性
		$primaryKey
			Eloquent默认的主键名称是id，可以自定义。

		$table
			Eloquent默认数据库表名是类名称小写复数形态，例如User模型对应users表。

		$timestamps
			默认数据库表需要有updated_at和created_at俩个字段，如果不需要设定这俩个字段，将值改为false即可。

		注：需要了解的是每一个Eloquent模型本身都是一个查询构造器，查询构造器的所有方法Eloquent模型都能调用，只不过需要使用静态方法来调用。

	4.2 Collection
		Eloquent模型返回的都是Illuminate\Database\Eloquent\Collection的一个实例，该类实现了ArrayAccess接口，该接口的实现使我们可以像访问数组一样遍历该对象。
	
	4.3 分页对象
		Illuminate\Pagination\Paginator
		Illuminate\Pagination\LengthAwarePaginator

		封装了分页的一些属性和接口。

		paginate($perPage = null, $columns = ['*'], $pageName = 'page', $page = null)
		perPage：每页显示数组
		columns：要查询的字段
		pageName：页面名称
		page：第几页

		example：
			user是orm对象，即Eloquent模型对象。
			User::paginate()
				返回一个Illuminate\Pagination\LengthAwarePaginator实例。
				封装了分页的相关信息和接口，并实现了Interator接口，可以通过foreach遍历获取数据结果集。
			
				在调用该方法之前可以添加查询条件，Paginator实例对象会在服务器存储你的查询条件的。

			User::simplePaginate()
				将返回一个Illuminate\Pagination\Paginator实例

4. 结构生成器
	提供一个与数据库无关的数据表产生方法。	

5. 关联
	parent model
		主模型

	related model
		关联模型

	5.1 基本
		简单的如一对一、一对多、一对多，这些并没有特殊的地方。
		hasOne
		belongsTo($related, $local_key)
			一对一的相对关联

		hasMany()
			一对多的关联

		belongsToMany()
			多对多

	5.2 远层一对多关联
		可以经由多层间的关联取得远层的关系，一对一对多。
		例如通过一个contry模型通过user模型关联到很多post模型。
		结构：
			contry
				id
				name

			user
				id
				name
				contry_id

			posts
				id
				name
				user_id
		
		hasManyThrough($related, $through, $fistKey, $secondKey, $localKey)
			参数：
				$realted：最终关联的对象，这里是posts。
				$throuth：中间者对象，这里是user。
				$firstKey：第一个外键，即中间者对象的关联字段，如果不填写该参数，将根据contry自动生成。
				$senondKey：最终关联对象的外键，该参数可以不填写，将根据中间者对象名自动生成。

	5.3 多态关联
		多态关联，允许一个模型同时关联多个模型。
		例如让photos模型关联orders和staff模型，主要是在一的模型一方增加俩个字段，一个id，一个type。
		type指定了要关联的模型对象类型(即命名空间)，type决定了关联模型的类型，所以叫做多态，id指定了模型在表中的id，

		表结构：
			staff
				id
				name
			orders
				id
				name
			photos
				id
				path
				imageable_id
				imageable_type

		staff、orders都拥有多个photos记录。
		
		Staff/Order模型
			morphMany(string $related, string $name, string $type, int $id, string $localKey)
				说明：
					定义一个一对多的多态关联关系
				
				参数：
					$related：要关联模型的类名，例如这里是App\Photo。
					$name：指定关联模型的关联字段前缀。

					$type
						关联模型的type字段

					$id
						关联模型的id字段

					$localKey：主模型(主表)的主键，案例这里是Staff/Order的主键，也能指定要对比的字段名。

		Photo模型
			morphTo()
				定义一个多态关联，反转的一对一或一对多的关联关系。
				使用这个方法的方法名用来作为id和type的前缀，例如：
				function imageable(){
					return $this->morphTo();
				}
	
	5.4 多对多的多态关联
		即通过中间表的type字段，来实现多态的多对多。
		例如：
			post(文章)可以有多个tag，而一个tag也可以拥有多个post(文章)。
			videos也可以有多个tag，一个tag也可以拥有多个videos。

		表结构
			post（左表）
				id
				name
			videos（左表）
				id
				name
			tags（右表）
				id
				name
			taggables
				tag_id
				tagable_id
				tagable_type

		morphToMany($related, $name, $table, $foreignKey, $otherKey, $inverse)
			$related
				关联的模型对象
			
			$name
				中间表外键id字段、type字段的前缀。ORM根据type、id来查询出的记录来查询右表的记录。

			$talbe
				中间表的表名
			

			左表模型使用的方法。
			$this->morphToMany('App\Models\Tag','taggable','taggable','taggable_id','tag_id',false);

		morphByMany()
			右表模型使用的方法。
			$this->morphedByMany('App\Models\Video','taggable','taggable','tag_id','taggable_id');
	
	5.5 预载入
		用于减少N+1查询问题，例如一个book模型会关联到一个Author模型。
		定义：
			class Book extends Author{
				function author(){
					return $this->belongsTo('App\Author');
				}
			}

		如果你定义了一个相对的一对一查询，在获取其中一方模型对象的集合，遍历这个集合获取关联对象的数据，就会产生N+1条sql语句：
		foreach(Book:all() as $book){
			echo $book->author->name;
		}

		with()
			预载入你要关联的对象，减少N+1查询，也可以使用闭包增加预载入的查询条件。

		load()
			通过Collection对象预载入关联对象，这对于需要根据情况是否载入关联对象的时候，或是跟缓存一起使用时很有用。
	
	5.6 关联模型的更新
		新增关联模型
			在一对一、一对多的关联关系下，要新增一个关联模型，只需要使用save()方法即可。

		Belongs To(从属关系模型)
			要更新belongsto关联时，使用associate()方法，这个方法会设置子模型的外键。
			注：associate()属于belongsTo对象。

		新增多对多模型
			belongsToMany类的方法

			attach()
				归属、附加。
				附加一个多对多的关联模型，关联关系会存储到中间表中。

			detach()
				删除一个多对多的关联模型。

			sync($array)
				附加一个以上的多对多关联。
				$array是一个关联模型的id数组，关联关系会存储到中间表中，同时删除$array中不存在的关系。

		更新上层时间戳
			当模型belongsTo另外一个模型的时候，比如Comment属于一个Post，如果能在子模型被更新的时候，更新上层的时间戳。
			
			Model::touches
				将方法增加到属性中即可。
			$touches = ['post'];

		操作中间表
			belongsToMany::piovt，该属性包含了关联模型所对应中间表的记录。
	
	5.7 集合
		所有Eloquent查询返回的数据，如果结果多余一行，都会转换成集合对象返回，该集合对象实现了IteratorAggregate接口。

		遍历、过滤、
	
	5.8 日期转换器
		Eloquent默认会把created_at/updated_at字段转化成Carbon实例，该对象继承自php的DateTime类。
		
		Model::getDates
			该方法设置模型哪些字段需要被转换，自己重写。
	
	属性类型转换

	模型事件

	模型观察者

	模型转换


6. 数据库迁移与填充
	5.1 数据库迁移
		迁移是一种数据库的版本控制，通常与结构生成器一起使用，可以简单的管理数据库表结构。

	5.2 数据库填充
		简单的使用seed类，通过查询构造器或ORM填充测试数据到数据库。

7. 配置文件
	config/database.php
	laravel的数据库配置文件。
	
	fetch => PDO::FETCH_CLASS
		默认返回结果集为php对象。
	
	default
		默认的数据库连接

	connections
		更详细的配置信息

	migrations
		迁移表的名称

	redis
		Redis数据库相关配置

	.env配置文件
		如果需要修改数据库配置信息，在该文件修改即可，位于根目录下。



php artisan make:migration create_users_table
	创建迁移文件
	--table：指定数据库表名，不创建表

	--create：创建新的表

php artisan migrate
	执行一次迁移

php artisan migrate:reset
	回滚所有迁移

php artisan migrate:refresh
php artisan migrate:refresh --seed
	回滚所有迁移并且再执行一次


php artisan db:seed
	默认 db:seed 命令会执行 DatabaseSeeder，可以使用它来调用其他 seed 类

php artisan db:seed --class=UserTableSeeder
	指定要执行的填充类


make:controller
make:model 