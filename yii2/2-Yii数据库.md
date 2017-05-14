## Yii与数据库

## Model


## DAO
Yii通过数据库访问对象(DAO，database access objects）来使用数据库的，DAO建立在PHP数据对象PDO之上，提供了一套面向对象的API来访问数据库。

在Yii2框架中，DAO的实现要注意以下俩个类：
- yii\db\Connection，代表了数据库连接对象
- yii\db\Commond，用于执行SQL语句

### yii\db\Connection
在config目录下的配置文件中，db选项配置了数据库的连接方式，这里yii/db/Connection对象负责读取配置信息并连接数据库，该连接对象以Yii::$app->db来进行访问。

连接不同的数据库主要是更改db选项中dsn的值，例如：
- MySQL/MariaDB：dsn => mysql:host=localhost;dbname=mydatabase
- SQLite: dsn => sqlite:/path/to/database/file
- 其他看手册

### yii\db\Commond
用SQL语句来创建一个yii/db/Commond对象，调用对象的方法来执行SQL查询。
调用的查询方法决定返回的数据是一维数组或二维数组，例如：
```php
Yii::$app->db->createCommond('select * from post')->queryAll();

# PDO绑定参数的写法
Yii::$app->db
	->createCommand('select * from post where id=:id')
	->bindValue(':id', $_GET['id'])
	->queryOne();
```
Commond对象操作数据库的优点：
- 简单，只需要处理SQL语句和数组
- 高效，通过SQL语句来查询数据库非常高效

缺点：
- 不同数据库的SQL语句会有所差别，无法做到代码适用于多种数据库系统
- 用数组而没有用对象来管理数据，代码难以维护
- 如果不小心会被SQL注入


## QueryBuilder
查询构建器也是建立在DAO对象基础之上，能让你创建程序化的创建SQL语句，就是说你不需要自己写SQL语句，调用方法构建语句，所以该类才叫做QueryBuilder。

### 使用方式
- 构建查询，构建一个yii\db\Query对象，通过调用类似select、orderBy、where等方法，构建出一条SQL语句
- 执行查询，执行all()、one()、column()等方法来查询数据

```php
# 构建查询
$row = (new \yii\db\Query())
	->select('id,email')
	->from('user')
	->where()
	->addParams()
	->orderBy('id')
	->limit()

# select案例
$query->select('user.id as uid, email')
$query->select('user.id as uid, email') #别名

# 执行MySQL函数
$query->select(["concat(first_name,'', last_name)", 'email'])

#子查询
$subQuery = new Query()->select('count(*)')->from('user');
$query->select(['id', 'count'=>$subQuery])->from('post')		

```
indexBy()，QueryBuilder查询数据是以二维数组返回，该数组索引是0开始的，indexBy方法能够指定查询的字段作为key来返回数据。

column()，查询某个列的值



## ActiveRecord
ActiveRecord（活动记录）提供了一套面向对象访问数据库的接口，特点：
- 一个ActiveRecord关联一张表，每个AR对象对应表中的一行数据。
- AR对象的属性对应表中的列
- 能以面向对象的方式来操纵数据库的数据，这样就不需要SQL语句就能实现对数据库的访问

ActiveRecord实际上是ORM的实现，ORM在快速开发下是非常便捷有用的。

### 创建
通过继承yii/db/ActiveRecord类来声明一个AR类，并实现一个tableName方法，返回与之对应表的名称。
```php
class Post extends \yii\db\ActiveRecord{
	public static function tableName(){
		return 'post';
	}
}
```

### 查询
- yii\db\ActiveRecord::find()

find()方法将返回ActiveQuery对象，继承自Query类，由于是以AR对象进行查询，所以查询结果是AR对象或AR数组。

```php
# 返回一条记录，$model代表着数据
$model = Post::find()->where(['id'=>1])->one();

# 返回多条记录
Post::find()->where(['status'=>1])->all();
```
find()方法实际是返回一个实现了yii/db/ActiveQueryInterface接口的实例对象，该对象既是yii\db\ActiveQuery对象，所以yii\db\Query实现多条件查询，ActiveQuery对象也能使用。

注1：ActiveRecord有俩个方法可以用于替代上述的方法，分别是：findOne()和findAll()，这俩个关键都是静态方法。
注2：在AR模型中，查询后返回的数据对象均实现了Interotar接口，所以PHP可以用foreach遍历它。


- yii\db\ActiveRecord\findBySql()
通过查询语句来创建ActiveQuery实例对象。


### CRUD操作
- yii\db\ActiveRecor::insert()
- yii\db\ActiveRecor::delete()
- yii\db\ActiveRecor::update()
- yii\db\ActiveRecor::save()

save()方法的说明，该方法会自动判断是否新增数据还是更新数据，估计是根据主键id是否存在来进行判断。

所以删除数据需要先进行find()，然后再删除。


### ActionQueryBuilder


## AR对象的生命周期
### save()的执行流程
1. beforeValidate()
该方法在数据验证之前执行，如果这个方法执行后返回false，后面的步骤将中断。
EVENT_BEFORE_VALIDATE，执行该方法的时候会触发该事件，可以捕捉该事件来注入代码，或者重写该方法。

2. 执行数据验证，失败则中断

3. afterValidate()
EVENT_AFTER_VALIDATE，执行触发的事件

4. beforeSave()
该方法是在保存数据之前的最后执行的方法，将会触发EVENT_BEFORE_INSERT OR EVENT_BEOFRE_UPDATE时间。

5. 执行数据插入或修改

6. afterSave
执行插入数据库后执行的方法，会触发EVENT_AFTER_INSERT OR EVENT_AFTER_UPDATE事件。保存好数据后可以进行尾部处理工作，标签的处理将会重写该方法。

### new()
new方法用来创建对象，会执行下列方法：
- constructor
- init()，该方法将触发EVENT_INIT事件。

### find()
该方法会用查询结果来创建对象，将执行以下方法：
- constructor()
- init()，触发EVENT_INIT事件
- afterFind()，EVENT_AFTER_FIND事件

### delete()
delete方法在真正执行的时候可以在前面重写代码：
- boforeDelete，将触发EVENT_BEFORE_DELETE事件
- 执行删除数据
- afterDelete， 将触发EVENT_AFTER_DELETE事件

### refresh
2.0.8版本才会有的方法，在执行完毕之后将会执行afterRefresh()方法，触发EVENT_AFTER_REFRESH事件。