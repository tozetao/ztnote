## 数据小部件
Yii提供了一些小部件widgets，这些小部件用于在视图中显示数据，常用的有：

- DetailView小部件用于显示一条记录数据
- ListView和GridView小部件能够用于显示	一个拥有分页、排序、和过滤功能的一个列表或者表格
 
### 1. DetailView小部件
该小部件主要用于显示一条记录的情况，例如：
- 一个Model对象的数据
- ActiveRecord类的一个实例对象
- 键值对构成的一个数组

DetailView::wiget()
该方法用于构建一个DetailView对象，参数是一个数组，以键model、attributes、templaate、options构成，
- attributes设置要显示的属性，并对能对属性的样式进行调整
- tempalte调节小部件中每一行的展示模板
- options设置整个表达的table标签属性
```php
DetailView::wiget([
	'model'=>$model,
	'attributes'=>[
		'id','title',content:ntext',
		'status',
		'update_time:date_time'
	]
]);

# 自定义列的名字，值的类型
DetailView::wiget([
	'model'=>$model,
	'attributes'=>[
		'id','title',content:ntext',
		'status',
		['label' => '状态', 'value'=>$model->postStatus->name],
		['attribute' => 'author_id', 'value'=>$model->author->name]
		'update_time:date_time'
	]
]);
```

### 2. GridView
该小部件是Yii功能最强大的小部件之一，非常适合用来建立系统管理后台，实例化如下：
```php
<?= GridView::widget([
	'dataProvider' => $dataProvider,
	'filterModel' => $searchModel,
	'columns' => [
		'id', 'title', ...
	]
]); ?>
```
- dataProvider：数据提供者对象，拥有GridView需要的数据
- filterMode：搜索模型类
- columns：包含要显示的列类

### columns
columns由各种列类组成，包括：
- 序号列：行号，从1开始并自动增长
- 数据列：指定模型字段的显示
- 动作列：显示动作按钮，如查看、更新、删除

### 序号列
序号列一般是用来显示序号，基本不用。
```
'columns' => [
	['class' => 'yii\grid\SerialColumn'],
	...
]
```

### 3. ListView
- 用于显示数据提供者DataProvider提供的数据
- 每条数据用一个指定的子视图文件来渲染，可以在子视图中自由的布置页面
- 由于有dataprovider提供数据，自然的集成了分页排序这些特性，可以方便的写出显示数据以及管理数据的界面



### 数据列
显示模型字段数据，能通过属性来控制显示的数据和样式，属性包括：

指定模型字段显示的数据和样式，属性包括：
- attribute，指定需要展示的属性
- label，标签名

- value，字段的值，value可以是string，也可以是一个function($model, $key, $index, $column)，函数参数分别是当前行模型、当前行的键、当前行的索引和列对象

- format，对值做格式化
- filter，自定义过滤条件的输入框
- comtentOptions，设置数据列HTML属性

```
'columns' => [
	'
]
```

### 动作列
显示动作按钮，如查看、更新、删除，动作列的属性有：
- template：定义动作列的按钮，大括号内括起来的就是按钮。
- buttons：一个按钮的渲染回调函数数组，数组中的键是按钮名，并且值是对应的按钮渲染回调函数。
- controller：指定执行动作的控制器ID，没有指定使用当前控制器。
```php
[
	'class' => 'yii\grid\ActionColumn',
	'template' => '{view}{update}{delete}{approve}',
	'buttons' => [
		'approve' => function($url, $model, $key){
			
		}
	]
],
```


## 自定义小部件
1. 从yii\base\Widget集成类
2. 重写yii\base\Widget::init()，方法中主要处理小部件属性
3. 重写yii\base\Widget::run()方法，该方法包含小部件生成渲染结果的代码
4. 渲染结果可在run()方法中直接打印输出以字符串返回。






## SearchModel(搜索模型)
一般模型与数据库表是对应的，在yii中模型不仅能对应表，也能对应视图中的Form表单。

SearchModel对应Form表单，它封装了搜索表单的字段，字段的验证规则，搜索方法等。一般SearchModel的父类是基础模型类，例如：

```php
class PostModel extend Model{}

class PostSearch extend PostModel{}
```

### 1. 属性与搜索表单对应
属性对应有俩点原因：
- 为了能够进行块赋值，
- 使用GridView小部件时，可以控制搜索表单字段的显示，不一致的属性是不会显示的，通过重写模型attributes()方法，能够定义要显示的字段。

```php
public function attributes(){
	return array_merge(parent::attributes(), ['authName']);
}
```

### 2. 数据规则重写
定义搜索表单字段的数据规则

### 3. 搜索实现
搜索实现是通过查询构造器来构建查询，然后交给数据提供者，在后续阶段执行查询

### 4. 排序实现
排序的实现是通过设置DataProvider的sort配置来实现的。

### 5. 表单字段的命名
以数组的形式命名，例如：
```php
<input type='text' name='PostSearch[title]'/>
```
这种命名方式能够非常方便的块赋值。


## 数据提供者(DataProvider)
数据提供者，封装分页信息、排序信息、查询对象来完成分页数据的查询。

功能大体如下：
- 可对获取到的数据进行分页和排序
- 经常用来给数据小物件提供数据，方便用户互动地进行数据的分页与排序
- 可以获取数据提供给其他组件页面来使用

```php
$dataProvider = new ActiveDataProvider([
	'query' => $query,
	'pagination' => ['pagesize'=>5],
	'sort' => [
		'defaultOrder' => ['id' => SORT_DESC]
	]
]);
```

该类实现了yii\data\DataProviderInterface，该接口定义了所有数据提供者的公共接口。


### 数据提供者的种类
根据功能大体分为：
- ActiveDataProvider，用yii\db\Query或者yii\db\ActiveQuery从数据库查询数据，以数据项或AR实例对象返回数据
 
- SqlDataProvider，执行一段SQL语句并将数据作为数据返回
- ArrayDataProvider，将一个大的数组依据分页和排序设定返回一部分数据


### DataProviderInterface
yii\data\DataProviderInterface，该接口定义了数据提供者类共同实现的方法，有：
- getPagination()
- getSort()
- getCount()
- getTotalCount()
- getModels()，取得DataProvider中的数据，有可能是对象数组，或者是普通数组，具体类型看数据提供者的类型。