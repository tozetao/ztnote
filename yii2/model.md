

模型的一些概念

- 场景

  定义在某种场景下要验证的属性以及要块赋值的属性。

- 块赋值

  块赋值会将输入的属性填充到Model::attributes属性中，块赋值只会应用在模型当前场景中列出的安全属性中。

- 安全属性

  块赋值只会应用在模型的当前场景声明的安全属性上。yii2提供了default场景，默认实现是将Model::rules()中声明所有属性和数据作为安全属性。

  可以使用safe将属性声明为安全属性，它将不会被验证。也可以在rules或scenarios()方法中通过!将一个属性设置为不安全属性，不安全属性必须手动赋值。





内连接与外连接的区别

- 内连接查询

  主表与从表匹配查询时，如果条件不匹配那么主表的记录不会出现在查询结果中。

- 外连接查询

  主表与从表匹配查询时，即使条件不匹配，主表的记录也是会出现在查询结果集中的。

在连接查询时，即使条件不匹配也希望保留主表记录，那么使用外连接，否则使用内连接。



ORM的连接查询

ORM的连接查询本质是要建立主对象与关联对象的关系，即外键与主键的关系，本质上的实现是使用外键与主键的关系，将关联对象挂载到主对象上面



缺陷1：

由于ORM连接查询的实现，如果是在主表上做聚合统计，缺少主键与外键的关系时，是无法建立ORM关联关系的，即使建立关系，查询的数据也是错误的。

```php
BarAccount对象
    bar_id, cny

Bar对象
    
//如果对BarAccount使用bar_id分组再进行聚合统计，是可以使用ORM关系做映射的，如果没有的话，则无法设置。
```

针对这种情况你需要分别查询，再进行setter注入连接查询的模型对象

缺陷2：

a表关联b表时，如果on的查询条件有多个的话，那么从表的数据挂载到主表时，是不正确的。

例如主表的id、time字段要与从表的id、time字段一致时，on a.id = b.id and a.time = b.time，这种情况就无法使用关联查询了。



with()

连接查询附加查询条件，在调用with()方法时，使用匿名构造函数附加查询条件



joinWith()

连接表查询时，希望能加载关联对象的时候可以使用joinWith()。

连接表的时候，on条件可以指定多个查询条件，例如：

```php
return Customer::find()->joinWith([
        'orders o' => function(ActiveQuery $query){
            $query->onCondition(['o.status' => 1]);
        }
    ])->all();
```
