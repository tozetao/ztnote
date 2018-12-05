### SKU设计

一个商品拥有多个销售属性，例如颜色、尺码等等。

用户在后台创建一个商品时，会先选择类目，一个类目可以确定一个商品拥有什么销售属性。用户在类目的销售属性上进行选择，由此构成了商品的SKU。

销售属性与销售属性的值是一个一对多关系，例如尺码销售属性会有大、中、小等多个值，因此需要俩张表，一张是类目的销售属性表，一张是销售属性值表。

- category_attr：销售属性表
- category_attr_val：销售属性值表

```sql
-- 类目
create table category();

create table category_attr(
    cate_id int comment '类目id',
    attr_id int comment '属性名的id',
    attr_name varchar(30) comment '属性名',
    is_required tinyint comment '是否必选',
    has_file tinyint comment '是否可有附件'
);
/*
销售属性表，比如1001类目是T恤类目，拥有以下销售属性。
1001 10 颜色 1 1
1001 11 尺码 1 0 
*/
create table category_attr_val(
    attr_id int comment '属性名的id',
    attr_val_id int comment '属性值id',
    attr_val_name int comment '属性值默认的名称，如果该属性值的名字不可修改',
    be_modified tinyint comment '该属性的值是否可以修改'
);
/*
销售属性值表，表示一个销售属性拥有多个值，例如尺码有大、中、小。
10 101 白色 1
10 102 橘色 1
10 103 灰色 1
11 201 S   1
11 202 M   1
11 203 L   1
*/
```



商品的sku是由不同的销售属性组成，因此需要一张sku表，来记录商品的sku；也需要一张商品销售属性表，来记录每条商品sku对应的销售属性和销售属性值。

- goods_sku
- goods_attr

```sql
create table goods_sku(
    id int primary key auto_increment,
	goods_id int not null,
    price int unsigned default 0 comment '价格',
    inventory int unsigned default 0 comment '库存',
    attrs varchar(100) not null comment '属性id的集合，以,分割'
);
/*
attrs是属性的集合，一条sku是由不同的销售属性组成的。
以下是9001商品的sku示例。

9001 35 100 1,4
9001 35 100 1,5
9001 35 100 1,6
9001 35 100 2,5
9001 35 100 3,6
*/

create table goods_attr(
    id int primary key auto_increment,
    goods_id int not null,
    sku_id int not null,
    attr_id int comment '属性的id',
    attr_name varchar(30) comment '属性的名字',
    attr_val_id int comment '属性值的id',
    attr_val_name varchar(30) comment '属性值的名称'
);
/*
 1 9001 10 颜色 101 白色
 2 9001 10 颜色 102 橘黄
 3 9001 10 颜色 103 灰色
 4 9001 11 尺码 201 S码
 5 9001 11 尺码 202 M码
 6 9001 11 尺码 203 L码
*/


-- 商品表
create table goods(
    id int unsigned primary key auto_increment,
    name varchar(60) not null,
    description varchar(1000) not null,
    main_img varchar(100) not null
);

-- 订单表
create table goods_order(
    id int primary key auto_increment,
    user_id int not null,
    order_id int not null comment '订单id',
    user_address_id int not null comment '收地地址id',
    
    title varchar(60) not null comment '商品标题',
    unit_price int not null comment '商品单价',
    all_price int not null comment '商品总价',
    nums int not null comment '购买数量',
    attr_vals varchar(60) comment '商品购买属性值的集合，以,划分，例如M,黄色',
)

-- 收货地址
create table user_address(
	id int primary key auto_increment,
    user_id int not null,
    province_id int not null,
    procince varchar(30) not null,
    city_id int not null,
    city varchar(30) not null,
    area_id int not null,
    area varchar(30) not null,
    info varchar(100) not null comment '详细地址'
);
```





购物设计流程

购买：在商品页面上有购买按钮，用户选择好商品的规格后点击按钮进行购买。

订单预览：在这一步会生成订单的一系列信息，同时让用户选择快递类型、送货地址、优惠券等等信息。

下单：用户确定订单信息后点击购买，这一步才生成订单。

付款：用户进行付款，订单状态修改为已支付，然后由商家来处理。



下单接口实现

前端传递的数据：sku_id、购买属性值、用户的收货地址id、使用的优惠券、选择的快递等数据。

后台：根据sku_id作一系列处理，例如查询商品基本信息，查询sku库存是否充足等等。



假设是下单减库存，我们的逻辑比较简单，所以可以在内存中进行控制。

生成订单数据 => 减少sku库存





如何减库存？

下单减库存。

付款减库存。

预扣库存。





减库存的优化思路

如果sku不复杂，在内存数据库中减库存。

如果有很复杂的减库存逻辑，或者必须使用事务，仍然是必须在数据库中完成减库存。

同一数据在数据库中肯定是一行存储，例如MySQL，因此会有大量线程来竞争Innodb行锁，而并发度越高时等待线程越多，TPS（Transaction Per Second）下降，RT会上升，数据库的吞吐量就会严重收到影响。

而这一会引发一个单点问题，就是单个热点商品会影响整个数据库的性能，导致1%的商品影响到99%的商品的售卖。一个解决的问题是进行热点隔离，把热点商品放到单独的热点库中，但是会带来维护的问题，比如热点数据的动态迁移以及单独的数据库等。

同时分离热点商品到单独的数据库也无法解决并发锁的问题，要解决并发锁的问题有俩种办法：

- 应用层做排队

  按照商品维度设置队列顺序执行，这样能减少同一台机器对数据库同一行记录进行操作的并发度。

  同时也能控制单个商品占用数据库连接的数量，防止热点商品占用太多的数据库连接。

- 数据库层做排队

  应用层只能做到单机的排队，如果应用机器很多，这种排队控制并发的能力仍然有限，所以如果能在数据库层做全局排队是最理想的方式。

  阿里团队开源了该补丁程序，可以在数据库层上的单行记录做到并发排队。

排队与锁竞争？

InnoDB内部的死锁检测，以及MySQL Server端与InnoDB引擎切换会比较消耗性能，因此采用排队的方式性能会好很多。

除了热点隔离与排队处理之外，某些场景的更新非常频繁（例如商品的lastmodifytime字段），在这些场景下多条SQL语句是可以合并的，只需要执行最后一条SQL就可以了，以减少对数据库的更新操作。