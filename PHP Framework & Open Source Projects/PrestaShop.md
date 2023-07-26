应用是如何初始化的？

路由是如何进行处理的？

页面是如何进行渲染的？

hocks是如何应用在项目中的？

PS的类加载原理？

















分析

Admin/ProductController

```sql
SELECT SQL_CALC_FOUND_ROWS 
	p.`id_product` AS `id_product`, 
	p.`reference` AS `reference`, 
	sa.`price` AS `price`, 
	p.`id_shop_default` AS `id_shop_default`, 
	p.`is_virtual` AS `is_virtual`, 
	pl.`name` AS `name`, 
	pl.`link_rewrite` AS `link_rewrite`, sa.`active` AS `active`, 
	shop.`name` AS `shopname`, 
	image_shop.`id_image` AS `id_image`, 
	cl.`name` AS `name_category`, 0 AS `price_final`, 
	pd.`nb_downloadable` AS `nb_downloadable`, 
	sav.`quantity` AS `sav_quantity`, 
	IF(sav.`quantity`<=0, 1, 0) AS `badge_danger` 
	FROM `ps_product` p
	LEFT JOIN `ps_product_lang` pl 
		ON (pl.`id_product` = p.`id_product` AND pl.`id_lang` = 1 AND pl.`id_shop` = 1) 
	LEFT JOIN `ps_stock_available` sav 
		ON (sav.`id_product` = p.`id_product` AND sav.`id_product_attribute` = 0 AND sav.id_shop = 1 AND sav.id_shop_group = 0 )
	JOIN `ps_product_shop` sa
		ON (p.`id_product` = sa.`id_product` AND sa.id_shop = 1) 
	LEFT JOIN `ps_category_lang` cl 
		ON (sa.`id_category_default` = cl.`id_category` AND cl.`id_lang` = 1 AND cl.id_shop = 1) 
	LEFT JOIN `ps_category` c 
		ON (c.`id_category` = cl.`id_category`) LEFT JOIN `ps_shop` shop ON (shop.id_shop = 1) 
	LEFT JOIN `ps_image_shop` image_shop 
		ON (image_shop.`id_product` = p.`id_product` AND image_shop.`cover` = 1 AND image_shop.id_shop = 1)
	LEFT JOIN `ps_image` i 
		ON (i.`id_image` = image_shop.`id_image`) 
	LEFT JOIN `ps_product_download` pd
		ON (pd.`id_product` = p.`id_product`)
	WHERE (1 AND state = 1) ORDER BY `id_product` desc LIMIT 0, 20 ;
```



- 在代码中可以看到PrestaShop对搜索参数做了缓存？

  为了保证用户重新进入商品目录页面时，能够恢复到用户的上一次操作。





搜索参数是怎么处理的？

这些参数是怎么应用到SQL查询中的? 不需要进行SQL过滤吗?

搜索参数被封装为一个表单，在提交表单时，会将表单字段转换为SQL条件。这一步是如何处理的? 



twig的render函数可以直接调用控制器的action?

















```
编写一个商城系统，使用原始的技术栈。后端：symfony，前端：semantic-ui

- 分析PrestaShop是如何实践领域驱动架构的：
	很好的示例：
	PrestaShopBundle\Controller\Admin\Configure\AdvancedParameters\PerformanceController
	不好的示例：
	PrestaShopBundle\Controller\Admin\ProductController

- 分析产品模块
	先研究下后台的产品模块是如何实践领域驱动设计的。
	新建产品、更新商品、查询产品、

定义了一个接口，再用一个类去装饰接口？
查看下模板是怎么应用的，查看不同参数的意义。


ProductController::catalogAction()

	
	
- 分析用户模块
- 分析结账模块是如何实现的


















Symfony CURD
SyliusResourceBundle、配合GridBundle快速CURD：支持一对多，复杂查询。



- 查看文档：https://docs.monofony.com/current/bdd/phpspec/how-to-design-entities-with-phpspec，完成demo
- 查看下doctrine组件，了解下entity、repository，一对多的存储关系。
- 把自定义的部分翻译完成。
- 查看ResourceComponent组件、Product组件的用处
- 查看CoreBundle、ProductBundle俩个bundle的用处。
- 查看SyliusCURDBundle

问题1：
- 一个Form表单涉及到了多个对象，ResourceBundle是怎么处理的?
- 在routes.yaml文件中定义的resource路由，与symfony的路由有什么关系?
- knp_menu是什么组件? 有什么用?

- GridBundle是什么bundle? 有什么作用?
- CoreBundle
- ResourceComponent组件

- twig中的block和include有什么作用和区别？

关于ResourceBundle：
https://archiv.pehapkari.cz/blog/2018/04/25/sylius-resource-bundle-how-to-develop-your-crud-apps-faster/


```

























代码大全

重构：改善代码的既有设计

Review of the Refactoring: Improving the Design of Existing Code by [Martin Fowler](https://martinfowler.com/), 2nd edition (2018).

Review of the Advanced Web Application Architecture：https://archiv.pehapkari.cz/blog/2020/08/21/review-architecture/

捷克语的PHP论坛：https://archiv.pehapkari.cz/blog/



























CSS进阶

https://xiedaimala.com/courses/003b1951-22af-4821-ad80-d2880c0074eb/random/e2d3279591?#/common

JavaScript/JS深入浅出

https://xiedaimala.com/courses/24f54465-854f-4de7-9808-72a0bf5b3181/random/88b5baebd6?#/common

Vue2 UI造轮子（完整版）

https://xiedaimala.com/courses/6d63da67-6eea-4711-aeb4-0c3a949341dc/random/15de4e6ec0?#/common

阶段二：前端精进JS+Node+Webpack+性能优化+TS等

https://xiedaimala.com/courses/8757eca2-d1b0-4149-b196-4681670ea275/random/ca3220bd9b?#/common

ES6进阶

https://xiedaimala.com/courses/12a78a03-35f9-42ea-9b37-540540460f6e/random/331b4fc417?#/common



JustCC：https://justcc.mengkang.net/#/

Linux C编程一站式学习
