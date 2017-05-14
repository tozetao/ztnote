<?php
/*

需求分析
--------
1. 订单流程分析
	a. 用户预约
		Index/Queue/getDesk
		产生订单、用户进入列队中。

		每种规格的订单，用户只能预约一次，在订单结束后才能继续预约。：这里条件判断

	b. 商家叫号
		叫号的时候推送信息，并显示 当前叫号号码，上一次叫号号码。
		防止重复点击：

	c. 用户用餐
		防止重复点击：
		Admin/Queue/updateOrder
		用餐的时候，需要更新用户的订单，根据商户id、叫号数字来查询订单。
		需要更新队列，将用餐的叫号数字移除队列，将当前叫号置0.
	
	d. 取消订单
		Admin/Queue/outOrder
		将用户的订单取消，

	d. 完成支付
		用餐

	其他要求：
		订单有大、中、小3种规格，
		后台设置用餐的时候，要开始计时。
		支付成功后，对商家进行评价 并 更改订单状态，订单所对应的员工增加相应的消费。
		叫号要推送信息
	
	wait_s：等待中的队列

	get_s：当前取号

	last_s：最后一次叫号

	now_s：当前叫号

2. 用户功能接口

3. 商家管理
	1. kv管理
		查看kv
		增、删、改

	2. 员工管理
		查看每位员工获得的小费（员工字段：头像、姓名、性别、职位、店铺、工号、学历、可提现小费、身份证、电话）
		增、删、改

		添加员工少了俩个字段，回头加上。
	
	3. 会员管理
		会员的增删改

	4. 排队管理
		详见订单流程分析

	5. 菜品管理
		查看菜品，增、删、改

7. 管理员管理
	菜品类型管理
		查看菜品类型，

	广场信息排序
	查询各商家的总收入
	统计总商家数、总销售额、订单数

*/




/*
常用测试接口
登录
http://www.oneym.cn/tocheng/wmindex.php?m=Login&a=login&name=18620115550&pwd=123456
D01kfy1loBCdOME

叫号
http://www.oneym.cn/tocheng/wmindex.php/Queue/getDesk?merid=1&token=D01kfy1loBCdOME&type=s

后台
http://www.oneym.cn/tocheng/admin.php?m=Index&a=index#

登陆
http://www.oneym.cn/tocheng/wmindex.php?m=Login&a=login&name=18620115550&pwd=123456
 */

