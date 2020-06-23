account
sb-bncmz1741451@business.example.com

client id
AfHw2fSwsJ59Snzv95jkcFn0UTAqoeQssekuf7tVnybc9-iHzHQRLEf1hY9A0xj1X3Eit5GQLB_egjRj

secret
ELVO5AHDJjumaSx3YEAjCkJBanWoRis5eZW4SGH_aSLaIsQ-u_OnOE3KBr83HdABVMdbx8sMoDh0nsAl



business account
sb-47ckod1758111@business.example.com
K^B4bI?y

personal account
sb-4k54i1756694@personal.example.com
3y.=*%aY











1668765725@qq.com
ZCctit666





中国地区账号

sb-agkzh1728969@personal.example.com

YNt9gh@0













### 创建订单

> /v2/checkout/orders

创建一个订单。



request body

- intent

  在订单生成后立刻抓取订单或授权订单支付的意图。

  一种是CAPTURE，指的是商家打算在客户付款后立刻抓取付款信息；

  一种是AUTHORIZE，商家授权付款，在客户付款后资金搁置。不清楚这种订单的作用。

- purchase_units

  一系列购买单位，包含买家部分或全部订单。

- application_context

  在paypal支付审批过程中，定制买家的体验。

  return_url：客服批准付款后被重定向的URL

  cancel_url：客服取消付款后被重定向的URL



### 获取订单

> /v2/checkout/orders/{id}/capture

获取订单的付款。

若要成功获取订单付款，买房必须首先批准订单或在请求中提供有效的支付来源。

买家可以在重定向到rel：approve URL时批准订单，这里读不懂表达的意思。。。



### 订单的状态
CREATE：订单刚创建。

APPROVED：客户通过paypal钱包或者其他方式批准支付。

VOIDED：所有购买单位均作废。

COMPLETED：授权支付或支付授权的订单被获取，到这一步订单就支付完成。





## 接口设计

### todo

- 境外服务器，境外域名

- 怎么标识一个用户



支付分为创建订单和确认订单俩个步骤。



### 创建订单

请求参数：

- goods_id
- price

接口调用成功将返回订单id；如果失败前端需要跳转到错误页面 ，让用户重试。





### 确认订单支付

请求参数：

- order_id

用户支付成功后会发出请求，携带订单id给服务器。

服务器首先查询数据库验证订单：

- 判断订单是否存在
- 订单状态是否未支付

如果验证失败则响应错误信息；

如果订单存在且支付状态没问题，存在会向paypal发出请求，查询订单是否支付成功。

当支付成功时会尝试向机器发出发货通知，通知成功则响应成功信息。通知失败会进行若干次尝试，否则发出失败信息。由客服人员来进行处理。





```sql
create table orders(
    id int primary key auto_increment,
    order_id varchar(60) not null,
    goods_id int not null,
    quantity int not null comment '购买数量',
    total int not null comment '总价，单位分',  
    status tinyint not null comment '订单状态',
    created_at int
);
```





- 



fecmall

- fecmall整个请求是如何执行的，各个层之间是怎么调用的。
- 商城首页修改