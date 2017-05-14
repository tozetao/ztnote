<?php
/*


 1. 统一的微信接口调用，按照微信接口的类别来进行类的划分。
    不同token接口
    
    WxUserApi
        提供关于微信用户的相关接口
        getOpenId()
            获取微信用户openid

        getWxUser()
            获取微信用户信息
    
    WxTokenApi
        getAccessToken()
            返回全局token

        getJsTicket()
            获取js接口的ticket

    JsApiPay
        微信网页h5页面支付类


    WxHandler接口
        每种微信请求的消息类型有对应的对象来处理，这些对象都实现了一个WxHandler接口
        responseMsg($postObj)

2. 微信支付
    1. 统一下单接口
        除被扫支付场景以外，其他支付场景都是去请求 统一下单接口 生成预支付交易单，返回预支付标识。
    
    2. 调用场景接口
        在返回正确的预支付标识后，才能按照不同场景生成交易串 发起支付。

    3. 支付结果通知
        支付结果通知接口的设置是在 统一下单提交的notify_url参数设置的。

        支付完成后，微信把支付相关结果发给商户，商户要正确应答。

        注意1：如果商户应答失败或者超时，微信认为通知失败后，会间隔一定时间发起通知，所以商户要能正确的处理微信的多次重复通知，
        
        注意2：建议采用数据锁进行并发控制

        注意3：对于支付的结果要做签名验证，防止数据泄露造成假通知。
    
    4. 统一接口参数
        body：商品描述
            广州小跑xx元充值

        out_trade_no
            商户订单号
            商户支付的订单号由商户自定义生成，微信支付要求商户订单号保持唯一性。
            （建议根据当前系统时间加随机序列来生成订单号）。

            重新发起一笔支付要使用原订单号，避免重复支付；已支付过或已调用关单、撤销（请见后文的API列表）的订单号不能重新发起支付。 
        
        total_fee
            总金额

        time_start
            交易起始时间

        time_expire
            交易结束时间

        attach
            商户数据包，用于携带商户自身定义的数据。

        goods_tag
            商品标记，代金券或立减优惠功能的参数

        notify_url
            通知地址

        trade_type
            交易类型

    5. 支付结果通知参数
        {
            "appid": "wx7f4ee8e24acdd02e",
            "attach": "小跑充值订单",   商家数据包，与请求一致
            "bank_type": "CFT",
            "cash_fee": "1",
            "fee_type": "CNY",      货币类型
            "is_subscribe": "Y",
            "mch_id": "1274701301", 商户id
            "nonce_str": "1pr5vkou3d6scy4zmkwlzwz1yvdmrf1a",
            "openid": "oEddBuA1_7gXrL6yIyhWdpcrh3vM",   用户openid
            "out_trade_no": "127470130120160406220057", 商户系统的订单号，与请求一致。
            "result_code": "SUCCESS",
            "return_code": "SUCCESS",
            "sign": "A12160C0C54856C33254E22351DD20B1", 签名，详见签名算法
            "time_end": "20160406220121",
            "total_fee": "1",                           总金额
            "trade_type": "JSAPI",                      场景类型
            "transaction_id": "4005932001201604064607352390"
        }

    6. #微信用户的支付信息记录表
    wx_user_payinfo(
        id int unsigned primary key auto_increment,
        mch_id int not null,
        openid varchar(128) not null,
        out_trade_no char(32) not null,
        attach varchar(128) not null,
        total_fee decimal(8,2) not null,
        trade_type varchar(16) not null
    )

    查询商户订单号，看是否存在。

    如果存在，证明已经处理过了，直接响应成功即可。
    如果不存在，那么插入支付记录，同时更新用户余额，再响应成功数据

3. 消息类型
    MsgType
        text，文本
        image，图片
        voice，语音
        video：视频
        shortvideo：小视频消息
        location：地理位置
        link：链接消息
        event：事件消息

    事件消息类
        Event：事件的类型
            subscribe(订阅)、
            unsubscribe(取消订阅)、
            CLICK(自定义菜单事件)
        EventKey
            事件的key值


命名规范
    采用驼峰命名
    对象、类、方法、首字母大写，
    属性、变量、首字母小写

整合ide环境

微信项目改进
    用户状态的保持
    开发接口的定义


微信部署注意点
    id和key
        appid
        appsectet
    
    微信服务器接口配置

    js回调接口

    js安全域名设置
        www.oneym.cn
        oneym.cn

    项目中的url更改、文件夹更改



支付的一系列设置
    1. 支付目录设置
        分为支付授权目录和测试目录
        注：
            正式目录和授权目录不能一样，否则报错，并且测试目录必须是发起支付的页面的精确目录，子目录下无法正常调用支付。
            例如：http://www.oneym.cn/wxshop/wxindex.php/WxPay/
            WxPay是TP框架的Action
    
    2. 证书下载

