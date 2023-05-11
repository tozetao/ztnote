

### API设计

以Restful API的设计风格来命名API。通过URI和HTTP动作来操作资源，示例：

- GET  /notifaications/statistics

    通知统计，比如查询用户未读的消息数量，HTTP动作使用GET，表示查询。

- PUT  user/read/notifications/:notification_id

    用户将消息标记为已读，PUT操作表示更新消息的状态。

- GET  /user/starred

    查询用户标记过的仓库

- GET  /user/starred/{owner}/{repo}

    检查用户是否标记过某个仓库

- PUT  /user/starred/{owner}/{repo}

    用户标记某个仓库，PUT表示更新动作

- DELETE  /user/starred/{owner}/{repo}

    用户取消某个仓库的标记，DELETE表示删除动作

- PUT /repos/:repo/issues/:number/lock

    锁定操作是一种资源，用锁lock来表示，PUT动作表示去更新它。

- DELETE /repos/:repo/issues/:number/lock

    删除github仓库中某个锁定的issue，使用DELETE操作来接触锁定状态。

- PUT topics/:topic_id/like

    点赞

大多数情况下我们访问的是资源的集合，可以通过id来获取单个资源。然而有时候资源是单数形式，例如某个资源的锁，因此用lock。





> 关于幂等性

计算机的幂等性是指一次请求与多次请求具有相同的副作用。GET、PUT、DELETE都具有幂等性，





### API的错误处理

PHP是弱类型，你可以返回0、false或null来表示调用函数或方法时产生的错误。PHP也提供了异常机制，允许你抛出异常来表示错误，并允许你去捕获该异常来处理错误。

我们应该采用哪一种？

在我的项目中，混合使用了这俩种方式，对于函数通过返回值来表示错误，对于类的方法，通过抛出异常来表示错误。我觉得异常就是预期的错误，可以认为是业务方面的预期错误，对于非预期的错误，统一用Exception来表示。



在API项目中或者WEB项目中，虽然都使用异常来表示错误，但是使用方式有些不同。

在API项目中可能会有api层（controler层，对外暴漏接口）、业务层、dao层。api层依赖业务层，业务层又依赖dao层。在这些层中出现错误，我会用异常对象封装业务错误码和错误信息，然后跑出去，由全局的异常处理器来处理。

对于预期的业务错误，我们会返回具体的业务错误码和错误信息。非预期的错误会统一返回服务器错误码。至于HTTP协议的状态码，我们会根据业务错误码去做划分，也就是这个业务错误码会有对应的HTTP状态码，最后由异常处理器统一去响应。

我觉得在API项目中这种处理方式会好得多，毕竟HTTP状态码只是表示请求的状态，不应该耦合到错误信息中。



在WEB项目中我们该如何处理异常？





> Dingo/API的响应

Dingo的Response。Response分为俩种，一种是\Dingo\Api\Http\Response，是对正常请求的响应，Dingo自身做了封装；另一种是快捷响应错误，本质上是抛出了异常，再由异常处理器处理。比如：

```php
	// \Dingo\Api\Http\Response\Factory的error方法    
	/**
     * Return an error response.
     *
     * @param string $message
     * @param int    $statusCode
     *
     * @throws \Symfony\Component\HttpKernel\Exception\HttpException
     *
     * @return void
     */
    public function error($message, $statusCode)
    {
        throw new HttpException($statusCode, $message);
    }
```



> web端怎么处理错误？





### HTTP状态码

2XX系列的状态码均表示请求正常。

- 200

    请求成功

- 201

    对创建新资源的POST操作进行响应。

- 204（No Content）

    对不会返回内容的成功请求的响应。



4XX系列，一般表示响应的错误原因是客户端造成的。

- 400（Bad Request）

    请求失败，请求的body无法解析。

- 401（Unauthorized）

    没有进行认证或者认证非法。比如登录认证失败，登录验证码错误或者用户未登录等。

- 403（Forbidden）

    服务器理解请求，但是拒绝执行它。一般是授权失败。

- 404（Not Found）

    请求一个不存在的资源

- 422（Unprocessable Entity）

    用于表示验证错误。比如请求参数错误、验证码失效。










