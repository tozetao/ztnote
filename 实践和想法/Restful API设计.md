### 数据响应格式

一般都是使用JSON作为响应格式。

用户需要在请求头部Accept中指定响应的数据格式，对于不支持的格式，服务端需要正确的返回状态码，并给出详细的说明。





### 资源（Resource）

以资源为中心地URL设计

- 资源分为单个文档和集合，一般使用复数形式来表现资源，单个资源使用id或者name等来进行表示。

- 资源可以嵌套，类似目录的形式，以体现资源之间的关系。

  资源可以有一对一、一对多、多对一的映射关系，因此URI一般也是按照这种映射关系来进行嵌套的。

example：

```
-- 需求：在guthub中，一个用户可以有N个仓库，1个仓库可以有N个问题，1个问题可以有N个回复，它的URL设计如下：

/users/:name/repos			个人所有仓库
/repos/:owner/:repo			单个仓库的详细信息
/repos/:owner/:repo/contents		获取某个仓库的内容列表

/repos/:owner/:repo/commits			获取某个仓库的评论列表
/repos/:owner/:repo/commits/:commit_sha		获取某个仓库的某一条评论

/repos/:owner/:repo/issues					获取某个仓库的所有问题
/repos/:owner/:repo/issues/:issue_id			获取某个问题
/repos/:owner/:repo/issues/:issue_id/commtents	获取某个问题的所有评论
```

上述中的URI都是GET类型的请求，如果需要查询不同的内容，可以在URI后面添加查询参数。比如：

```
-- 分页查询某个用户的所有仓库
/users/:name/repos?page=2&per_page=5
```



### HTTP方法

对于资源的URL设计，所有针对资源的操作都是通过HTTP方法指定的。

- HEAD

  获取某个资源的头部信息。比如只想了解某个资源的大小，修改日志等等。

- GET

  获取资源

- POST

  创建资源

- PATCH

  更新资源的部分数学，PATCH比较新，实现复杂，一般使用POST代替。

- PUT

  客户端需要提供新建资源的所有属性。

- DELETE

  删除资源

注：创建和更新操作应该返回最新的资源，来通知用户资源的情况；删除资源一般不返回内容。

在实践中总有不符合CURD的情况，一般有几种处理方法：

- 将动作转化成CRUD操作的资源

  比如喜欢一个要点，就增加一个 /gist/:id/start URI，喜欢就用PUT操作，取消喜欢就用DELETE操作；

  另一个例子是fork要点，这也是一个动作，但是可用在gist下面增加一个forks资源，就符合CRUD兼容的：/gist/:id/forks。

- 增加控制参数，strong text

  添加动作的相关参数。比如一个博客网站，会有把写好的文章发布的功能。发布没有对应的名词，我们就可以文章中增加published字段，而发布就是更新该字段而已。

  > PUT /articles/id?published=true








### 状态码

Restful API要求正确的使用状态码，用状态码来反映响应的结果，如果报错将选择合适的状态码来返回。

常见状态码：

- 200 OK

   对成功的 GET、PUT、PATCH 或 DELETE 操作进行响应。也可以被用在不创建新资源的 POST 操作上

- 201 Created

  对创建新资源的 POST 操作进行响应。应该带着指向新资源地址的 Location 头

- 202 Accepted

  服务器接受了请求，但是还未处理，响应中应该包含相应的指示信息，告诉客户端该去哪里查询关于本次请求的信息

- 204 No Content

  对不会返回响应体的成功请求进行响应（比如 DELETE 请求）

- 304 Not Modified

  HTTP 缓存 header 生效的时候用

- 400 Bad Request

  请求异常，比如请求中的 body 无法解析

- 401 Unauthorized

  没有进行认证或者认证非法

- 403 Forbidden

  服务器已经理解请求，但是拒绝执行它

- 404 Not Found

  请求一个不存在的资源

- 405 Method Not Allowed

  所请求的 HTTP 方法不允许当前认证用户访问

- 410 Gone

  表示当前请求的资源不再可用。当调用老版本 API 的时候很有用

- 415 Unsupported Media Type

  如果请求中的内容类型是错误的

- 422 Unprocessable Entity

  用来表示校验错误

- 429 Too Many Requests

  由于请求频次达到上限而被拒绝访问



### 错误处理

对于客户的请求，如果成功将直接返回内容体。如果发生错误，将会根据错误的类型响应对应的状态码，以及给出错误的明确信息。

返回错误的body应该包含错误信息和错误对象，错误信息即描述错误的明确信息，而错误对象包括资源、字段属性和错误代码属性。

```json
{
    "message": "Body should be a JSON object",
    "errors": {
        "resource": "Issue",
        "field": "title",
        "code": "invalid"
    }
}
```



### 书写风格

无论是URL还是响应参数，使用小写字母与下划线凭借的方式来进行命名。









