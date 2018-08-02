### RESTful

RESTful是一种设计API的风格。所谓的API是一种应用接口，对外部提供可用服务。



### Resource

用资源来表示对象或数据库中的数据

用资源来对应数据，资源一般是以连接地址的方式出现。每个资源都有一个唯一的请求地址，也称为末端（endpoint）。通常一种数据具有俩种类型的资源，一种是数据的集合，一种是数据的个体。

数据的集合

> http://api.apitpl.dev/articles

配上ID表示数据的个体

> http://api/apitpl.dev/articles/1

一般的网址表示资源，所有URL中只有名次而没有动词，而且名词使用复数形式，普遍用复数来表示数据集合。



### HTTP Action

资源的操作是通过HTTP动词来进行的，HTTP动词有：

- GET：获取资源
- POST：创建数据
- PUT或PATCH：修改数据
- DELETE：删除数据
- HEAD：获取HTTP响应头信息，不展示响应体资源。
- OPTIONS：显示末端支持的动词，在响应头的Allow项显示支持的动词。



example

> GET  /users/1，查看id为1的用户
>
> POST  /users，创建一个用户
>
> DELETE /users/1，删除id为1的用户
>
> HEAD  /users/3，显示/users/3的响应头信息
>
> OPTIONS  /users，显示有关末端/users支持的动词



### Custom Resources

自定义资源。

需求：实现TOP10，显示前10名发表文章的作者？如何把top10作为独立的资源，该如何提供接口？

1. TOP10控制器，继承自yii\rest\Controller控制器，因为模型不是对应数据库的表。
2. 实现action方法进行查询，

资源一般是用名词的复数形式来表示，但是top10本身就有负数概念，加s就表示多个排行榜了。

pluralize => false，不用加false了。

except设定哪些http动词被禁用。





### Authentication



关于认证

认证的实现？

设置用户认证的组件，禁用session和cookie，因为API应用和web应用不同，API的客户端不能用session和cookie来维持认证状态。

Access Token是存取令牌，客户端是在每次请求的时候，同时发送一个access token给服务端。服务器每次收到存取令牌后，靠他来确认哪个用户发送的请求的，从而达到维持认证状态的目的。

存取令牌的生成：当登录验证成功后返回该Access Token，客户端接受到后会保存下来，方便后续的使用。



认证状态的维持？

认证成功后，客户端用access token去调用服务端的服务，能够保持认证状态。

1. 在控制器的behaviors()方法中配置过滤器，yii\filters\auth\QueryParamAuth。
2. 客户端使用access-token字段进行传递



要点分析

access token的认证方式，

用QueryParamAuth过滤器



Http基本认证

http规范中所指定的最基本的认证方式，实现是在响应中设置一个验证身份的Header即可。

> header('WWW-Authenticate: Basic realm="Test Auth"');

HttpBasicAuth过滤器实现了HTTP基本认证



Oauth2认证方式

经常用于第三方登录这种场景，例如使用QQ、微博登录的话就会使用基于OAuth2的认证服务。

认证流程

1. 客户端向服务器请求认证授权
2. 服务器就会给客户端一个授权凭证
3. 



安全问题

通过访问令牌直接去访问资源，在数据传输过程中是很容易被窃取的，根本的解决方法时用https协议来访问服务端。

这种就需要服务器端支持https请求了。

1. 采用安全的https传输协议

2. 给令牌增加过期时间，避免令牌泄露造成的危害。

   在登录成功后设置过期时间，每次请求的时候验证access token的同时也验证过期时间。

   过期的时候一般会让用户重新登录，同时重新设置一个令牌。

登录的时候，http动词不要用get方式，get方式用户名和密码是以明文的方式出现在url中，即使用https传输也没用。提交的时候要用post动词，

access-token











### API的授权

rbac的授权数据，检查发出API请求的客户端用户是否有权限执行其他请求的功能。



















### yii2相关

- yii\rest\ActiveController

  该控制器实现了一套符合restful风格的动作方法，AR数据模型用$modelClass来指定。



yii2的响应分为3个步骤：

- 首先是内容协商，客户端在想服务端发送请求的时候，会向http头信息中的Accept提出一些要求，比如媒介类型，语言、版本等方面的要求。

  例如客户端请求头Accept: appliction/json，表示要求服务端响应内容为json格式。

- 服务端收到请求后，首先准备资源，资源数据是以对象或集合的形式出现的，为了最终能转成字符串输入，这一步会先把对象或集合转换为数组，使用yii\rest\Serializer转换

- 得到数组后，服务端再根据第一步内容协商，客户端提出的要求，把数组转换成相应格式的字符串返回。



字段相关

一般用模型来代表响应数据，可以自定义模型要响应的字段列表，也可以过滤或格式化字段的内容。

重写模型类的fields或extraFields方法，根据需求设置字段以及字段的值，Serializer转换的数组就是需要的数据了。

- Model::fields
- Model::extraFields



链接HATEOAS

- Hypermedia as the Engine of Application State的缩写
- 让API返回的数据中附带一些和这条记录相关的链接地址

链接字段，在开发阶段响应给客户端查看的，主要实现Linkable接口





自定义分页的实现

1. unset原来的动作
2. 重写原来的action方法，设置数据提供者的分页设置
3. 客户端可以使用page来指定页码



搜索的实现

需求：按照关键字搜索文章

URI：articles/search

query string：key=?

1. 实现Action方法
2. 配置文件中配置符合restful风格的url，extraPatterns配置

POST动作 => 创建数据?，默认客户端使用html/json数据传输，如果要在POST数组中能够进行访问，需要使用application/x-www-form-urlencoded表单类型提交。



自定义资源，TOP10发布数量的作者



5个功能的测试，同时测试yii2的9个特性

