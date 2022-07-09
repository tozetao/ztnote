在web应用程序中，路由是将URL请求中的数据提供给适当的类进行处理的行为。Magento路由使用以下流程：

pub/index.php => Application => Front controller => Routing => Controller Action.



### FrontController class

FrontCntroller类通过RouterList类提供的路由列表进行搜索，直到匹配到一个可以处理请求的路由。当FrontController找到一个匹配的路由时，它将请求分派给路由返回的一个动作类。

如果FrontController不能找到处理请求的路由，它就使用默认的路由。



### Router class

路由类将一个请求与处理该请求的Action类相匹配。下面的表格显示了Magento自带的核心路由。

前端路由：

- robots：将请求与rebots.txt文件匹配
- urlrewrite：将请求与数据库中定义的URL匹配
- standard：标准路由
- cms：匹配对CMS页面的请求
- default：默认路由

adminhtml路由：

- admin：匹配Admin请求
- default：Admin的默认路由



#### Standard router

一个使用标准路由的url拥有下面格式：

```
<store-url>/<store-code>/<front-name>/<controller-name>/<action-name>
```

- store-url：指定应用实例的基础地址
- store-code：指定store上下文
- front-name：指定FrontController会使用的frontName，比如[routesxml]
- controller-name: 指定控制器名
- action-name：指定在控制器类上执行的action类

标准路由会解析此URL格式，并将其与正确的控制器和Action相匹配。



#### Default router

DefaultRouter类，是应用在路由检查过程中的最后一个路由。到达这一端的请求通常包含以前的路由器无法处理的无效URL。

应用使用默认的NoRouteHandler去处理这些请求，但是你也可以通过实现NoRouteHandlerInterface去重写自己的no-route-handler。



#### Custom routers

通过创建一个RouterInterface实现来创建一个自定义路由，同时在这个类中定义match()函数去使用你自己的路由匹配逻辑。

如果你需要路由配置数据，可以使用Route Config类。

去添加你的自定义路由到FrontController的路由列表中，在你的模块的frontend/di.xml文件中添加以下信息：

```xml
<type name="Magento\Framework\App\RouterList">
    <arguments>
        <argument name="routerList" xsi:type="array">
            <item name="%name%" xsi:type="array">
                <item name="class" xsi:type="string">%classpath%</item>
                <item name="disable" xsi:type="boolean">false</item>
                <item name="sortOrder" xsi:type="string">%sortorder%</item>
            </item>
        </argument>
    </arguments>
</type>
```

- %name% - 在Magento的唯一路由名
- %classpath% - 路由类的地址。例如: Magento\Robots\Controller\Router
- %sortorder% - 该实体在路由列表中的排序顺序。



### routes.xml

routes.xml文件映射出哪一个模块需要使用特定的frontName和区域的URL。routes.xml文件在模块中的位置（etc/frontend或etc/adminhtml）指定了哪些路由是激活的。

该文件内容使用了如下格式：

```xml
<config xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="urn:magento:framework:App/etc/routes.xsd">
    <router id="%routerId%">
        <route id="%routeId%" frontName="%frontName%">
            <module name="%moduleName%"/>
        </route>
    </router>
</config>
```

- %routerId - 指定在Magento的路由名。可以看Router class章节的参考表。
- %routeId% - 为该路由指定唯一的节点id，也是其关联的布局处理xml文件名的第一段（routeId_controller_action.xml）
- %frontName% - 指定了一个请求的基础URL之后的第一段。
- %mouduleName% - 指定模块名。

#### Before and after parameters

你可以在模块条目中添加一个before或after参数来覆盖或扩展现有模块中的路由。

```xml
<config xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="urn:magento:framework:App/etc/routes.xsd">
    <router id="standard">
        <route id="customer">
            <module name="ExampleCorp_RoutingExample" before="Magento_Customer" />
        </route>
    </router>
</config>
```

这个配置告诉FrontController在搜索Magento_Customer模块之前先在ExampleCorp_RouteingExample模块中寻找Actions。如果app/code/ExampleCorp/RoutingExample/Controller/Account/Login.php存在，它将使用该文件替代原始的类去处理login路由。



### Action class

Action类是ActionInterface接口的实现，路由会在请求匹配时返回ActionInterface接口。这些类的execute()方法包含着分发请求的逻辑。

每个Action都应该实现一个或多个Magento\Framework\App\Action\HttpHTTP MethodActionInterface，以声明它可以处理哪些HTTP请求方法。

- \Magento\Framework\App\Action\HttpDeleteActionInterface
- \Magento\Framework\App\Action\HttpGetActionInterface
- \Magento\Framework\App\Action\HttpPostActionInterface
- \Magento\Framework\App\Action\HttpPutActionInterface

应用为所有POST非AJAX请求提供了一个表单Key验证——如果你的ACtion不需要该验证，或者你想修改它，那么可以实现CsrfAwareActionInterface。

如果你需要把一个请求转发给你的类中的另一个动作，请使用Forward::forward(string $action)方法。

```php
<?php
declare(strict_types=1);

namespace ExampleCorp\RoutingExample\Controller\Index;

use Magento\Framework\App\Action\HttpGetActionInterface;
use Magento\Framework\Controller\Result\Forward;
use Magento\Framework\Controller\Result\ForwardFactory;

/**
 * Class Index
 */
class Index implements HttpGetActionInterface
{
    /**
     * @var ForwardFactory
     */
    private $forwardFactory;

    /**
     * @param ForwardFactory $forwardFactory
     */
    public function __construct(
        ForwardFactory $forwardFactory
    ) {
        $this->forwardFactory = $forwardFactory;
    }

    /**
     * @inheritdoc
     */
    public function execute()
    {
        /** @var Forward $forward */
        $forward = $this->forwardFactory->create();
        return $forward->forward('defaultNoRoute');
    }
}
```

注1：在Router中使用ActionFactory创建Action类的实例。

注2：Action类需要返回一个result object。



### Result object

- json

  在header中设置Content-Type:application/json，并返回一个包含数据的json编码的数组。

- raw

  返回已设置的数据。不在header中设置Content-Type。

- redirect

  创建外部重定向，浏览器将遵循该重定向并请求新的url

- forward

  在内部调用另一个action类的execute方法，并且不会触发来自浏览器的新请求。URL保持不变。

- layout

  View result。可以使用一个通用的布局响应去render任意种类的布局。layout包含了其layout element的响应主题，并将其设置为HTTP响应。

- page

  page是视图内容（View result）。封装了page type、page configuration，并施加了某些布局处理。page会触发layout.xml去渲染成HTML。

  

### Example of routing usage

声明一个新路由：

**File:** ExampleCorp/RoutingExample/etc/frontend/routes.xml

```xml
<?xml version="1.0"?>

<config xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:noNamespaceSchemaLocation="urn:magento:framework:App/etc/routes.xsd">
    <router id="standard">
        <route id="routing" frontName="routing">
            <module name="ExampleCorp_RoutingExample" />
        </route>
    </router>
</config>
```

为该路由声明一个布局处理（layout handler）：

**File:** ExampleCorp/RoutingExample/view/frontend/layout/routing_index_index.xml

```xml
<?xml version="1.0"?>

<page xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:noNamespaceSchemaLocation="urn:magento:framework:View/Layout/etc/page_configuration.xsd">
    <body>
        <referenceBlock name="page.main.title">
            <action method="setPageTitle">
                <argument translate="true" name="title" xsi:type="string">Routing Page</argument>
            </action>
        </referenceBlock>
    </body>
</page>
```



定义一个新的自定义路由：

**File:** ExampleCorp/RoutingExample/etc/frontend/di.xml

```xml
<type name="Magento\Framework\App\RouterList">
    <arguments>
        <argument name="routerList" xsi:type="array">
            <item name="routingExample" xsi:type="array">
                <item name="class" xsi:type="string">ExampleCorp\RoutingExample\Controller\Router</item>
                <item name="disable" xsi:type="boolean">false</item>
                <item name="sortOrder" xsi:type="string">40</item>
            </item>
        </argument>
    </arguments>
</type>
```

创建控制器，它将会处理routing路由并获取router传递的参数。

**File:** ExampleCorp/RoutingExample/Controller/Index/Index.php

```php
<?php
declare(strict_types=1);

namespace ExampleCorp\RoutingExample\Controller\Index;

use Magento\Framework\App\Action\HttpGetActionInterface;
use Magento\Framework\App\RequestInterface;
use Magento\Framework\View\Result\Page;
use Magento\Framework\View\Result\PageFactory;

/**
 * Class Index
 */
class Index implements HttpGetActionInterface
{
    /**
     * @var PageFactory
     */
    private $pageFactory;

    /**
      * @var RequestInterface
      */
    private $request;

    /**
     * @param PageFactory $pageFactory
     * @param RequestInterface $request
     */
    public function __construct(PageFactory $pageFactory, RequestInterface $request)
    {
        $this->pageFactory = $pageFactory;
        $this->request = $request;
    }

    /**
     * @inheritdoc
     */
    public function execute()
    {
        // Get the params that were passed from our Router
        $firstParam = $this->request->getParam('first_param', null);
        $secondParam = $this->request->getParam('second_param', null);

        return $this->pageFactory->create();
    }
}
```

最后创建router类，该类将自定义路由名learning与现有的routing路由相匹配。

```php
<?php
declare(strict_types=1);

namespace ExampleCorp\RoutingExample\Controller;

use Magento\Framework\App\Action\Forward;
use Magento\Framework\App\ActionFactory;
use Magento\Framework\App\ActionInterface;
use Magento\Framework\App\RequestInterface;
use Magento\Framework\App\ResponseInterface;
use Magento\Framework\App\RouterInterface;

/**
 * Class Router
 */
class Router implements RouterInterface
{
    /**
     * @var ActionFactory
     */
    private $actionFactory;

    /**
     * @var ResponseInterface
     */
    private $response;

    /**
     * Router constructor.
     *
     * @param ActionFactory $actionFactory
     * @param ResponseInterface $response
     */
    public function __construct(
        ActionFactory $actionFactory,
        ResponseInterface $response
    ) {
        $this->actionFactory = $actionFactory;
        $this->response = $response;
    }

    /**
     * @param RequestInterface $request
     * @return ActionInterface|null
     */
    public function match(RequestInterface $request): ?ActionInterface
    {
        $identifier = trim($request->getPathInfo(), '/');

        if (strpos($identifier, 'learning') !== false) {
            $request->setModuleName('routing');
            $request->setControllerName('index');
            $request->setActionName('index');
            $request->setParams([
                'first_param' => 'first_value',
                'second_param' => 'second_value'
            ]);

            return $this->actionFactory->create(Forward::class, ['request' => $request]);
        }

        return null;
    }
}
```

因此，通过访问http://site.com/learning 路径，http://site.com/routing/index/index 路径被加载。



### Declaring the new route as Page Type







reference

- https://bsscommerce.com/confluence/routing-in-magento-2/
- https://meetanshi.com/blog/magento-2-routing/
