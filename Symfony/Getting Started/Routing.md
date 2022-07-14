当你的应用程序接受到一个请求时，它会调用控制器Action去生成一个响应。路由配置定义了每个进来的请求由哪个Action去处理。它也提供了其他有用的功能，比如生成SEO友好的URL（例如用/read/intro-to-symfony替代index.php?article_id=57）。



### Creating Routes

路由可以被配置在YAML、XML、PHP中，也可以用annotations或attributes配置。不同方式的配置都提供了相同的功能和性能，所以你可以选择你喜欢的方式。Symfony推荐attributes，因为把路由配置和控制器放在一起是很方便的事情。

#### Creating Routes as Attributes or Annotations

PHP attributes和annotatiions允许这些路由配置定义在相关的控制器代码旁边。Attributes是PHP8或更高版本的原生功能，所以你可以直接使用该特性。

在PHP7或更早版本你可以使用annotation是（通过Doctrine Annotions库），但是你首先要在你的项目中安装以下依赖：

```
composer require doctrine/annotations
```

> 5.2 使用PHP Attributes配置路由是在5.2中引入的。在此之前，Doctrine Annotations配置路由的唯一方式是使用注解。

无论你使用属性或注解，在使用它们之前，你都需要在你的项目中添加一些配置。如果你安装了注解依赖，并且你的项目使用Symfony Flex，该文件已经帮你配置了，否则你需要手动创建以下文件（type:annotation选项也适用于属性，所以保留它）。

```yaml
# config/routes/annotations.yaml
controllers:
    resource: ../../src/Controller/
    type: annotation

kernel:
    resource: ../../src/Kernel.php
    type: annotation
```

该配置会告诉Symfony在src/Controller目录中的任意PHP类文件中，去寻找定义为Attributes或Annotation的路由。

假设你想在你的应用中定义一个/blog URL的路由。首先跟下面一样去创建一个控制器：

```php
// src/Controller/BlogController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\Routing\Annotation\Route;

class BlogController extends AbstractController
{
    /**
     * @Route("/blog", name="blog_list")
     */
    public function list()
    {
        // ...
    }

    // 这里是使用属性定义路由
    #[Route('/blog', name: 'blog_list')]
}
```

该配置定义了一个blog_list的路由，当路由请求/blog URL时就会被匹配到。当发生Url匹配时，应用就会运行BlogController控制器的list()方法。

注：在匹配路由时是不考虑URL的query字符串。在这个例子中，类似/blog?foo=bar或/blog?foo-bar&bar=foo也会匹配到blog_list路由。

警告：如果你在同个文件定义了多个PHP类，Symfony仅会加载第一个类的路由并忽略其他类。

路由名称（blog_list）现在并不重要，但以后在生成URL时，它将是必不可少的。你只需要记住，每个路由名称在应用程序中必须是唯一的。



#### Creating Routes in YAML, XML or PHP Files

你可以在一个单独的YAML、XML或PHP文件中定义路由，而不是在控制器类中定义路由。优点是它们不需要任何额外的依赖；缺点是，在检查一些控制器动作的路由时，你必须处理多个文件。

下面的例子展示了如何在YAML/XML/PHP中定义一个名为blog_list的路由，将/blog URL与BlogController的list()Action联系起来。

```yaml
# config/routes.yaml
blog_list:
    path: /blog
    # the controller value has the format 'controller_class::method_name'
    controller: App\Controller\BlogController::list

    # if the action is implemented as the __invoke() method of the
    # controller class, you can skip the '::method_name' part:
    # controller: App\Controller\BlogController
```

> 5.1 从Symfony 5.1开始，默认情况下Symfony只加载以YAML格式定义的路由。如果你用XML和/或PHP格式定义路由，请更新src/Kernel.php文件，增加对.xml和.php文件扩展名的支持。





#### Matching HTTP Methods

默认情况下，路由匹配任何HTTP动词（GET、POST、PUT等）。使用methods选项来限制每个路由应该响应的动词。

```php
// src/Controller/BlogApiController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class BlogApiController extends AbstractController
{
    /**
     * @Route("/api/posts/{id}", methods={"GET","HEAD"})
     */
    public function show(int $id): Response
    {
        // ... return a JSON response with the post
    }

    /**
     * @Route("/api/posts/{id}", methods={"PUT"})
     */
    public function edit(int $id): Response
    {
        // ... edit a post
    }
}
```

```yaml
# config/routes.yaml
api_post_show:
    path:       /api/posts/{id}
    controller: App\Controller\BlogApiController::show
    methods:    GET|HEAD

api_post_edit:
    path:       /api/posts/{id}
    controller: App\Controller\BlogApiController::edit
    methods:    PUT
```

注：HTML表单只支持GET和POST方法。如果你用不同的方法从HTML表单中调用路由，可以添加一个名为\_method的隐藏字段，写上要使用的方法（例如：<input type="hidden" name="_method" value="PUT"/>）。如果你用[Symfony Forms](https://symfony.com/doc/5.4/forms.html)创建你的表单，这将自动为你完成。



#### Matching Expressions

如果你有一些路由需要基于某些逻辑进行匹配，请使用condition选项：

```yaml
# config/routes.yaml
contact:
    path:       /contact
    controller: 'App\Controller\DefaultController::contact'
    condition:  "context.getMethod() in ['GET', 'HEAD'] and request.headers.get('User-Agent') matches '/firefox/i'"
    # expressions can also include configuration parameters:
    # condition: "request.headers.get('User-Agent') matches '%app.allowed_browsers%'"
    # expressions can even use environment variables:
    # condition: "context.getHost() == env('APP_MAIN_HOST')"
```

condition选项的值可以是任意有效的[ExpressionLanguage 表达式](https://symfony.com/doc/5.4/components/expression_language/syntax.html)，并且可以用于任何Symfony创建的变量：

- context

  [RequestContext](https://github.com/symfony/symfony/blob/5.4/src/Symfony/Component/Routing/RequestContext.php)的实例，包含关于被匹配的路由的最基本信息。

- request

  [Symfony Request](https://symfony.com/doc/5.4/components/http_foundation.html#component-http-foundation-request) 对象表示当前请求

你也可以使用这个函数：

- env(string $name)

  使用[Environment Variable Processors](https://symfony.com/doc/5.4/configuration/env_var_processors.html)返回一个变量的值。

在幕后表达式被编译成原始的PHP代码。正因为如此，除了底层PHP的执行时间，使用condition选项不会造成额外的开销。

警告：生成URL时不考虑条件（本文后面会解释）。





#### Debugging Routes

随着你应用程序的增长，你最终会有许多路由。Symfony包含了一些命令去帮助你调试路由问题。首先，debug:router命令会按照Symfony评估后的顺序列出你应用的所有路由：

```
$ php bin/console debug:router

----------------  -------  -------  -----  --------------------------------------------
Name              Method   Scheme   Host   Path
----------------  -------  -------  -----  --------------------------------------------
homepage          ANY      ANY      ANY    /
contact           GET      ANY      ANY    /contact
contact_process   POST     ANY      ANY    /contact
article_show      ANY      ANY      ANY    /articles/{_locale}/{year}/{title}.{_format}
blog              ANY      ANY      ANY    /blog/{page}
blog_show         ANY      ANY      ANY    /blog/{slug}
----------------  -------  -------  -----  --------------------------------------------
```

可以把某些路由名（或路由一部分名字）传递给该参数，以打印出路由的详细信息。

```
$ php bin/console debug:router app_lucky_number

+-------------+---------------------------------------------------------+
| Property    | Value                                                   |
+-------------+---------------------------------------------------------+
| Route Name  | app_lucky_number                                        |
| Path        | /lucky/number/{max}                                     |
| ...         | ...                                                     |
| Options     | compiler_class: Symfony\Component\Routing\RouteCompiler |
|             | utf8: true                                              |
+-------------+---------------------------------------------------------+
```

另一个命令是router:match，它会显示匹配给予的URL的路由。找出某些URL没有执行你预期的控制器Action时是非常有用的：

```
$ php bin/console router:match /lucky/number/8
  [OK] Route "app_lucky_number" matches
```



### Route Parameters

在上面的例子中定义了一个不会变化的路由（/blog）。然而，定义某些部分是可变的路由是很常见的。例如，显示某些博客文章的URL可能包括标题或slug（例如/blog/my-first-post或/blog/all-about-symfony）。

在Symfony路由中，变量部分被包裹在{ ... }包裹，而且必须有一个唯一的名字。例如，显示博客文章内容的路由被定义为/blog/{slug}。

```yaml
# config/routes.yaml
blog_show:
    path:       /blog/{slug}
    controller: App\Controller\BlogController::show
```

变量名的一部分（本例为{slug}）被用于创建一个变量，该变量会被作为路由内容存储并传递给控制器。如果一个用户访问/blog/my-first-post URL，Symfony会执行BlogController控制器的show()方法，并传递参数\$slug = 'my-first-post'给show方法。

路由可以定义任意数量的参数，但每个参数只能在每个路由上使用一次（例如，/blog/posts-about-{category}/page/{pageNumber}）。





#### Parameters Validation

想象一下，你的应用程序有一个blog_show路由（URL：/blog/{slug}）和一个blog_list路由（URL：/blog/{page}）。鉴于路由参数接受任何值，没有办法区分这两个路由。

如果用户请求/blog/my-first-post，两条路由都会匹配，Symfony会使用最先定义的路由。要解决这个问题，可以使用requirements选项给{page}参数添加一些验证。

```yaml
# config/routes.yaml
blog_list:
    path:       /blog/{page}
    controller: App\Controller\BlogController::list
    requirements:
        page: '\d+'

blog_show:
    path:       /blog/{slug}
    controller: App\Controller\BlogController::show
```

requirements选项定义了PHP正则表达式，路由参数必须匹配这些正则表达式才能匹配整个路由。在这个例子中，\d+ 是一个正则表达式，可以匹配任何长度的数字。现在：

| URL                   | Route       | Parameters                |
| --------------------- | ----------- | ------------------------- |
| `/blog/2`             | `blog_list` | `$page` = `2`             |
| `/blog/my-first-post` | `blog_show` | `$slug` = `my-first-post` |

注1：路由requirements（也包括路由path选项）可以包括配置参数，只需要定义一次复杂的正则表达式就可以在多个路由中重复使用它。

注2：参数也支持 [PCRE Unicode properties](https://www.php.net/manual/en/regexp.reference.unicode.php)，它是匹配通用字符类型的转义序列。例如， \p{Lu} 匹配任何语言中的任何大写字符， \p{Greek} 匹配任何希腊字符，等等。

提示：当在路由中使用正则表达式时，你可以设置 utf8 路由选项为true，这可以使得 . 字符匹配任意个UTF8字符而不只是单个字节的匹配。

```
# config/routes.yaml
blog_list:
    path:       /blog/{page<\d+>}
    controller: App\Controller\BlogController::list
```

如果愿意，可以使用语法{parameter\u name<requirements>}将requirements内联到每个参数中。此功能使配置更加简洁，但当需求复杂时，它会降低路由可读性：

```
# config/routes.yaml
blog_list:
    path:       /blog/{page<\d+>}
    controller: App\Controller\BlogController::list
```



#### Optional Parameters

在前面的例子中，blog_list的URL是/blog/{page}。如果用户访问/blog/1，它将匹配。但如果他们访问/blog，它将不匹配。只要你给路由添加一个参数，它就必须有一个值。

你可以通过为{page}参数添加一个默认值，使blog_list在用户访问/blog时再次匹配。当使用Annotations或Attributes时，默认值被定义在控制器Action的参数中。在其他配置格式中，它们是通过defaults选项来定义的。

```yaml
# config/routes.yaml
blog_list:
    path:       /blog/{page}
    controller: App\Controller\BlogController::list
    defaults:
        page: 1
    requirements:
        page: '\d+'

blog_show:
    # ...
```

```php
// src/Controller/BlogController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class BlogController extends AbstractController
{
    #[Route('/blog/{page}', name: 'blog_list', requirements: ['page' => '\d+'])]
    public function list(int $page = 1): Response
    {
        // ...
    }
}
```

现在，当用户访问/blog时，blog_list路由将被匹配，$page将默认为1的值。

警告：你可以有一个以上的可选参数（例如：/blog/{slug}/{page}），但可选参数之后的所有内容必须是可选的。例如，/{page}/blog是一个有效的路径，但page永远是必需的（即/blog不会匹配这个路径）。

如果你想在生成的URL中总是包含一些默认值（例如强制生成/blog/1而不是前面例子中的/blog），在参数名称前添加！字符。/blog/{！page}

和requirements一样，默认值也可以使用{parameter_name?default_value}语法内置在每个参数中。这个功能与内置requirements兼容，所以你可以在一个参数中同时内置两者。

```php
// src/Controller/BlogController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class BlogController extends AbstractController
{
    #[Route('/blog/{page<\d+>?1}', name: 'blog_list')]
    public function list(int $page): Response
    {
        // ...
    }
}
```

```yaml
# config/routes.yaml
blog_list:
    path:       /blog/{page<\d+>?1}
    controller: App\Controller\BlogController::list
```

提示：要给任何参数一个空的默认值，在? 字符后不加任何东西（例如：/blog/{page?}）。如果你这样做，别忘了更新相关控制器参数的类型，以允许传递空值（例如，用?int $page替换int $page）。



#### Priority Parameter

Symfony按照路由定义的顺序来评估路由的优先级。如果一个路由的path匹配了多个模式，它可能会阻止其他路由被匹配。在YAML和XML中，你可以在配置文件中向上或向下移动路由定义来控制它们的优先级。在定义为 PHP 注解或属性的路由中，这就很难做到了，所以你可以设置这些路由选项的priority参数去控制它们的优先级。

```php
// src/Controller/BlogController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\Routing\Annotation\Route;

class BlogController extends AbstractController
{
    /**
     * This route has a greedy pattern and is defined first.
     *
     * @Route("/blog/{slug}", name="blog_show")
     */
    public function show(string $slug)
    {
        // ...
    }

    /**
     * This route could not be matched without defining a higher priority than 0.
     *
     * @Route("/blog/list", name="blog_list", priority=2)
     */
    public function list()
    {
        // ...
    }
}
```

priority参数期望一个整数值。优先级较高的路由排在优先级较低的路由之前。没有定义时，默认值为0。



#### Parameter Conversion

一个常见的路由需求是将存储在某些参数中的值（例如作为用户ID的整数）转换成另一个值（例如代表用户的对象）。这个功能被称为 "param converter"。

为了增加对 "param converter "的支持，我们需要SensioFrameworkExtraBundle。

```
$ composer require sensio/framework-extra-bundle
```

现在，保持之前的路由配置，但改变控制器Action的参数。取代字符串$slug，添加BlogPost $post。

```php
// src/Controller/BlogController.php
namespace App\Controller;

use App\Entity\BlogPost;
use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class BlogController extends AbstractController
{
    // ...

    /**
     * @Route("/blog/{slug}", name="blog_show")
     */
    public function show(BlogPost $post): Response
    {
        // $post is the object whose slug matches the routing parameter

        // ...
    }
}
```

如果你的控制器参数包括对象的类型提示（本例中是BlogPost），"param converter "会使用请求参数（本例中是slug）进行数据库请求以找到对象。如果没有找到对象，Symfony会自动生成一个404响应。



#### Special Parameters

除了你自己的参数外，路由还可以包括以下由Symfony创建的特定参数。

- _controller

  该参数被用于去确定当一个路由被匹配时会执行哪个控制器和Action。

- _format

  匹配值用于设置Request对象的 "请求格式"。这用于设置响应的内容类型（例如，json格式转换为application/json的Content-type）。

- _fragment

  用于设置片段标识符，这是URL中可选的最后一部分，以#字符开头，用于标识文档的一部分。

- _locale

  用来设置请求中的地区语言。

你可以在单个路由（individual routes）或路由导入（route imports）中包含这些属性（除了_fragment）。Symfony定义了一些具有相同名称的特殊属性（除了前面的下划线），所以你可以更容易地定义它们。

```yaml
# config/routes.yaml
article_search:
  path:        /articles/{_locale}/search.{_format}
  controller:  App\Controller\ArticleController::search
  locale:      en
  format:      html
  requirements:
      _locale: en|fr
      _format: html|xml
```

```php
// src/Controller/ArticleController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class ArticleController extends AbstractController
{
    /**
     * @Route(
     *     "/articles/{_locale}/search.{_format}",
     *     locale="en",
     *     format="html",
     *     requirements={
     *         "_locale": "en|fr",
     *         "_format": "html|xml",
     *     }
     * )
     */
    public function search(): Response
    {
    }
}
```



#### Extra Parameters

在路由的defaults选项中，你可以选择性地定义不包括在路由配置中的参数。这对于向路由的控制器传递额外的参数很有用。

```php
// src/Controller/BlogController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class BlogController extends AbstractController
{
    /**
     * @Route("/blog/{page}", name="blog_index", defaults={"page": 1, "title": "Hello world!"})
     */
    public function index(int $page, string $title): Response
    {
        // ...
    }
}
```

```yaml
# config/routes.yaml
blog_index:
    path:       /blog/{page}
    controller: App\Controller\BlogController::index
    defaults:
        page: 1
        title: "Hello world!"
```





#### Slash Characters in Route Parameters

路由参数可以包含任何值，除了/斜线字符，因为那是用来分隔URL不同部分的字符。例如，如果/share/{token}路由中的token值包含一个/字符，这个路由就不会匹配。

一个可能的解决方案是改变参数要求，使其更加宽松。

```yaml
# config/routes.yaml
share:
    path:       /share/{token}
    controller: App\Controller\DefaultController::share
    requirements:
        token: .+
```

```php
// src/Controller/DefaultController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class DefaultController extends AbstractController
{
    /**
     * @Route("/share/{token}", name="share", requirements={"token"=".+"})
     */
    public function share($token): Response
    {
        // ...
    }
}
```

提示1：如果路由定义了几个参数，并且允许正则表达式应用于所有参数，则可能会得到意外的结果。例如，如果路由的定义是/share/{path}/{token}，而path和token都接受/，那么token将只得到最后一部分，其余部分由path匹配。

提示2：如果路由包括特殊的{\_format}参数，你不应该对允许斜杠的参数使用 .+ 表达式进行约束（requirement）。例如，如果模式是/share/{token}.{\_format}，而{token}允许任何字符，那么/share/foo/bar.json URL将认为foo/bar.json是token，format将是空的。这可以通过用[^.]+替换.+的要求来解决，以允许除点以外的任何字符。





### Route Groups and Prefixes

一组路由共享一些选项是很常见的（比如所有与博客相关的路由都以/blog开头）这就是为什么Symfony提供了共享路由配置的功能。

当把路由定义为Attributes或Annotataions时，把共同的配置放在控制器类的#[Route]Attributes（或@Route Annotataions）中。对于其他路由格式中，当导入路由时会使用选项定义的共同配置。

```yaml
# config/routes/annotations.yaml
controllers:
    resource: '../../src/Controller/'
    type: annotation
    # this is added to the beginning of all imported route URLs
    prefix: '/blog'
    # this is added to the beginning of all imported route names
    name_prefix: 'blog_'
    # these requirements are added to all imported routes
    requirements:
        _locale: 'en|es|fr'

    # An imported route with an empty URL will become "/blog/"
    # Uncomment this option to make that URL "/blog" instead
    # trailing_slash_on_root: false

    # you can optionally exclude some files/subdirectories when loading annotations
    # exclude: '../../src/Controller/{DebugEmailController}.php'
```

```php
// src/Controller/BlogController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

/**
 * @Route("/blog", requirements={"_locale": "en|es|fr"}, name="blog_")
 */
class BlogController extends AbstractController
{
    /**
     * @Route("/{_locale}", name="index")
     */
    public function index(): Response
    {
        // ...
    }

    /**
     * @Route("/{_locale}/posts/{slug}", name="show")
     */
    public function show(Post $post): Response
    {
        // ...
    }
}
```

在这个示例中，index()Action的路由会被称为blog_index，它的url为/blog/{\_locale}。show()Action路由被称为blog_show，它的URL为/blog/{\_locale}/posts/{slug}。这两个路由还将验证_locale参数是否与类注解中定义的正则表达式相匹配。

如果任何一个有前缀的路由定义了一个空路径，Symfony会在尾部给它加上一个斜杠。在前面的例子中，一个以/blog为前缀的空路径会导致/blog/的URL。如果你想避免这种行为，可以把trailing_slash_on_root选项设置为false（这个选项在使用PHP属性或注释时不可用）。

```yaml
# config/routes/annotations.yaml
controllers:
    resource: '../../src/Controller/'
    type:     annotation
    prefix:   '/blog'
    trailing_slash_on_root: false
    # ...
```

注：Symfony可以从不同源导入路由（ [import routes from different sources](https://symfony.com/doc/5.4/routing/custom_route_loader.html)），你甚至可以定义你自己的路由加载器。



### Getting the Route Name and Parameters

Symfony创建的Request对象在 "请求属性 "中存储了所有的路由配置（如名称和参数）。你可以在控制器中通过Request对象获得这些信息。

```php
// src/Controller/BlogController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class BlogController extends AbstractController
{
    /**
     * @Route("/blog", name="blog_list")
     */
    public function list(Request $request): Response
    {
        $routeName = $request->attributes->get('_route');
        $routeParameters = $request->attributes->get('_route_params');

        // use this to get all the available attributes (not only routing ones):
        $allAttributes = $request->attributes->all();

        // ...
    }
}
```

你也可以在服务中获得这些信息，注入request_stack服务来获得服务中的Request对象（[get the Request object in a service](https://symfony.com/doc/5.4/service_container/request.html)）。在模板中，使用Twig全局应用变量（[Twig global app variable](https://symfony.com/doc/5.4/templates.html#twig-app-variable)）来获取request和它的属性。

```php
{% set route_name = app.request.attributes.get('_route') %}
{% set route_parameters = app.request.attributes.get('_route_params') %}

{# use this to get all the available attributes (not only routing ones) #}
{% set all_attributes = app.request.attributes.all %}
```





### Special Routes

Symfony定义了一些特殊的控制器来渲染模板，并从路由配置中重定向到其他路由，所以你不必创建一个控制器动作。

#### Rendering a Template Directly from a Route

请阅读关于Symfony模板的主要文章中关于从路由渲染模板（[rendering a template from a route](https://symfony.com/doc/5.4/templates.html#templates-render-from-route)）的部分。



#### Redirecting to URLs and Routes Directly from a Route

使用RedirectController去重定向到其他路由和URL。

```yaml
# config/routes.yaml
doc_shortcut:
    path: /doc
    controller: Symfony\Bundle\FrameworkBundle\Controller\RedirectController
    defaults:
        route: 'doc_page'
        # optionally you can define some arguments passed to the route
        page: 'index'
        version: 'current'
        # redirections are temporary by default (code 302) but you can make them permanent (code 301)
        permanent: true
        # add this to keep the original query string parameters when redirecting
        keepQueryParams: true
        # add this to keep the HTTP method when redirecting. The redirect status changes
        # * for temporary redirects, it uses the 307 status code instead of 302
        # * for permanent redirects, it uses the 308 status code instead of 301
        keepRequestMethod: true

legacy_doc:
    path: /legacy/doc
    controller: Symfony\Bundle\FrameworkBundle\Controller\RedirectController
    defaults:
        # this value can be an absolute path or an absolute URL
        path: 'https://legacy.example.com/doc'
        permanent: true
```

提示：Symfony还提供了一些实用工具来重定向控制器内部（[redirect inside controllers](https://symfony.com/doc/5.4/controller.html#controller-redirect)）。



Redirecting URLs with Trailing Slashes

历史上，URLs遵循UNIX的惯例，即在目录中加入尾部斜线（例如：https://example.com/foo/），在引用文件时去掉斜线（https://example.com/foo）。尽管为两个URL提供不同的内容是可以的，但现在通常把两个URL当作同一个URL，并在它们之间重定向。

Symfony遵循这个逻辑，在有尾部斜线和无尾部斜线的URL之间重定向（但只针对GET和HEAD请求）。

| Route URL | If the requested URL is `/foo`       | If the requested URL is `/foo/`     |
| :-------- | :----------------------------------- | :---------------------------------- |
| `/foo`    | It matches (`200` status response)   | It makes a `301` redirect to `/foo` |
| `/foo/`   | It makes a `301` redirect to `/foo/` | It matches (`200` status response)  |



### Sub-Domain Routing

路由可以配置一个host选项，要求传入请求的HTTP host与某些特定值相匹配。在下面的例子中，两个路由都匹配相同的路径（/），但仅其中一个响应特定的主机名。

```yaml
# config/routes.yaml
mobile_homepage:
    path:       /
    host:       m.example.com
    controller: App\Controller\MainController::mobileHomepage

homepage:
    path:       /
    controller: App\Controller\MainController::homepage
```

主机选项的值可以包括参数（这在多租户应用中很有用），这些参数也可以用requirements来验证。

```yaml
# config/routes.yaml
mobile_homepage:
    path:       /
    host:       "{subdomain}.example.com"
    controller: App\Controller\MainController::mobileHomepage
    defaults:
        subdomain: m
    requirements:
        subdomain: m|mobile

homepage:
    path:       /
    controller: App\Controller\MainController::homepage
```

在上面的例子中，子域名参数定义了一个默认值，否则你每次使用这些路由生成URL时都需要包含一个子域名值。

注：你也可以在导入路由（ [importing routes](https://symfony.com/doc/5.4/routing.html#routing-route-groups)）时设置host选项，使所有的路由都需要该host名。

当使用子域路由时，你必须在功能测试中设置Host HTTP头信息，否则路由将不匹配。

```php
$crawler = $client->request(
    'GET',
    '/',
    [],
    [],
    ['HTTP_HOST' => 'm.example.com']
    // or get the value from some configuration parameter:
    // ['HTTP_HOST' => 'm.'.$client->getContainer()->getParameter('domain')]
);
```

注：你也可以在host选项中使用行内的默认值和格式要求。比如：{subdomain<m|mobile>?m}.example.com



### Localized Routes (i18n)

如果你的应用程序被翻译成多国语言，每个路由可以为每个翻译地区定义一个不同的URL。这就避免了重复路由的需要，这也减少了潜在的错误。

```yaml
# config/routes.yaml
about_us:
    path:
        en: /about-us
        nl: /over-ons
    controller: App\Controller\CompanyController::about
```

提示：当使用PHP属性进行本地化路由时，必须使用path命名参数来指定路径数组。

当匹配本地化路由时，Symfony会在整个请求过程中自动使用相同的区域设置。

注：当应用程序使用完整的 "语言+地域 "地域（如fr_FR、fr_BE）时，如果URL在所有相关地域中都是相同的，路由可以只使用语言部分（如fr），以避免重复相同的URL。

国际化应用程序的一个常见要求是在所有的路由中加入一个地区性的前缀。这可以通过为每个地区定义一个不同的前缀来实现（如果你喜欢，可以为你的默认地区设置一个空的前缀）。

```yaml
# config/routes/annotations.yaml
controllers:
    resource: '../../src/Controller/'
    type: annotation
    prefix:
        en: '' # don't prefix URLs for English, the default locale
        nl: '/nl'
```

另一个常见的要求是，根据不同的地区，将网站托管在不同的域名上。这可以通过为每个地区定义一个不同的主机来实现。

> 5.1 在Symfony 5.1中引入了定义主机数组的能力。

```yaml
# config/routes/annotations.yaml
controllers:
    resource: '../../src/Controller/'
    type: annotation
    host:
        en: 'https://www.example.com'
        nl: 'https://www.example.nl'
```





### Stateless Routes

> 5.1 The `stateless` option was introduced in Symfony 5.1.

有时，当一个HTTP响应应该被缓存时，必须确保能够发生。然而，只要在请求过程中启动了会话，Symfony就会把响应变成一个不可缓存的私有响应。

For details, see [HTTP Cache](https://symfony.com/doc/5.4/http_cache.html).

路由可以配置一个stateless布尔选项，以声明在匹配请求时不应该使用会话。

```yaml
# config/routes.yaml
homepage:
    controller: App\Controller\MainController::homepage
    path: /
    stateless: true
```

现在，如果会话被使用，应用程序将根据你的kernel.debug参数来报告它。

- enabled: 将会抛出一个[UnexpectedSessionUsageException](https://github.com/symfony/symfony/blob/5.4/src/Symfony/Component/HttpKernel/Exception/UnexpectedSessionUsageException.php) exception
- disabled：日志会记录一个警告。

这将帮助你理解并希望能修复你的应用程序中的意外行为。



### Generating URLs

路由系统是双向的：

1）它们将URL与控制器相关联（如前几节所述）；

2）它们为给定的路由生成URL。从路由中生成URL允许你不在你的HTML模板中手动编写<a href="...">值。另外，如果某些路由的URL发生变化，你只需要更新路由配置，所有的链接都会被更新。

要生成一个URL，你需要指定路由的名称（如blog_show）和路由定义的参数值（如slug = my-blog-post）。

由于这个原因，每个路由都有一个内部名称，在应用程序中必须是唯一的。如果你没有用name选项明确设置路由名称，Symfony会根据控制器和动作自动生成一个名称。



#### Generating URLs in Controllers

如果你的控制器继承自[AbstractController](https://symfony.com/doc/5.4/controller.html#the-base-controller-class-services)，可以使用generateUrl辅助方法：

```php
// src/Controller/BlogController.php
namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;
use Symfony\Component\Routing\Generator\UrlGeneratorInterface;

class BlogController extends AbstractController
{
    /**
     * @Route("/blog", name="blog_list")
     */
    public function list(): Response
    {
        // generate a URL with no route arguments
        $signUpPage = $this->generateUrl('sign_up');

        // generate a URL with route arguments
        $userProfilePage = $this->generateUrl('user_profile', [
            'username' => $user->getUserIdentifier(),
        ]);

        // generated URLs are "absolute paths" by default. Pass a third optional
        // argument to generate different URLs (e.g. an "absolute URL")
        $signUpPage = $this->generateUrl('sign_up', [], UrlGeneratorInterface::ABSOLUTE_URL);

        // when a route is localized, Symfony uses by default the current request locale
        // pass a different '_locale' value if you want to set the locale explicitly
        $signUpPageInDutch = $this->generateUrl('sign_up', ['_locale' => 'nl']);

        // ...
    }
}
```

提示：如果你传递给generateUrl()方法一些不是路由定义的参数，它们将作为查询字符串包含在生成的URL中。

```php
$this->generateUrl('blog', ['page' => 2, 'category' => 'Symfony']);
// the 'blog' route only defines the 'page' parameter; the generated URL is:
// /blog/2?category=Symfony
```

警告：当对象作为占位符使用时，会被转换为字符串，而当它们作为额外参数使用时，则不会被转换。因此，如果你传递一个对象（例如Uuid）作为额外参数的值，你需要明确地将其转换为字符串。

```php
$this->generateUrl('blog', ['uuid' => (string) $entity->getUuid()]);
```

如果你的控制器没有从AbstractController扩展，你需要在你的控制器中获取服务，并遵循下一节的指示。





#### Generating URLs in Services

在我们的服务中注入Symfony路由服务，并使用其generate()方法。当使用service autowiring时，你只需要在服务构造函数中添加一个参数，并用UrlGeneratorInterface类对其进行类型提示。

```php
// src/Service/SomeService.php
namespace App\Service;

use Symfony\Component\Routing\Generator\UrlGeneratorInterface;

class SomeService
{
    private $router;

    public function __construct(UrlGeneratorInterface $router)
    {
        $this->router = $router;
    }

    public function someMethod()
    {
        // ...

        // generate a URL with no route arguments
        $signUpPage = $this->router->generate('sign_up');

        // generate a URL with route arguments
        $userProfilePage = $this->router->generate('user_profile', [
            'username' => $user->getUserIdentifier(),
        ]);

        // generated URLs are "absolute paths" by default. Pass a third optional
        // argument to generate different URLs (e.g. an "absolute URL")
        $signUpPage = $this->router->generate('sign_up', [], UrlGeneratorInterface::ABSOLUTE_URL);

        // when a route is localized, Symfony uses by default the current request locale
        // pass a different '_locale' value if you want to set the locale explicitly
        $signUpPageInDutch = $this->router->generate('sign_up', ['_locale' => 'nl']);
    }
}
```



#### Generating URLs in Templates

在关于Symfony模板的文章中读取[creating links between pages](https://symfony.com/doc/5.4/templates.html#templates-link-to-pages) （在页面中创建links）这一章节。



#### Generating URLs in JavaScript

如果你的JavaScript代码包含在Twig模板中，你可以使用Twig的path()和url()函数来生成URL并将其存储在JavaScript变量中。需要使用escape()过滤器来转义任何非JavaScript安全的值。

```js
<script>
    const route = "{{ path('blog_show', {slug: 'my-blog-post'})|escape('js') }}";
</script>
```

如果你需要动态地生成URL，或者你使用的是纯JavaScript代码，这个解决方案就不适用了。在这些情况下，考虑使用[FOSJsRoutingBundle](https://github.com/FriendsOfSymfony/FOSJsRoutingBundle).



#### Generating URLs in Commands

在命令中生成URL与在服务中生成URL的工作方式相同。唯一的区别是，命令不是在HTTP上下文中执行的。因此，如果你生成绝对的URL，你会得到http://localhost/，而不是你真正的主机名。

解决方案是配置default_uri选项来定义命令在生成URL时使用的 "请求环境"。

```yaml
# config/packages/routing.yaml
framework:
    router:
        # ...
        default_uri: 'https://example.org/my/path/'
```

> 5.1 The `default_uri` option was introduced in Symfony 5.1.

现在当执行命令时你将会得到你预期的结果。

```php
// src/Command/SomeCommand.php
namespace App\Command;

use Symfony\Component\Console\Command\Command;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Routing\Generator\UrlGeneratorInterface;
use Symfony\Component\Routing\RouterInterface;
// ...

class SomeCommand extends Command
{
    private $router;

    public function __construct(RouterInterface $router)
    {
        parent::__construct();

        $this->router = $router;
    }

    protected function execute(InputInterface $input, OutputInterface $output): int
    {
        // generate a URL with no route arguments
        $signUpPage = $this->router->generate('sign_up');

        // generate a URL with route arguments
        $userProfilePage = $this->router->generate('user_profile', [
            'username' => $user->getUserIdentifier(),
        ]);

        // generated URLs are "absolute paths" by default. Pass a third optional
        // argument to generate different URLs (e.g. an "absolute URL")
        $signUpPage = $this->router->generate('sign_up', [], UrlGeneratorInterface::ABSOLUTE_URL);

        // when a route is localized, Symfony uses by default the current request locale
        // pass a different '_locale' value if you want to set the locale explicitly
        $signUpPageInDutch = $this->router->generate('sign_up', ['_locale' => 'nl']);

        // ...
    }
}
```



#### Checking if a Route Exists

在高度动态的应用程序中，可能有必要在生成URL之前检查它是否存在。在这些情况下，不要使用getRouteCollection()方法，因为那会重新生成路由缓存并减慢应用程序的速度。

相反，尝试生成URL，并捕捉路由不存在时抛出的RouteNotFoundException。

```php
use Symfony\Component\Routing\Exception\RouteNotFoundException;

// ...

try {
    $url = $this->router->generate($routeName, $routeParameters);
} catch (RouteNotFoundException $e) {
    // the route is not defined...
}
```





#### Forcing HTTPS on Generated URLs

默认情况下，生成的URL使用与当前请求相同的HTTP scheme。在控制台命令中，如果没有HTTP请求，URL默认使用http。你可以通过每个命令（通过路由器的getContext()方法）或下面这些配置参数在全局范围内改变这一点：

```yaml
# config/services.yaml
parameters:
    router.request_context.scheme: 'https'
    asset.request_context.secure: true
```

在控制台命令之外，使用 schemes 选项来明确定义每个路由的scheme：

```yaml
# config/routes.yaml
login:
    path:       /login
    controller: App\Controller\SecurityController::login
    schemes:    [https]
```

这里为登录路径生成的URL将始终使用HTTPS。这意味着，当使用Twig函数path()生成URL时，如果原始请求的HTTP scheme 与路由使用的sheme不同，你可能会得到一个绝对URL而不是相对URL。

```twig
{# if the current scheme is HTTPS, generates a relative URL: /login #}
{{ path('login') }}

{# if the current scheme is HTTP, generates an absolute URL to change
   the scheme: https://example.com/login #}
{{ path('login') }}
```

scheme requirement也适用于传入的请求，如果你试图用HTTP访问/login URL，你会自动被重定向到相同的URL，但采用HTTPS scheme。

如果你想强制一组路由使用HTTPS，你可以在导入它们时定义默认方案。下面的例子在所有定义为注解的路由上强制使用HTTPS。

```yaml
# config/routes/annotations.yaml
controllers:
    resource: '../../src/Controller/'
    type: annotation
    defaults:
        schemes: [https]
```

提示：Security件提供了另一种方式（[another way to enforce HTTP or HTTPS](https://symfony.com/doc/5.4/security/force_https.html) ），通过request_channel设置强制执行HTTP或HTTPS。



### Troubleshooting

下面是你在使用路由时可能看到的一些常见错误。

控制器 "App\Controller\BlogController::show() "要求你为"$slug "参数提供一个值。这发生在当你的控制器方法有一个参数（例如$slug）。

```php
public function show(string $slug): Response
{
    // ...
}
```

但你的路由中没有{slug}参数（例如，它是/blog/show）。所以在你的路由路径中添加一个{slug}：/blog/show/{slug}或者给参数一个默认值（即$slug = null）。

缺少一些强制性参数（"slug"）来生成路由 "blog_show "的URL。

这意味着你试图为blog_show路由生成一个URL，但你没有传递一个slug值（这是必须的，因为它在路由路径中有一个{slug}参数）。要解决这个问题，在生成路由时要传递一个slug值：

```php
$this->generateUrl('blog_show', ['slug' => 'slug-value']);
```

或者在Twig：

```
{{ path('blog_show', {slug: 'slug-value'}) }}
```







### Learn more about Routing

- [How to Create a custom Route Loader](https://symfony.com/doc/5.4/routing/custom_route_loader.html)
- [Looking up Routes from a Database: Symfony CMF DynamicRouter](https://symfony.com/doc/5.4/routing/routing_from_database.html)



