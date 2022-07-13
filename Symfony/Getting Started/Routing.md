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
#### Parameter Conversion
#### Special Parameters
#### Extra Parameters
#### Slash Characters in Route Parameters