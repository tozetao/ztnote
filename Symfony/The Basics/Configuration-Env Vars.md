### Configration Files

Symfony应用使用放在config/目录下的文件进行配置，它的默认结构为：

```
your-project/
├─ config/
│  ├─ packages/
│  ├─ bundles.php
│  ├─ routes.yaml
│  └─ services.yaml
├─ ...
```

routes.yaml定义了路由配置，services.yaml配置了服务容器中的服务，bundles.php文件在你的应用中启用或禁用packages。

你主要在config/packages目录下工作。这个目录存储了你应用中安装的每个包的配置。Packages（Symfony也叫做bundles，其他项目可能叫做"plugins/modules"）为你的项目添加随时可用的功能。

当使用Symfony应用中默认启用的Symfony Flex时，packages更新buldes.php文件并在安装期间会在config/packages目录中自动创建新的配置文件。例如，这是通过“API Platform”包创建的默认文件：

```
# config/packages/api_platform.yaml
api_platform:
    mapping:
        paths: ['%kernel.project_dir%/src/Entity']
```

对于新手来说将配置文件切割成许多小的配置文件是令人生畏的。然后你很快就会习惯它们，并且在包安装后你很少会去更改这些文件

Tip：要学习更多关于可用的配置选项，查阅配置手册（https://symfony.com/doc/current/reference/index.html），或者运行config:dump-reference命令。



#### Configuration Formats

与其他框架不同的是，Symfony不强加给你一个特定的格式来配置你的应用程序。Symfony让你在YAML、XML和PHP之间进行选择，在整个Symfony文档中，所有的配置例子都会以这三种格式显示。

注：从5.1版本开始，Symfony默认仅加载以YAML格式定义的配置文件。如果你以PHP或XML格式定义配置，需要通过重写configureContainer()方法来更新src/Kernel.php文件，以添加对.xml和.php文件的支持。

```php
// src/Kernel.php
use Symfony\Component\DependencyInjection\Loader\Configurator\ContainerConfigurator;
use Symfony\Component\HttpKernel\Kernel as BaseKernel;

class Kernel extends BaseKernel
{
    // ...
    private function configureContainer(ContainerConfigurator $container): void
    {
        $configDir = $this->getConfigDir();

        $container->import($configDir.'/{packages}/*.{yaml,php}');
        $container->import($configDir.'/{packages}/'.$this->environment.'/*.{yaml,php}');

        if (is_file($configDir.'/services.yaml')) {
            $container->import($configDir.'/services.yaml');
            $container->import($configDir.'/{services}_'.$this->environment.'.yaml');
        } else {
            $container->import($configDir.'/{services}.php');
        }
    }
}
```

格式之间没有任何实际差别。事实上，在程序运行之前Symfony会将它们转换并缓存为PHP，所以它们之间没有任何性能差异。

当安装软件包后会使用YAML作为默认格式，是因为它间接可读。以下是不同格式之间的优缺点：

- YAML

  简单、干净、可读，但不是所有的IDE都支持它的自动提示和验证。学习YAML语法（https://symfony.com/doc/current/components/yaml/yaml_format.html）。

- XML：大多数IDE都支持自动完成/验证，并且可以被PHP原生解析，但有时它生成的配置被认为过于冗长。学习XML的语法。

- PHP：非常强大，它允许你用数组或ConfigBuilder创建动态配置。



#### Importing Configration Files

有时在多个配置文件中会存在相同的配置值，你可以把这些相同的配置值定义为一个参数（parameter），就像一个可以重用的配置值，而不是重复使用它。按照惯例，参数（parameter）是被定义在config/services.yaml文件的parameters key下面的：

```yaml
# config/services.yaml
parameters:
	app.admin_email: 'something@example.com'
```

注：当使用XML配置，在<parameter>标签中的值不会被修饰过滤。这意味着下面参数的值为'\n    something@example.com\n'：

```xml
<parameter key="app.admin_email">
    something@example.com
</parameter>
```

一旦定义，你就可以使用特定语法在任意配置文件中引用这些参数，使用俩个%符号去包裹参数名来进行引用（比如%app.admin_email%）。

```yaml
# config/packages/some_package.yaml
some_package:
    # any string surrounded by two % is replaced by that parameter value
    email_address: '%app.admin_email%'
```

如果一些参数值包含%字符，你需要通过添加一个%来转移它，这样Symfony就不会认为它是对参数名的引用。

```yaml
# config/services.yaml
parameters:
    # Parsed as 'https://symfony.com/?foo=%s&amp;bar=%d'
    url_pattern: 'https://symfony.com/?foo=%%s&amp;bar=%%d'
```

由于参数的解析方式，你不能使用它们来动态的构建导入的路径。这意味着以下的方式是行不通的：

```php
// config/services.php
$loader->import('%kernel.project_dir%/somefile.yaml');
```

在Symfony应用中配置参数是很普遍的。一些package甚至会定义自己的配置参数（例如当安装translation package，一个新的locale参数会被添加到config/services.yaml文件中）。



### Configuration Environments

你只有一个应用，但无论你是否意识到，你需要应用在不同环境下有不同表现。

- 当处于开发时，你想要记录所有日志并显示调试工具。
- 在部署到生产环境之后，为了性能你想要该引用可以得到优化，并仅记录错误。

Symfony会使用处于config/packages目录下的文件去配置应用服务。换句话说，你可以通过切换加载的配置文件来改变应用行为。这就是Symfony的配置环境（configuration environments）。

一个典型的Symfony应用有3个环境：dev（针对本地开发）、prod（针对生产服务）、test（针对automated tests）。当运行应用时，Symfony会按照这个顺序加载配置文件（后一个文件可以覆盖前一个文件中配置的值）：

- config/packages/*.yaml（还有.xml和.php文件）
- config/packages/<environment-name\>/*.yaml（包括.xml和.php文件）
- config/services.yaml
- config/services_\<environment-name>.yaml（包括services\_\<environment-name\>.xml和services\_\<environment-name\>.php文件）

以默认安装的框架（framework）包为例：

- 首先，在所有环境中config/packages/framework.yaml会被加载，并且会配置框架一些选项。
- 在prod环境中，由于没有config/packages/prod/framework.yaml文件，所以不会有额外的设置。
- 在dev环境中，同样也没有config/packages/dev/framework.yaml文件。
- 在test环境中，config/packages/test/framework.yaml文件被加载以覆盖之前在config/packages/framework.yaml中配置的一些设置。

在显示中每个环境与其他环境仅有一些不同。这意味着所有环境都有一个共同的基础配置，这些配置位于/config/packages目录下的文件中。

注：在一个单一的配置文件中你可以使用when关键字为不同环境定义选项：

```yaml
# config/packages/webpack_encore.yaml
webpack_encore:
    # ...
    output_path: '%kernel.project_dir%/public/build'
    strict_mode: true
    cache: false

# cache is enabled only in the "prod" environment
when@prod:
    webpack_encore:
        cache: true

# disable strict mode only in the "test" environment
when@test:
    webpack_encore:
        strict_mode: false

# YAML syntax allows to reuse contents using "anchors" (&some_name) and "aliases" (*some_name).
# In this example, 'test' configuration uses the exact same configuration as in 'prod'
when@prod: &webpack_prod
    webpack_encore:
        # ...
when@test: *webpack_prod
```



疑问：也就是说，除了共有的配置文件，不同的环境会按照规则去加载配置文件。？





#### Selecting the Active Environment

Symfony应用会在项目根目录带有一个.env文件。该文件被用于去定义环境变量的值。

打开.env文件（如果你创建了.env.local文件那更好），编辑APP_ENV变量的值去改变应用运行时选择的环境。例如在生产环境运行：

```
# .env (or .env.local)
APP_ENV=prod
```

这个值会被web应用或控制台应用使用。然而你也可以在运行命令时通过设置APP_ENV的值去重写它：

```
# Use the environment defined in the .env file
php bin/console command_name

# ignore the .env file and run this command in producation
APP_ENV=prod php bin/console command_name
```



#### Creating a New Environment

Symfony默认提供的三个环境对大多数项目来说已经足够了，但你也可以定义你自己的环境。例如，你可以这样定义一个staging环境，客户可以在进入生产前测试项目。

- 使用环境的名字创建一个配置目录，本案例为config/packages/staging/。
- 在config/packages/staging目录中添加需要的配置文件，以定义新环境的应用行为。Symfony首先加载config/packages/*.yaml文件，所以你只需要配置这些文件的差异。
- 如上一节所示使用APP_ENV环境变量选择staging环境。

提示：环境之间彼此相似是很常见的，所以你可以在config/packages/<environment-name>/目录之间使用符号链接来重复使用相同的配置。

你可以使用环境变量，而不是创建新的环境，就像下一节所说的那样。通过这种方式你就可以使用相同的应用程序和环境（例如prod），但由于是基于环境变量的配置你可以改变它的行为（例如，同在不同的场景中运行应用程序：staging、quality assurance、client review等）。



#### Configuration Based on Environment Variables

根据应用运行的环境去使用环境变量进行配置是一种常见做法（例如，数据库凭证通常在生产中与你的本地机器不同）。如果这些值是敏感的，你甚至可以把它们加密为密钥。

你可以使用特殊语法%env(ENV_VAR_NAME)%来引用环境变量。这些选项的值在运行时被解析（每次请求只解析一次，以不影响性能）。

这个例子显示了如何使用env var来配置数据库连接。

```php
// config/packages/doctrine.php
namespace Symfony\Component\DependencyInjection\Loader\Configurator;

return static function (ContainerConfigurator $container) {
    $container->extension('doctrine', [
        'dbal' => [
            // by convention the env var names are always uppercase
            'url' => '%env(resolve:DATABASE_URL)%',
            // or
            'url' => env('DATABASE_URL')->resolve(),
        ],
    ]);
};
```

注1：env()配置器实在5.3中引入的。在php配置文件中，它可以根据处理器名自动完成方法（例如，env('SOME_VAR')->default('foo')）。

注2：env vars的值只能是字符串，但是Symfony包含一些env var processors去转换它们的内容（例如把一个字符串值转为一个整数）。

定义一个env var，你需要做以下步骤：

- 在.env文件中添加值。
- 将值加密为密钥
- 在你的web服务器或者shell中将该值设置为真是的环境变量。

警告：要注意导出$\_SERVER和$\_ENV变量的内容或输出phpinfo()的内容会显示环境变量的值，暴露敏感信息，如数据库凭证。环境变量的值也会在Symfony剖析器的网页界面上暴露出来。在实践中，这不应该是一个问题，因为网络剖析器在生产中绝不能被启用。



#### Configuring Environment Variables in .env Files

Symfony提供了一种方便的方法，可以在项目根目录的.env文件中定义环境变量，而不是在你的shell或Web服务器中定义。

.env文件在每个请求中都被读取和解析，它的env vars会被添加到$\_ENV和$\_SERVER PHP变量中。任何已有的env vars都不会被.env文件中定义的值所覆盖，所以你可以把两者结合起来。

例如，要定义本文前面显示过的DATABASE_URL环境变量，你可以添加：

```
# .env
DATABASE_URL="mysql://db_user:db_password@127.0.0.1:3306/db_name"
```

这个文件应该被提交到你的版本库，并且（由于这个事实）应该只包含适合本地开发的 "默认 "值。这个文件不应该包含生产环境的值。

除了你自己的环境变量外，这个.env文件还包含你的应用程序中安装的第三方包所定义的环境变量（它们是由Symfony Flex在安装包时自动添加的）。

注：由于.env文件在每次请求时都会被读取和解析，如果你使用Docker，就不需要清除Symfony缓存或重新启动PHP容器。



#### .env File Syntax

通过#前缀来添加注释：

```
# database credentials
DB_USER=root
DB_PASS=pass # this is the secret password
```

通过在变量前面加上$前缀，在一个值中使用环境变量的值：

```
DB_USER=root
DB_PASS=${DB_USER}pass # include the user as a password prefix
```

注：当某些环境变量依赖于其他环境变量的值时，这个顺序很重要。在上面的例子中，DB_PASS必须被定义在DB_USER之后。此外，如果你定义了多个.env文件并把DB_PASS放在前面，它的值将取决于其他文件中定义的DB_USER值，而不是这个文件中定义的值。



在未设置环境变量下设置默认值：

```
DB_USER=
DB_PASS=${DB_USER:-root}pass # results in DB_PASS=rootpass
```

通过$()嵌入命令（windows不支持）:

```
START_TIME=$(date)
```

