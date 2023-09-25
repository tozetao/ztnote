你可以用任何Web服务器（Apache、nginx、内部PHP Web服务器等）运行Symfony应用程序。然而，Symfony提供了它自己的Web服务器，使你在开发应用程序时更有成效。

虽然这个服务器并不打算用于生产环境，但它支持HTTP/2、TLS/SSL、自动生成安全证书、本地域名和许多其他功能，这些功能在开发Web项目时迟早会用到。此外，该服务器不与Symfony绑定，你也可以将其用于任何PHP应用程序，甚至用于HTML或单页应用程序。



### Installation

当你安装Symfony时，Symfony服务器是创建的symfony二进制文件的一部分，支持Linux、masOS和windows。

注：你可以在[symfony-cli/symfony-cli GitHub repository](https://github.com/symfony-cli/symfony-cli)github仓库中查看和贡献源代码。



### Getting Started

Symfony服务器在每个项目中只启动一次，所以最终你可能会有几个服务实例（每个实例监听不同端口）。这是Symfony project的常见工作流程。

```
$ cd my-project/
$ symfony server:start

[OK] Web server listening on http://127.0.0.1:....
...

$ symfony open:local
```

运行服务这种方式会在控制显示日志信息，这是你不能去运行其他命令。当然你可以在后台运行服务：

```
$ cd my-project/

# start the server in the background
$ symfony server:start -d

# continue working and running other commands....

# show the latest log messages
$ symfony server:log
```



### Enabling PHP-FPM

当服务器启动时，它依次检查web/index_dev.php, web/index.php, public/app_dev.php, public/app.php。如果找到一个，服务器将自动启动并启用PHP-FPM。否则，服务器将在没有PHP-FPM的情况下启动，当试图在浏览器中访问一个.php文件时，将显示一个Page not found页面。

注1：PHP-FPM必须安装在本地，以便Symfony服务器使用。

注2：当index.html和前台控制器（如index.php）同时存在时，服务器仍将在启用PHP-FPM的情况下启动，但index.html将优先于前台控制器。这意味着当index.html文件存在于web或public目录中时，index.html将被显示，而不是index.php，后者将显示例如Symfony应用程序。



### Enabling TLS

在本地浏览应用程序的安全版本对于早期发现混合内容的问题，以及仅运行在HTTPS中的库是非常重要的。传统上，这是很痛苦和复杂的设置，但Symfony服务器将一切自动化。首先，运行这个命令。

```
$ symfony server:ca:install
```

该命令创建了一个本地授权证书，并注册到你的系统信任列表中，也会在Firefox中注册（这只对该浏览器有要求），并为localhost和127.0.0.1创建一个默认的证书。换句话说，它为你做了一切。

在使用https代理http浏览你的本地应用之前，停止你的服务并重启它。



### Different PHP Settings Per Project

#### Selecting a Different PHP Version

如果你的电脑上安装了多个PHP版本，你可以在项目根目录中创建一个.php-version文件告诉Symfony你选择的版本。

```
$ cd my-project/

# use a specific PHP version
$ echo 7.4 > .php-version

# use any PHP8.x VERSION AVAILABLE
$ echo 8 > .php-version
```

注：Symfony服务器会遍历目录结构直到根目录，所以你可以创建一个.php-version文件为在同个父级目录下的一组项目去设置同个PHP版本。

如果你不记得你电脑安装的PHP版本可以执行下面这个命令：

```
$ symfony local:php:list
# You'll see all supported SAPIs (CGI, FastCGI, etc.) for each version.
# FastCGI(php-fpm) is used when possible; then CGI (which acts as a FastCGI server as well), and finally, the server falls back to plain CGI.
```



文档还未翻译完成：

https://symfony.com/doc/5.4/setup/symfony_server.html#different-php-settings-per-project