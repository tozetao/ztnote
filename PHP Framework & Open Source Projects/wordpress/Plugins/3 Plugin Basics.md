## [Getting Started](https://developer.wordpress.org/plugins/plugin-basics/#getting-started)

At its simplest, a WordPress plugin is a PHP file with a WordPress plugin header comment. It’s highly recommended that you create a directory to hold your plugin so that all of your plugin’s files are neatly organized in one place.

最简单地说，WordPress 插件就是一个带有 WordPress 插件头注释的 PHP 文件。强烈建议您创建一个存放插件的目录，以便将插件的所有文件整齐地组织在一个地方。

To get started creating a new plugin, follow the steps below.

要开始创建新插件，请按照以下步骤操作。

1. Navigate to the WordPress installation’s **wp-content** directory.

   导航到 WordPress 安装的 wp-content 目录。

2. Open the **plugins** directory.

   打开插件目录。

3. Create a new directory and name it after the plugin (e.g. `plugin-name`).

   创建一个新目录，并以插件命名（如 plugin-name）。

4. Open the new plugin’s directory.

   打开新插件目录。

5. Create a new PHP file (it’s also good to name this file after your plugin, e.g. `plugin-name.php`).

   创建一个新的 PHP 文件（也可以用插件的名字命名该文件，如 plugin-name.php）。

Here’s what the process looks like on the Unix command line（下面是 Unix 命令行下的过程）：

```bash
wordpress $ cd wp-content
wp-content $ cd plugins
plugins $ mkdir plugin-name
plugins $ cd plugin-name
plugin-name $ vi plugin-name.php
```

In the example above, `vi` is the name of the text editor. Use whichever editor that is comfortable for you.

在上例中，`vi` 是文本编辑器的名称。请使用你喜欢的编辑器。

Now that you’re editing your new plugin’s PHP file, you’ll need to add a plugin header comment. This is a specially formatted PHP block comment that contains metadata about the plugin, such as its name, author, version, license, etc. The plugin header comment must comply with the [header requirements](https://developer.wordpress.org/plugins/the-basics/header-requirements/), and at the very least, contain the name of the plugin.

现在您正在编辑新插件的 PHP 文件，需要添加一个插件头注释。这是一个特殊格式的 PHP 块注释，包含插件的元数据，如名称、作者、版本、许可证等。插件头注释必须符合 [header requirements](https://developer.wordpress.org/plugins/the-basics/header-requirements/)，至少要包含插件名称。

Only one file in the plugin’s folder should have the header comment — if the plugin has multiple PHP files, only one of those files should have the header comment.

如果插件包含多个 PHP 文件，那么只有其中一个文件有头部注释。

After you save the file, you should be able to see your plugin listed in your WordPress site. Log in to your WordPress site, and click **Plugins** on the left navigation pane of your WordPress Admin. This page displays a listing of all the plugins your WordPress site has. Your new plugin should now be in that list!

保存文件后，您就可以在 WordPress 网站上看到您的插件了。登录 WordPress 网站，点击 WordPress 管理器左侧导航窗格中的**插件**。该页面会显示 WordPress 网站上所有插件的列表。您的新插件现在应该在列表中！



## [Hooks: Actions and Filters](https://developer.wordpress.org/plugins/plugin-basics/#hooks-actions-and-filters)

WordPress hooks allow you to tap into WordPress at specific points to change how WordPress behaves without editing any core files.

WordPress钩子允许您在特定的点上接入WordPress，在不编辑任何核心文件的情况下改变WordPress的行为方式。

There are two types of hooks within WordPress: *actions* and *filters*. Actions allow you to add or change WordPress functionality, while filters allow you to alter content as it is loaded and displayed to the website user.

WordPress中有两种钩子：actions和filters。Actions允许您添加或更改 WordPress 的功能，而filters允许您更改加载并显示给网站用户的内容。

Hooks are not just for plugin developers; hooks are used extensively to provide default functionality by WordPress core itself. Other hooks are unused place holders that are simply available for you to tap into when you need to alter how WordPress works. This is what makes WordPress so flexible.

钩子不仅适用于插件开发人员；WordPress 内核本身也广泛使用钩子来提供默认功能。其他钩子则是未使用的占位符，当您需要改变WordPress的工作方式时，可以使用它们。这就是WordPress的灵活之处。



### [Basic Hooks](https://developer.wordpress.org/plugins/plugin-basics/#basic-hooks)

The 3 basic hooks you’ll need when creating a plugin are the [register_activation_hook()](https://developer.wordpress.org/reference/functions/register_activation_hook/) , the [register_deactivation_hook()](https://developer.wordpress.org/reference/functions/register_deactivation_hook/) , and the [register_uninstall_hook()](https://developer.wordpress.org/reference/functions/register_uninstall_hook/) .

创建插件时需要使用 3 个基本钩子：[register_activation_hook()](https://developer.wordpress.org/reference/functions/register_activation_hook/) 和[register_deactivation_hook()](https://developer.wordpress.org/reference/functions/register_deactivation_hook/)。以及 [register_uninstall_hook()](https://developer.wordpress.org/reference/functions/register_uninstall_hook/) 。

The [activation hook](https://developer.wordpress.org/plugins/the-basics/activation-deactivation-hooks/) is run when you *activate* your plugin. You would use this to provide a function to set up your plugin — for example, creating some default settings in the `options` table.

[activation hook](https://developer.wordpress.org/plugins/the-basics/activation-deactivation-hooks/) 在激活插件时运行。你可以使用它来提供一个设置插件配置的功能 -- 例如，在 `options` 表中创建一些默认设置。

The [deactivation hook](https://developer.wordpress.org/plugins/the-basics/activation-deactivation-hooks/) is run when you *deactivate* your plugin. You would use this to provide a function that clears any temporary data stored by your plugin.

[deactivation hook](https://developer.wordpress.org/plugins/the-basics/activation-deactivation-hooks/) 在停用插件时运行。你可以使用它来提供一个清除插件存储的临时数据的功能。

These [uninstall methods](https://developer.wordpress.org/plugins/the-basics/uninstall-methods/) are used to clean up after your plugin is *deleted* using the WordPress Admin. You would use this to delete all data created by your plugin, such as any options that were added to the `options` table.

这些 [uninstall methods](https://developer.wordpress.org/plugins/the-basics/uninstall-methods/) 用于在使用 WordPress 管理器*删除*您的插件后进行清理。您可以使用它来删除插件创建的所有数据，例如添加到 `options` 表中的任何选项。



### [Adding Hooks](https://developer.wordpress.org/plugins/plugin-basics/#adding-hooks)

You can add your own, custom hooks with [do_action()](https://developer.wordpress.org/reference/functions/do_action/) , which will enable developers to extend your plugin by passing functions through your hooks.

您可以使用 [do_action()](https://developer.wordpress.org/reference/functions/do_action/) 添加自己的自定义钩子，这样开发人员就可以通过你的钩子传递函数来扩展您的插件。



### [Removing Hooks](https://developer.wordpress.org/plugins/plugin-basics/#removing-hooks)

You can also use invoke [remove_action()](https://developer.wordpress.org/reference/functions/remove_action/) to remove a function that was defined earlier. For example, if your plugin is an add-on to another plugin, you can use [remove_action()](https://developer.wordpress.org/reference/functions/remove_action/) with the same function callback that was added by the previous plugin with [add_action()](https://developer.wordpress.org/reference/functions/add_action/) . The priority of actions is important in these situations, as [remove_action()](https://developer.wordpress.org/reference/functions/remove_action/) would need to run after the initial [add_action()](https://developer.wordpress.org/reference/functions/add_action/) .

您还可以调用 [remove_action()](https://developer.wordpress.org/reference/functions/remove_action/) 来移除先前定义的函数。例如，如果您的插件是另一个插件的附加组件，您可以使用 [remove_action()](https://developer.wordpress.org/reference/functions/remove_action/)移除前一个插件通过 [add_action()](https://developer.wordpress.org/reference/functions/add_action/) 添加的回调函数。在这种情况下，动作的优先级很重要，因为 [remove_action()](https://developer.wordpress.org/reference/functions/remove_action/) 需要在 [add_action()](https://developer.wordpress.org/reference/functions/add_action/) 初始之后运行。

You should be careful when removing an action from a hook, as well as when altering priorities, because it can be difficult to see how these changes will affect other interactions with the same hook. We highly recommend testing frequently.

在移除action hooks以及更改优先级时应小心谨慎，因为很难确定这些更改会如何影响与同一钩子的其他交互。我们强烈建议经常进行测试。

You can learn more about creating hooks and interacting with them in the [Hooks](https://developer.wordpress.org/plugin/hooks/) section of this handbook.

您可以在本手册的 [钩子](https://developer.wordpress.org/plugin/hooks/) 部分了解有关创建钩子和与钩子交互的更多信息。



## [WordPress APIs](https://developer.wordpress.org/plugins/plugin-basics/#wordpress-apis)

Did you know that WordPress provides a number of [Application Programming Interfaces (APIs)](https://make.wordpress.org/core/handbook/core-apis/)? These APIs can greatly simplify the code you need to write in your plugins. You don’t want to reinvent the wheel, especially when so many people have done a lot of the work and testing for you.

您知道 WordPress 提供了许多 [应用程序编程接口 (API)](https://make.wordpress.org/core/handbook/core-apis/)吗？这些 API 可以大大简化您在插件中需要编写的代码。您不想重新发明轮子，尤其是有这么多人已经为您做了大量的工作和测试。

The most common one is the [Options API](https://codex.wordpress.org/Options_API), which makes it easy to store data in the database for your plugin. If you’re thinking of using [cURL](https://en.wikipedia.org/wiki/CURL) in your plugin, the [HTTP API](https://codex.wordpress.org/HTTP_API) might be of interest to you.

最常见的是[Options API](https://codex.wordpress.org/Options_API)，它可以让您轻松地在数据库中为您的插件存储数据。如果您想在插件中使用 [cURL](https://en.wikipedia.org/wiki/CURL)，那么 [HTTP API](https://codex.wordpress.org/HTTP_API) 可能会引起您的兴趣。

Since we’re talking about plugins, you’ll want to study the [Plugin API](https://codex.wordpress.org/Plugin_API). It has a variety of functions that will assist you in developing plugins.

既然我们谈论的是插件，你就应该研究一下 [Plugin API](https://codex.wordpress.org/Plugin_API)。它有各种功能，可以帮助您开发插件。





## [How WordPress Loads Plugins](https://developer.wordpress.org/plugins/plugin-basics/#how-wordpress-loads-plugins)

When WordPress loads the list of installed plugins on the Plugins page of the WordPress Admin, it searches through the `plugins` folder (and its sub-folders) to find PHP files with WordPress plugin header comments. If your entire plugin consists of just a single PHP file, like [Hello Dolly](https://wordpress.org/plugins/hello-dolly/), the file could be located directly inside the root of the `plugins` folder. But more commonly, plugin files will reside in their own folder, named after the plugin.

当 WordPress 在 WordPress 管理器的插件页面上加载已安装插件的列表时，它会搜索 `plugins` 文件夹（及其子文件夹），以找到带有 WordPress 插件头部注释的 PHP 文件。如果您的整个插件只包含一个 PHP 文件，如 [Hello Dolly](https://wordpress.org/plugins/hello-dolly/)，该文件可能直接位于`plugins`根目录下。但更常见的情况是，插件文件存放在以自己的插件命名的文件夹中。



## [Sharing your Plugin](https://developer.wordpress.org/plugins/plugin-basics/#sharing-your-plugin)

Sometimes a plugin you create is just for your site. But many people like to share their plugins with the rest of the WordPress community. Before sharing your plugin, one thing you need to do is [choose a license](https://opensource.org/licenses/category). This lets the user of your plugin know how they are allowed to use your code. To maintain compatibility with WordPress core, it is recommended that you pick a license that works with GNU General Public License (GPLv2+).

有时，您创建的插件只是为了您的网站。但很多人喜欢与 WordPress 社区的其他人分享他们的插件。在分享您的插件之前，您需要做的一件事是[选择许可证](https://opensource.org/licenses/category)。这可以让您的插件用户知道他们可以如何使用您的代码。为保持与 WordPress 核心的兼容性，建议您选择与 GNU 通用公共许可证 (GPLv2+) 兼容的许可证。