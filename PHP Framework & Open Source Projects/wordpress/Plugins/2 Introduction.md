# Introduction to Plugin Development

Welcome to the Plugin Developer Handbook. Whether you’re writing your first plugin or your fiftieth, we hope this resource helps you write the best plugin possible.

欢迎阅读《插件开发人员手册》。无论您是在编写第一个插件还是第 50 个插件，我们都希望本资源能帮助您编写出最好的插件。

The Plugin Developer Handbook covers a variety of topics — everything from what should be in the plugin header, to security best practices, to tools you can use to build your plugin. It’s also a work in progress — if you find something missing or incomplete, please notify the documentation team in slack and we’ll make it better together.

《插件开发人员手册》涵盖各种主题 -- 从插件头中应该包含的内容到安全最佳实践，再到您可用于构建插件的工具，无所不包。这也是一项正在进行中的工作--如果您发现有遗漏或不完整的内容，请在 slack 中通知文档团队，我们将一起把它做得更好。



## Why We Make Plugins

If there’s one cardinal rule in WordPress development, it’s this: **Don’t touch WordPress core**. This means that you don’t edit core WordPress files to add functionality to your site. This is because WordPress overwrites core files with each update. Any functionality you want to add or modify should be done using plugins.

如果说 WordPress 开发有什么基本原则的话，那就是：不 不要触碰 WordPress 核心。也就是说，不要编辑 WordPress 核心文件来为网站添加功能。这是因为WordPress每次更新都会覆盖核心文件。您想添加或修改的任何功能都应使用插件来完成。

WordPress plugins can be as simple or as complicated as you need them to be, depending on what you want to do. The simplest plugin is a single PHP file. The [Hello Dolly](https://wordpress.org/plugins/hello-dolly/) plugin is an example of such a plugin. The plugin PHP file just needs a [Plugin Header](https://developer.wordpress.org/plugins/the-basics/header-requirements/), a couple of PHP functions, and some [hooks](https://developer.wordpress.org/plugins/hooks/) to attach your functions to.

WordPress 插件可以很简单，也可以很复杂，这取决于您想做什么。最简单的插件就是一个 PHP 文件。Hello Dolly 插件就是这样一个例子。该插件的 PHP 文件只需要一个插件头、几个 PHP 函数和一些hooks，以便将您的函数附加到这些hooks上。

Plugins allow you to greatly extend the functionality of WordPress without touching WordPress core itself.

通过插件，您可以极大地扩展 WordPress 的功能，而无需触及 WordPress 内核本身。

