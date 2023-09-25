Hooks are a way for one piece of code to interact/modify another piece of code at specific, pre-defined spots. They make up the foundation for how plugins and themes interact with WordPress Core, but they’re also used extensively by Core itself.

钩子是一段代码在预定义的特定位置与另一段代码交互/修改的一种方法。它们是插件和主题与 WordPress 核心进行交互的基础，同时也被核心广泛使用。

There are two types of hooks: [Actions](https://developer.wordpress.org/plugins/hooks/actions/) and [Filters](https://developer.wordpress.org/plugins/hooks/filters/). To use either, you need to write a custom function known as a `Callback`, and then register it with a WordPress hook for a specific action or filter.

钩子有两种类型：Actions和Filters。要使用这两种钩子，您需要编写一个称为 "回调"（Callback）的自定义函数，然后将其注册到WordPress 钩子中特定的Action或Filter。

[Actions](https://developer.wordpress.org/plugins/hooks/actions/) allow you to add data or change how WordPress operates. Actions will run at a specific point in the execution of WordPress Core, plugins, and themes. Callback functions for Actions can perform some kind of a task, like echoing output to the user or inserting something into the database. Callback functions for an Action do not return anything back to the calling Action hook.

Actions允许您添加数据或更改 WordPress 的运行方式。Actions将在 WordPress 核心、插件和主题的特定执行点运行。Actions的回调函数可以执行某种任务，如向用户回声输出或向数据库插入某些内容。Action的回调函数不会向调用的Action钩子返回任何内容。

[Filters](https://developer.wordpress.org/plugins/hooks/filters/) give you the ability to change data during the execution of WordPress Core, plugins, and themes. Callback functions for Filters will accept a variable, modify it, and return it. They are meant to work in an isolated manner, and should never have [side effects](https://en.wikipedia.org/wiki/Side_effect_(computer_science)) such as affecting global variables and output. Filters expect to have something returned back to them.

Filters让您能够在 WordPress 核心、插件和主题执行期间更改数据。Filters的回调函数将接受一个变量，对其进行修改并返回。这些回调函数以孤立的方式工作，绝不会产生副作用，例如影响全局变量和输出。Filters希望返回一些内容。

WordPress provides many hooks that you can use, but you can also [create your own](https://developer.wordpress.org/plugins/hooks/custom-hooks/) so that other developers can extend and modify your plugin or theme.

WordPress 提供了许多钩子供您使用，但您也可以创建自己的钩子，以便其他开发人员可以扩展和修改您的插件或主题。



## [Actions vs. Filters](https://developer.wordpress.org/plugins/hooks/#actions-vs-filters)

The main difference between an action and a filter can be summed up like this:

Action和Filter的主要区别可以这样概括：

- an action takes the info it receives, does something with it, and returns nothing. In other words: it *acts* on something and then exits, returning nothing back to the calling hook.

  Action接收信息，对信息进行处理，然后什么也不返回。换句话说：它对某些信息采取行动，然后退出，不返回任何信息给调用钩子。

- a filter takes the info it receives, modifies it somehow, and returns it. In other words: it *filters* something and passes it back to the hook for further use.

  Filter接收信息，以某种方式对其进行修改，然后返回。换句话说：它过滤某些信息，并将其返回给钩子供进一步使用。

Said another way:

换一种说法：

- an action interrupts the code flow to do something, and then returns back to the normal flow without modifying anything;

  Action会中断代码流来做一些事情，然后返回正常流程，不会修改任何内容；

- a filter is used to modify something in a specific way so that the modification is then used by code later on.

  filter用于以特定方式修改某些内容，以便以后的代码可以使用修改后的内容。

The *something* referred to is the parameter list sent via the hook definition. More on this in later sections.

所谓的 "内容 "就是通过发送钩子定义的参数列表。关于这一点，我们将在后面的章节中详细介绍。



## [More Resources](https://developer.wordpress.org/plugins/hooks/#more-resources)

- [Filter Reference](https://codex.wordpress.org/Plugin_API/Filter_Reference)
- [Action Reference](https://codex.wordpress.org/Plugin_API/Action_Reference)