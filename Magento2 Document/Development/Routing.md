在web应用程序中，路由是将URL请求中的数据提供给适当的类进行处理的行为。Magento路由使用以下流程：

pub/index.php => Application => Front controller => Routing => Controller Action.



FrontController class

FrontCntroller类通过RouterList类提供的路由列表进行搜索，直到匹配到一个可以处理请求的路由。当FrontController找到一个匹配的路由时，它将请求分派给路由返回的一个动作类。

如果FrontController不能找到处理请求的路由，它就使用默认的路由。



Router class

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

