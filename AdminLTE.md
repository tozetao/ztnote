## 依赖
AdminLTE2主要依赖于bootstartp3和jquery1.11俩个框架，其他的插件可以自己选择。

## 布局
### 1. 页面布局
页面布局包括4个主要部分：
- 容器（wrapper）：.wrapper div包裹了整个网站
- 主头部（main header）:.main-header类包含logo和导航条
- 侧边栏（Sidebar）：.sidebar-wrapper类包含用户板块和导航菜单
- 内容快（Content）：.content-wrapper类包含页眉和内容

### 2. 布局选项
LTE2有4个布局选项，在pages/layout目录下可以找到范例。

### 3. 皮肤


## 组件

### 1. Main-Header
Main-Header有俩种展示方式的：
- 普通布局
- 顶级导航布局

### 2. 侧边栏
侧边栏位于sidebar-menu类下，一级目录是用li元素，配合treeview类控制，次级目录是ul元素配合treeview-menu类使用。

如果你希望菜单是被访问的，只需要在菜单上增加active类即可。


### 3. 控制侧边栏组件
控制侧边栏是指右边的侧边栏，一般将其代码放置在页脚后面，它必须包裹在.warpper div中。

该侧边栏有俩种样式


## JS说明
- <script src="dist/js/adminlte.min.js"></script>
adminlte核心JS文件

- <script src="dist/js/pages/dashboard.js"></script>
仪表盘JS文件案例，主要用于初始化一些插件，例如绘图插件、日期插件等。

- <script src="dist/js/demo.js"></script>
LTE演示脚本