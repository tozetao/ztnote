### 资源管理

允许将同个组件的js、css注册成一个别名，比如：

```php
use Dact\Admin\Admin;

Admin::assert()->alias('@editor', [
    'js' => ['a.js', 'b.js'],
    'css' => ['a.css', 'b.css']
]);
```

使用时可以加载整个组件或单中类型的资源文件，比如：

```php
Admin::js('@editor');	// 只加载js文件
Admin:css('@editor');	// 只加载css文件
// 注：当然你也可以不通过别名，使用相对路径或绝对路径去加载资源文件。
```

如果你喜欢在视图文件中加载资源，Dact也提供了admin_assert()辅助函数在加载资源。

```php
<!-- 加载js -->
<script src="{{ admin_assert('admin/a.js) }}"></script>
<!-- 加载css-->
<link ref="stylesheet" href="{{ admin_assert('admin/a.css) }}"></link>
```



### Dact Admin的视图

可以将js、css、html的代码都整合到一个视图文件中，比如：

```php
<div class="editor">
    hello world.
    <div class="my-class">
        this is body.
    </div>
</div>

<!-- 自定义css样式 -->
<style>
    .my-class {
        color: blue;
    }
</style>

<!-- @editor是提前定义好的资源组件别名，这里会自动进行加载。 -->
<script require="@editor">
    // showPage1();
    // 自定义js代码
    Dact.Ready(function() {
        console.log("hi");
    });
</script>
```

最后再通过admin_view辅助函数去加载该视图文件即可。



### Grid

Grid用于显示表格之类的数据，支持各种复杂的搜索。

使用时需要绑定好对应的数据源，同时设置要显示的列，要搜索的条件，Grid可以帮你自动生成后端的查询代码以及前端要显示的界面。

fix：

- 自定义行功能按钮
- 自定义Filter查询条件

- 自定义Grid工具栏
- scope范围查询、复杂查询、selectTable





### Form

fix：

- 文件上传



### 权限





### 需求

- 整合论坛

- 线性表怎么配置？多条曲线对比，增加日期提示。

- Grid预设搜索条件

- 怎么在行中添加功能按钮？

- timestamp可以用到索引吗？

- pajax
- dact admin设计原理





关于权限的设计

在larabbs中，授权是通过Policy实现的，Policy定义了授权动作必须满足的要求或者说授权规则，在满足要求下授权才成功。

比如管理员就可以编辑、删除帖子和评论，只有自己发布的评论或帖子，用户才能够编辑和删除，这都是通过Policy来实现的。而对于管理员，会在Policy执行之前判断用户是否管理员，如果是就授权通过，否则拒绝。

- Policy

    更多的是针对业务规则上的授权判断。在执行某个动作时，会判断执行该动作的用户是否满足要求。

对于管理后台，给控制器的每个方法都加上Policy是很繁琐的事情，所以还是倾向于一个路由对应一个权限，授权时判断用户是否拥有这个权限就可以了。
