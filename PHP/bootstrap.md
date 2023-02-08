## bootstrap

### 容器

bootstrap提供了俩种容器，.container和.container-fluid。.container类用于固定宽度，并且支持响应式布局的容器；container-fluid用于100%宽度，会占据全部viewport。

.container的响应式布局是通过media来实现的，它的样式如下：

```css
.container {
    padding-right: 15px;
    padding-left: 15px;
    margin-right: auto;
    margin-left: auto;
}

/* 当宽度大于1200px时，容器宽度1140px */
@media (min-width: 1200px)
.container-xl, .container-lg, .container-md, .container-sm, .container {
    max-width: 1140px;
}

/*  */
@media (min-width: 992px)
.container-lg, .container-md, .container-sm, .container {
    max-width: 960px;
}

/* 当宽度大于768px时，容器宽度为720p x*/
@media (min-width: 768px)
.container-md, .container-sm, .container {
    max-width: 720px;
}

/*当宽度大于576px时，容器宽度为540px*/
@media (min-width: 576px)
.container-sm, .container {
    max-width: 540px;
}
```

.container容器是自动居中的，同时盒子中的内容左右会相距15px。另外可以看到响应式布局的关键在于，样式要能感觉屏幕的宽度做出不同的变化。.container在实现上是将屏幕宽度划分为了4个档次：

- 576px

    设备宽度最小的一档，小于768px，针对手机，这是bootstrap的默认样式，也是移动设备优先的由来。

- min-witdh(768px)，

    小屏幕，比如平板，大于或等于768px，媒体查询变量@screen-sm-min。

- 992px（@screen-md-min）

    中等屏幕，比如桌面显示器，大于或等于992px，

- 1200px

    大屏幕，大于1200px。

案例：

```html
<div class="bs-docs-header">
    <div class="container">
        <h1>这是页面内容</h1>
        <p>fjskdlfjklsdjfkljsdklfjklsdjfklsdjklfjsdkljfklsdjfklsdjkfljdsklf</p>
    </div>
</div>
```

css样式：这里使用了bootstrap，当设备宽度大于768px时做样式的叠加和覆盖。

```css

.bs-docs-header {
    position: relative;
    padding: 30px 0;
    color: #cdbfe3;
    text-align: center;
    text-shadow: 0 1px 0 rgb(0 0 0 / 10%);
    background-color: #6f5499;
    background-image: -webkit-gradient(linear,left top,left bottom,from(#563d7c),to(#6f5499));
    background-repeat: repeat-x;
    
    margin-bottom: 40px;
    font-size: 20px;
}

.bs-docs-header h1 {
    margin-top: 0;
    color: #fff;
}
.bs-docs-header p {
    margin: 0 0 10px;
}

@media (min-width: 768px) {
    .bs-docs-header {
      padding-top: 60px;
      padding-bottom: 60px;
      font-size: 24px;
      text-align: left;
    }
    .bs-docs-header h1 {
      font-size: 60px;
      line-height: 1;
    }
    .bs-docs-header p {
        margin-bottom: 0;
        font-weight: 300;
        line-height: 1.4;
    }
}
```





### 栅格

栅格是将容器划分为多个行，每行等分的12个列的一种布局。

特性：

- 栅格布局也是响应式的布局，row的宽度会随着屏幕宽度而改变，row宽度变化与容器宽度变化是一致的，都是按照4个挡位来设置宽度。
- 几个挡位的样式，主要用于控制当设备宽度大于某个值时，列会组成一行。

列的规格有：

- sm：>=576px生效

- md：>=768px生效

- lg：>=992px生效

这几种规格的样式都是一样的，只不过生效的范围不一样。比如采用sm规则的类样式，当设备宽度>=576px时，列会漂浮起来组成一行，占据外部整个容器。

比如说你只设置了md样式，那么当屏幕宽度小于768px时，列就不会漂浮成一行，而是水平排列在一起。

```html
<!-- 把窗口拉伸到小于768px，看看div是否水平堆在一起了 -->
<div class="row">
    <div class="col-md-5 col-lg-5" style="background-color: red;">红色</div>
    <div class="col-md-3 col-lg-3" style="background-color: green;">绿色</div>
    <div class="col-md-4 col-lg-4" style="background-color: pink;">粉色</div>
</div>
```

注：3与4版本的栅格布局是不一样的，这里讲解的是4版本。





### 表单



## 公共样式

### 排版

bootstrap为标题设置了基本的样式，文字的内容和排榜也做了基本设置，字体大小14px，行高1.428，都赋予给body和p元素。

注：如果要使用标题或文字，先参考这里的是否有实现。



### 辅助类





## media

语法：

```css
@media mediatype and|not|only (media feature) {
    /*css code*/
}
```

- media type：设备类型
- media feature：媒体功能



常见的媒体功能：

- min-width：定义设备的最小可见区域，当设备宽度大于最小可见区域的值时样式生效。

- max-widht：定义设备的最大可见区域，即在该width范围内生效。





## 心得

常用的布局元素和class样式有：

- 导航

    提供水平排列的多种选项。

- 列表

    列表用于展示多条内容，一般会用ul来表示。通常列表项的内容会比较复杂。比如像论坛的主题列表，这种可以使用media类来处理。

- card（块）

    提供了一个带边框，有头部、内容体和尾部的块。







## 问题

表单分析

比如登录表单

发送邮件表单

表单错误

看看它们是怎么布局的

表单的布局、宽度要怎么处理？



H5布局