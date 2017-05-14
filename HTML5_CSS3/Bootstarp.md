## Grid System
bootstrap将网页划分成了12格来进行设计，这种设计叫做网格系统。
在bootstrap的网格系统中，有3种类别的网格：
- col-xs-*，这种网格类是专门为手机准备的，窗口小于768则会使用
- col-sm-*，应付小型设备，在窗口>=768px会应用
- col-md-*，>=992px会应用
- col-lg-*，>=1200

网格系统就是将窗口进行切割，在应用样式的时候，只需要设定要占据的格数，bootstrap会自动被该元素分配面积的大小。
意思就是仍然占据一行，只不过占用的面积是由网格数来决定的。

## 布局
考虑俩点，1是是用什么样的布局，2是考虑在什么条件下使用这种布局，这里的什么条件指的是你要在哪种设备上应用这种样式的布局，因为网格系统有多种类别。

**混合布局**
所谓的混合布局即能让布局能适应多种设备，例如手机、平板、电脑。
这种布局你只要使用多种类别的网格样式即可，bootstrap会根据网页的宽度来自动选择应用哪种样式。
```html
<div class="col-sm-8 col-md-7 col-lg-5"></div>
```

**网格的Push和Pull**
example：
```html
col-md-push-3
往右边推3个网格

col-md-pull-9
往左边拉3个网格
```

**嵌套布局**
可以在网格中继续使用网格系统来布局，类似嵌套的效果。
只需要在网格中继续使用row类选择器元素，然后划分即可。

**清除浮动**
clearfix选择器，包含了清除浮动的样式。


**偏移**
偏移能让你的网格向右进行移动，使用的时候要根据你要解决的设备来进行样式的选择。

**显示与隐藏的CSS类**
这些类选择器能让你选择在何种设备要显示或隐藏你的内容。

隐藏类选择器：
- visible-xs
- visible-md
- visible-lg

- hidden-xs
- hidden-md
- hidden-lg


## 导航栏
在H5中，新增了nav导航栏元素。
```html
<nav class=""/>
```

navbar navbar-default，导航栏的默认样式
navbar navbar-inverse，导航栏另外一种样式
nav navbar-nav，组成导航栏面包屑

**导航栏的固定样式**
navbar-fixed-top，这种是永远固定的样式，不会随着滚动条而该表。
navbar-static-top，随着滚动条滚动而滚动


## modal
modal组件用于弹出对话框，你可以在里面设置写内容。

```php
<div class="modal-backdrop in" style=“display:block"></div>
# 在对话框底部显示一个半透明的层，让用户的注意力集中在弹窗上面。
# in选择器与display属性用于显示对话框
# fade选择器，弹出动画效果


<div class="modal-dialog">
# 暂时不知道意义

<div class="modal-content"></div>
# 定义了对话框的阴影和圆角等样式，内容写在这个div中。
```

**tabindex**
当按下tab键的时候，页面会对应选择元素，我们可以通过tabindex控制选择的元素序列。
tabindex="-1"

**options**
通过data系列属性来控制对话框。
data-backdrop="false"，控制对话框的背景颜色。
```js
$(function(){
	$('#login-modal').modal({
		show:false,
		backdrop: false,	//控制背景颜色
		remote : 'login-from.html'
	});
});

$('#login-modal').modal();
# 根据传进去的参数可以控制显示或隐藏对话框
```

**event**
事件，对话框在现实或隐藏的时候都会触发事件。
- show.bs.modal


## 旋转木马

## bootstrap小图标
http://getbootstrap.com/components/
