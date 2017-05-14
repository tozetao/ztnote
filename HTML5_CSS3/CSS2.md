## HTML文档格式
```html
```
第一行告诉用户的浏览器，该文档的类型，以及使用哪种html标准，浏览器会根据这个标准去解析显示该网页。

html标签，代表着文档的开始和结束。

head标签，中的内容用于定义网页的信息，例如标题、css样式

meta标签，元数据，有解释说明的意思，告诉浏览器的一些信息或者说明文档作用


## HTML基础标签
### p标签
p标签代表一个段落。
p标签要注意的是p标签有内置的margin-top和margin-botton，且对包括的div有影响，因为css盒子bug，所以多个段落会有序的间隔排列

```html
<!DOCTYPE html>
<html>
<head>
	<title>this is my demo</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
	<style type="text/css">
		body{
			margin: 0px;
			padding: 0px;
		}
		.content{
			background: green;
		}
		.other{
			background: #cdcdcd;
		}
	</style>
</head>
<body>
<div class="other">
	this is my demo
</div>
<div class="content">
	<p>this is my demo</p>
</div>
<div class="content">
	<p>this is my demo</p>
</div>
</body>
</html>
```
h标签与p标签都是块元素，所以div盒子模型的bug也对他们有效，下面的例子便能很好的说明：
```html
<h1>first</h1>
<p>this is my demo</p>

<h1>second</h1>
<p>this is my demo</p>

<h1>three</h1>
<p>this is my demo</p>
```

### img
用于显示图片，属性有：
- src，加载图片的路径，可以是相对路径或绝对路径
- alt，对图片的描述
- align，图片对齐方式
- hspace，对img边框的设定

### a标签
用于创建一个链接，属性有：
- href，连接的网址
- target，打开a标签的方式

注：行内标签

### ul/li
li标签用于显示无序列表，必须包裹在ul标签中。

### ol
ol标签包裹的列表是有序列表


### span
span标签是属于行内标签，也叫做行内元素，这类标签的内容是不会换行显示，相对的有区块标签，使用这种标签会在新的一行显示，例如：p、h1、div等。

该标签针对段落中的内容做不同的样式。


## CSS
简单的说，css是网页的样式，定义了网页内容该如何显示。

### CSS应用
CSS有3种应用方式
- 行内样式，直接在标签中输入样式
- 在html中的style标签设置标签的css样式
- 通过导入css样式文件来该表标签的样式

### CSS选择器
**标签选择器**
对HTML标签做统一样式的定义，如下：
```html
<style>
	p{
		color:#ccc;
	}
</style>
```

**class选择器**
该选择器能够定义可重复利用的样式，例如：
```html
<style>
	.mainBody{
		color:#ccc;
	}
</style>

<div class="mainBody"></div>
```

**id选择器**
id选择器同class选择器功能相同，但是它只能在一个页面中出现一次。
```html
<style>
	#mainBody{
		color:#ccc;
	}
</style>

<div id="mainBody"></div>
```

**派生选择器**
能够对某一个位置中的某一个标签去添加样式，例如：
```html
<style>
	ul li{}	//只能对ul标签中的li标签产生作用
	li a{}	//只能对li标签中的a标签产生作用
</style>

<ul>
	<Li><a href='*'>link</a></li>
</ul>
```

**群选择器**
设置多个选择器共同拥有的样式，多个选择器之间以逗号","分割。

### CSS重叠
CSS样式重叠有俩种情况，
- 多个标签选择器样式的重叠
- 不同的多个选择器样式的重叠



当一个标签被其他标签包裹的时候，该标签除了它自身拥有的样式，还可能继承自外层元素的样式，如果这些样式不冲突的话，浏览器会将这些样式叠加在一起应用于标签，如果这些样式冲突，浏览器只会采用继承自最近的父元素的样式。

上述这种情况的重叠应该是标签选择器的重叠

```html
body{color:#999;}
div{color:#cdc; line-height:10px;}
h1{color:green;}

<body>
	<div>
		<h1>second</h1>
		<p>this is my demo</p>
	
		<h1>three</h1>
		<p>this is my demo</p>
	</div>
</body>
```

css样式重叠还有另外一种情况，即多个选择器同时作用于相同的标签，样式该如何解析。
```html

```
一个标签选择器是1分，一个类选择器是10分，一个id选择器是100分，一个行内选择器是1000分。如果样式不同，那么样式会组合使用，如果样式重叠，那么会优先使用分高的样式。

派生选择器的计分比较特殊，例如：#special h3，该选择器的分数是1001分。


### text-style
**设置HTML文档字体默认样式**
网页字体的单位有：像素、百分比、em，
浏览器默认字号是16px，而标题是一个相对大小，它会随着字体的大小变化而变化。

一般浏览器设置h2字体的大小是普通文字的1.5倍，默认是16px*1.5，在html文档中，我们可以通过设置body标签设置字体的默认样式，来影响标题标签。 


**设置字体的序列**
font-family:...

**设置行间距**
line-height，该属性用于设置行高，不关文字有多大，行高都是固定的。如果不指定行高的具体指，以倍数来指定，则会以默认字体乘以倍数。


### box
浏览器把html标签看成是装有内容的小盒子，这个盒子是由内容、内边距、边框、外边距组成。
- 内容，标签中文字和图片
- 内边距，内容与边框之间的距离
- 外边距，边线以外的内容

在html中，我们使用margin控制外边距的距离，使用padding控制内边距的距离。
常见的网页宽度是960px，margin:0 auto，用于让层居中显示。


### float
html的块级元素都会占用一块地方并另起一行，float就是用于打破块级元素的默认规则。对一个元素使用float之后，元素下面的内容就会围绕着浮动元素来显示，这里的内容指的是文字、图片这种内容，周围的元素是不会围绕的，因为正常元素触碰不到float元素。

如果想要元素下面的内容不被float影响，那么需要让内容自己来清除浮动，使用clean属性，这里的内容可以是文字或者元素。

### layout
这样讲float布局，布局的话有个宽度的概念。
一个盒子的占用HTML文档的真正宽度是：盒子自身width + padding + border，计算应该这样计算。

例如：
```html
.mainBody{
	width:620px;
	border:5px;
	padding: 20px;
}

```
使用mainBody选择器的div真正占用宽度是：620+10+40=670px


### background
background能为元素设置背景，填充颜色、使用背景图片，背景图片能设置它的填充方式，例如横向或纵向平铺。

- 用于填充颜色
- 填充图片，各种平铺方式，no-repeat同时能控制显示位置（坐标）
```html
background: url()

# 填充方向的设置
repeat-x，横向平铺
repeat-y，纵向平铺
no-repeat，不填充，只进行显示一次。

# 控制图片的显示方向
# 关键字的控制
background: url(./phone.jpg) no-repeat right top

# 百分比的控制
# 水平方向：从左到有分别是0-100%
# 垂直方向：上上到下分别是0-100%
background: url(./phone.jpg) no-repeat 100% 0

# px像素的控制
# 同百分比一致。
background: url(./phone.jpg) no-repeat right top
```

### position
控制元素在网页上的精确位置，值有，
**absolute**
绝对定位，例如：position：absolute；right:0; top:0;
默认是相对于body元素进行定位，设置position后，使用left、right、top、bottom来进行定位。

z-index属性能够决定relatioe属性元素的上下层关系。


设定成absolute属性的元素，基本就脱离了文档流，块级元素是无法捕捉到他的。

**relative**
将外层元素设置为relative后，内层的aobosulte元素能够相对该元素进行定位。
