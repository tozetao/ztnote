## CSS
css是网页的样式，定义了网页内容该如何显示，一般在网页中有3种方式来引入css样式。

- 行内样式：使用HTML标签的style属性，来定义标签样式
- 在HTML文档heade标签内，通过定义style标签来定义样式
- 将CSS样式保存为文件，通过导入文件来引用样式

## CSS选择器
### 1. 标签选择器
标签选择器是对HTML标签做统一样式的定义，例如：
```html
<style>
p{
	color:#ccc;
}
</style>
```
将所有的p标签字体设置为#ccc颜色

### 2. class选择器
将多个不同的样式定义为一组样式并命名，方便重复使用，例如：
```html
<style>
	.mainBody{
		color:#ccc;
	}
</style>

<div class="mainBody"></div>
# mainBody类选择器可以被任何标签引用
```


### 3. id选择器
跟class选择器是一样的，但是id选择器是唯一的，一个文档中只能出现一次。
```html
<style>
	#mainBody{
		color:#ccc;
	}
</style>

<div id="mainBody"></div>
```

### 4. 派生选择器
HTML文档标签是一个一棵树来呈现的，标签和标签之间有依赖的层级关系，
派生选择器允许定位到一个标签，通过标签或者样式的层级关系，来定义子元素的样式。

example1：
```html
<style>
	.content{
		width: 200px;
		height: 200px;	
	}
	.content ul li{
		color: red;
	}
</style>

<div id='content'>
	<ul>
		<li>this is demo</li>
		<li>this is demo</li>
	</ul>
</div>
```
上述例子中指定所有引用.content类选择器标签下的ul li标签的样式

example2：


### 5. 群选择器
群选择器指的是一组选择器，可以设置多个选择器相同的样式，用法是将多个选择器之间以逗号","分割。
```html
<style>
	.first, .second, .third{}
</style>
```

## CSS样式重叠
HTML标签是一棵树，标签和标签是层级包裹的关系。当一个标签被其他标签包裹的时候，该标签除了它自身的样式，还会继承外层标签的样式。

如果样式不冲突这些样式会叠加起来并作用于当前标签，如果样式冲突，对于冲突的样式浏览器会采用继承自最近父元素标签的样式，例如：
```html
<style type="text/css">
	.body{
		color: red;
		background: #cdc;
	}
	.content{
		color: yellow;
		line-height: 10px;
	}
	h1{color: green;}
</style>

<body class="body">
	<div class="content">
		<h1>second</h1>
		<p>this is my demo</p>
	
		<h1>three</h1>
		<p>this is my demo</p>
	</div>
	<p>red</p>
</body>
```
在上面的例子中，p标签未定义任何样式，但是继承了body标签的样式，所以字体是红色的。
而div标签定义了字体样色样式，所以标签内的文字是yellow颜色，div中的h1标签又重新定义了自身字体样色样式，覆盖了父级元素的color属性，所以是green。

上面的是属于标签与标签之间样式的重叠，如果多个选择器同时作用于一个标签时，样式也可能会发生重叠，这里涉及到不同选择器样式采用的优先级问题，例如：
```html
<style type="text/css">
	.first{
		color: yellow;
		line-height: 10px;
	}
	.second{
		color: red;
	}

	h1{color: green;}
	h2{color: #cdc;}
</style>

<div class="first second">
	<h1>Title</h1>
	<h2 class="first">Title</h2>
	content
</div>
<!--
	h2标签选择器被class选择器覆盖，相同选择器样式重叠下是按照选择器定义的先后顺序来使用。
 -->
```
优先级说明：
优先级的计算是按照选择器所对应的分数来计算的
- 一个标签选择器是1分
- 一个类选择器是10分
- 一个id选择器是100分
- 一个行内选择器是1000分。

如果CSS选择器的样式不同，多个CSS选择器的样式会组合使用，
但是发生样式重叠，那么会优先使用分高的样式。

注1：如果定义了多个同名的CSS选择器或定义了同名的样式，后面重复的样式会覆盖前面定义的样式
注2：派生选择器的计分比较特殊，例如：#special h3，该选择器的分数是1001分。


## CSS盒子模型
浏览器将HTML文档看成是装有内容的小盒子，这个盒子是由内容、内边距、边框、外边距组成。
- 内容：标签中的文字和内容
- 边框：盒子的边框
- 内边距：盒子的内容与盒子边框之间的间距
- 外边距：边框以外的间距

在HTML中，使用margin控制外边距的距离，使用padding控制内边距的距离，
常见的网页宽度是960px，margin:0 auto，用于让层居中显示。

## CSS Float
### 1. 块与行
HTML文档标签分为块级元素和行级元素
- 块级标签：占据HTML文档一行内容并另起一行。
- 行级标签：标签随着内容大小变化，无法显示指定宽和高，因为不是块级元素。

### 2. float
float是CSS属性，HTML文档的块级元素会占用一行内容并重新另起一行作为开头，而float属性就是用于更改块级元素的特性。

对一个块级标签使用float属性后，元素下面的内容就会围绕着浮动元素来显示，这里的内容指的是文字、图片这种内容，块级标签是不会围绕着float属性的标签，因为float标签脱离了文档流，其他块级标签感知不到，所以会移动上去占据float标签的地方，例如：
```html
<style type="text/css">
	body{margin: 0px; padding: 0px;}
	.content{
		float: left;
		width: 256px;
		height: 300px;
		background: #cdc;
	}
	.first{
		width: 100px;
		height: 20px;
		background: red;
	}
	.second{
		width: 700px;
		height: 20px;
		background: yellow;
	}

	span{
		background: red;
	}
</style>

<div class="content"></div>
<div class="first"></div>
<div class="second"></div>
<p>hello world</p>
<!--
	first div无法看到，被content div覆盖了，但是first div扔占据一行内容。
	再查看second div，会有256px宽度被覆盖，证明了块级元素无法感知浮动元素，但是文字内容可以可以感知到的。
-->
```

如果希望块级元素能感知到float标签，可以使用clean属性来清除float效果，这样块级元素就会排在float标签后面了。

## CSS布局
要布局的话，需要了解盒子占用的宽度。
一个盒子的占用HTML文档的真正宽度是：width + padding*2 + border*2，例如：
```html
.mainBody{
	width:620px;
	border:5px;
	padding: 20px;
}

```
使用mainBody选择器的div真正占用宽度是：620+10+40=670px

