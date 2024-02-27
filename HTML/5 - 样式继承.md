
## CSS样式继承
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
