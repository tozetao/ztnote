
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