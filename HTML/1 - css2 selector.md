### 标签选择器
标签选择器是对HTML标签做统一样式的定义，例如：
```html
<style>
p{
	color:#ccc;
}
</style>
```
将所有的p标签字体设置为#ccc颜色



### class选择器

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



### id选择器

跟class选择器是一样的，但是id选择器是唯一的，一个文档中只能出现一次。
```html
<style>
	#mainBody{
		color:#ccc;
	}
</style>

<div id="mainBody"></div>
```



### 派生选择器

HTML文档标签是一个一棵树来呈现的，标签和标签之间有依赖的层级关系，
派生选择器允许定位到一个标签，通过标签或者样式的层级关系，来定义子元素的样式。

example：
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

<div class='content'>
	<ul>
		<li>this is demo</li>
		<li>this is demo</li>
	</ul>
</div>
```
上述例子中指定所有引用.content类选择器标签下的ul li标签的样式，只要ul li标签是.content类选择器标签的后裔，指定的样式就会生效。





### 群选择器

群选择器指的是一组选择器，可以设置多个选择器相同的样式，用法是将多个选择器之间以逗号","分割。
```html
<style>
	.first, .second, .third{}
</style>
```
