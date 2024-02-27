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