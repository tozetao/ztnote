## position
CSS一个很重要的属性，定义元素的定位类型，主要有以下几个定位方式：

- static：元素的默认值，没有定位，元素处于正常文档流，忽略top、button、right、left、z-index。
- absolute：生成绝对定位的元素，相对于static定位以外的第一个父元素进行定位。
- relative：绝对定位，


### 1. relative
相对定位，使用top、z-index、button等属性来控制定位。
relative根据标签在文档流中的位置而进行的偏移定位，由于它没有脱离文档流，元素仍旧会占据原有的文档空间，也就是所占据的文档空间不会随top等属性发生偏移。

```html
<style type="text/css">  
	.first{ width: 200px; height: 100px; border: 1px solid red; position: relative; top: 20px; left: 20px;} /*add position*/  
	.second{width: 200px; height: 100px; border: 1px solid blue;}
</style>

<div class="first"></div>
<div class="second"></div>  
```
可以看到second div仍旧在first div所占据位置的下方。

注1：使用margin属性仍旧是相对于该标签所在的文档流位置来进行偏移的，在margin和top等属性同时使用，首先margin根据文档流偏移位置，之后top等属性再根据现在的文档流偏移位置。
注2：margin是占据文档流空间的。

### 2. absolute
absolute是绝对定位，绝对定位的标签会脱离文档流，根据祖先类元素来进行定位，并且该祖先类元素的position属性的值必须是非static值，如果找不到这种直系元素，将会以html标签来定位。

测试相对偏移的元素
```html
<style type="text/css">  
    html{border:1px dashed green;}  
    body{border:1px dashed  purple;}  
    #first{ width: 200px;height: 100px;border: 1px solid red;position: relative;}  
    #second{ width: 200px;height: 100px;border: 1px solid blue;position: absolute;top :0;left : 0;}  
</style>  
<div id="first">relative</div>  
<div id="second">absoult</div>

<!--
	上述代码可以看出不同定位的元素相对的元素是不同的
-->
```

非static值元素的padding属性是不会影响到absolute元素，absolute元素的标签脱离了文档流并且是相对于父级元素来偏移的，例如：
```html
<style type="text/css">  
    html{
    	border:1px dashed green;
    }  
    body{
    	border:1px dashed  purple;
    }  
    
    #first{
    	width: 200px;
    	height: 100px;
    	border: 1px solid red;
    	position: relative;
    	margin:40px;
    	padding:40px;
    }  

    #second{
    	width: 200px;
    	height:100px;
    	border: 1px solid blue;
    	position: absolute;
    	top:20px;
    	left:20px;
    }
</style>

<body>
	<div id="first">
		<div id="second">second</div>
		first
	</div>  
</body>  
```




### text-style
**设置HTML文档字体默认样式**
网页字体的单位有：像素、百分比、em，
浏览器默认字号是16px，而标题是一个相对大小，它会随着字体的大小变化而变化。

一般浏览器设置h2字体的大小是普通文字的1.5倍，默认是16px*1.5，在html文档中，我们可以通过设置body标签设置字体的默认样式，来影响标题标签。 


**设置字体的序列**
font-family:...

**设置行间距**
line-height，该属性用于设置行高，不关文字有多大，行高都是固定的。如果不指定行高的具体指，以倍数来指定，则会以默认字体乘以倍数。





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
