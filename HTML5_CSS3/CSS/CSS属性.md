## position
CSS一个很重要的属性，定义元素的定位类型，主要有以下几个定位方式：

- static：元素的默认值，没有定位，元素处于正常文档流，忽略top、button、right、left、z-index属性
- absolute：生成绝对定位的元素，相对于static定位以外的第一个父元素进行定位。
- relative：相对定位，相对于自身所在文档流中的位置进行定位。


### 1. relative
相对定位，relative属性的元素不会脱离文档流，
相对定位的元素会相对于自身在文档中的位置进行偏移，由于它没有脱离文档流，所以元素会占据原有的文档空间。

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
绝对定位，绝对定位的标签会脱离文档流。

根据祖先类元素来进行定位，并且该祖先类元素的position属性的值必须是非static值，如果找不到这种直系元素，将会以html标签来定位。

```html
<style type="text/css">  
    html{
		border:1px dashed green;
	}

    body{
    	border:1px dashed  purple;
    }

    #first{
    	position: relative;
    	width: 200px;
    	height: 100px;
    	border: 1px solid red;
    }  

    #second{
    	position: absolute;
    	width: 200px;
    	height: 100px;
    	border: 1px solid blue;
    	top :0;
    	left : 0;
    }
</style>  
<div id="first">relative</div>  
<div id="second">absoult</div>

<!--
	second元素是绝对定位，由于其父元素是html元素，所以相对于html定位。
-->
```

absolute元素的标签脱离了文档流并且是相对于父级元素来偏移的，所以父元素的padding属性是不会影响到absolute元素，，例如：
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




## line-height
line-height是行间距，用于控制行与行之间的距离。

### line-height的值
line-height属性可以被指定为下面任意一个：
- normal：浏览器默认值，默认1-1.2，取决于浏览器
- number：一个数字，在应用时具体值等于该数字number乘以该元素的字体大小
- length：长度，可以是css任意单位，用于指定line box高度的值
- percentage：与元素自身的字体大小有关，计算值是给定的百分比值乘以元素计算出的字体大小。

这4中类型的值在继承的时候会有差别：
- 值如果是百分比，父元素的行高等于自己的font-size乘以百分比，后代元素直接继承父元素的行高，
- 如果是number，父子元素的行高等于各自的font-size乘以number计算得到
- 如果是length，子元素直接继承父元素的行高。

### inline-boxes与inline-height
行高基于一个准则应用于inline-boxes，行高作用于一个元素时，是将line-height与font-size的计算值之差分为俩半，分别添加在content area(文字)的顶部和底部。
```html
.box{
	width: 18em;
	background: #cdc;

	font-size: 20px;
	line-height: 20px;
}

<div class="box">
	Avoid unexpected results by using unitless line-height
</div>

# 修改line-height的值，可以观察到行间距的变化(字体顶部和底部的变化)
```
如果line-height小于font-size，




## background
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
