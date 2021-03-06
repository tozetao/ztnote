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

