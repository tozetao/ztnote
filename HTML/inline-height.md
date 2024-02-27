
## line-height
line-height是行高，用于控制行所需要的空间，例如文字。
它主要作用于块元素的内容，如果是行内元素中的内容则无法生效。

###  1. 属性值的类型
line-height属性可以被指定为下面任意一个：
- normal：浏览器默认值，默认1-1.2，取决于浏览器
- number：一个数字，在应用时具体值等于该数字number乘以该元素的字体大小
- length：长度，可以是css任意单位，用于指定line box高度的值
- percentage：与元素自身的字体大小有关，计算值是给定的百分比值乘以元素计算出的字体大小。

这4中类型的值在继承的时候会有差别：
- 值如果是百分比，父元素的行高等于自己的font-size乘以百分比，后代元素直接继承父元素的行高
- 如果是number，父子元素的行高等于各自的font-size乘以number计算得到
- 如果是length，子元素直接继承父元素的行高。

### 2. inline-height的盒子模型
例如在一个p标签中，box类型能够分为4种：
- containing box：包裹着所有内容的box
- inline-boxes：inline-height作用于字体后的区域
- inline-box：一个inline-boxes有多个inline-box
- content area：文字内容区域



### 3. inline-height和字体大小的关系
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

如果line-height小于font-size，inline-boxes会优先于行高，这时候content area会溢出inline-boxes，
可以理解成line-height与font-size的计算值之差是作为负数作用于content area，例如将上面案例的line-height改成10px，就可以看到这种现象。
