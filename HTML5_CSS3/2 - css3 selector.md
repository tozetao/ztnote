### 类选择器

将不同样式定义成一组样式并命名，方便重复使用。

- css3支持多个类选择器应用同一个元素，以组合的方式复用样式。比如：

```html
<style type="text/css">
    .background{
        background: #cdccec;
    }

    .font {
        font-size: 30px;
        color: red;
    }
</style>

<div class="background font">
    <p>this is content</p>
</div>
```



- 也支持类选择器在特定标签生效。语法是先写标签，再写类选择器，注意中间是没有空格的。

```html
<style type="text/css">
	h2.title{
		font-size:25px;
		color:red;
	}
</style>

<div class="title">
    title
</div>
<h2 class="title">
    title
</h2>
```



### 属性选择器

css3的新特性，可以通过标签的属性值来匹配HTML文档中的标签。

- [attribute]

  选择拥有该属性的元素

- \[attribute_1\]\[attribute_2\]

  选择同时拥有多个属性

- [attribute = 'value']

  选择某个属性拥有特定值的标签

- [attribute |= 'value']

  匹配属性等于value值的标签，或者属性的值以value-开头的标签

- [attribute ~= 'value']

  选择属性拥有多个值的标签，以空格分开。某些标签属性拥有多个值

- [attribute ^= 'value']

  匹配属性值以value开头的标签

- [attribute $= 'value']

  匹配属性值以value结尾的标签

- [attribute *= 'value']

  匹配属性包含value值的标签

example：
```html
<style type="text/css">
    /* 匹配拥有data-album属性的标签 */
	[data-album]{
		display: inline-block;
		text-align: center;
		color: black;
		padding-right: 20px;
	}

    /* 匹配拥有多个属性的标签 */
	div[data-name][data-age]{
		width: 50px;
		height: 50px;
		background: #d3d;
	}

    /* 匹配属性值等于a或等于a-开头值的标签 */
	div[data-name|="a"]{
		width: 150px;
		height: 150px;
		background: #cdc;
		margin-top: 10px;
	}
</style>

<ul>
	<li data-album="1">1</li>
	<li data-album="1">2</li>
	<li data-album="1">3</li>
</ul>
<div data-name="lisi" data-age="20"></div>
<div data-name="a"></div>
<div data-name="a-123"></div>
```



### 后代选择器

指允许通过标签、类选择之间的关系来应用样式。

```css
element1 element2
```

匹配元素1内部所有元素2，只要是元素1的后代就会被匹配。比如子孙、子子孙等后台元素。

注：element可以是标签名也可以是选择器名。

example:

```html
<style type="text/css">
    .foo .article {
        background: #cdccec;
    }
    
    .foo .article .child {
        font-size: 20px;
        height: 30px;
        line-height: 30px;
    }
</style>

<div class="foo">
    <div class="article">
        <p class="child">
            hello world!...
        </p>
        <div>
            <p class="child">
                hello world!...
            </p>
        </div>
    </div>

    <!-- 不符合指定规则则不匹配 -->
    <div class="child">
        foo2123123
    </div>
</div>
```



```css
element1 > element2
```

只匹配元素1中所有直系的元素2，不包括子孙等关系的元素。

example:

```html
<style type="text/css">
	.container > div{
		height: 20px;
		background: lightgreen;
		margin-bottom: 10px;

	}
	/* 只匹配div子节点 */
</style>

<div class="container">
	<div></div>
	<div></div>
	<div>
		<div></div>
		<div></div>
	</div>
</div>
<!-- 匹配container div下所有子div元素 -->
```





```css
element1 * element2
```

不匹配元素1中的直系后台元素2，只会匹配元素2中的其他后代元素。



```css
element1 + element2
```

选择临近元素1的下一个兄弟元素，如果有其他元素阻隔则无法选择。



```css
element1 ~ element2
```

选择临近元素1的所有兄弟元素，无视其他元素阻隔。

example:

```html
<style type="text/css">
	div ~ p{
		font-size: 20px;
		color: yellow;
	}
</style>

<div></div>
<h1>title</h1>
<p>yellow?</p>
<p>yellow?</p>
<p>yellow?</p>
```



### 伪类
before和after是附着在元素前后的伪元素，伪元素是不在dom中生成的，而是浏览器渲染css时画上去的，所以在浏览器上查看元素是看不到伪类的HTML结果的。

在伪元素的样式上可以通过content属性设置伪元素的内容，默认伪元素是行内显示的，也可以将其设置为块元素显示。

content：该属性在css2.1中引入，配合before、after伪类来为元素插入内容，值可以是
- string：如果是空字符或者none，是不会显示的。
- attr
- url

example1:
```html
<style>
    .demo{
        font-style: normal;
        font-weight: normal;
    }

    .demo:before{
        content: 'before ';
    }

    .demo:after{
        content: ' after';
        width: 100%;
        height: 100%;
        border: 1px solid;
    }
</style>


<i class="demo">cotent</i>
```



### 选择器伪类

浏览器一般会在没有访问的连接上去应用一个:link的伪类，
在访问后的连接上会应用一个:visited的伪类，

通过这俩个伪类你可以为标签设置访问过的和未访问过的链接的样式。


与用户互动的伪类
:hover，鼠标指针移动在元素上面会应用
:active，用户点击按钮鼠标左键时会应用，松开时就不会应用
:focus，在文本框处于焦点的时候，会添加一个focus的伪类。
互动类型的伪类可以对大部分元素进行使用：
```html
li:hover{
}

.search:focus{
}
```

伪类:target
如果一个链接引用了一个id元素，当点击链接的时候，被引用的元素会发生一个:target的伪类。
```html
<a href="#content">跳转至内容</a>

<div id="content">这里是主体内容...</div>
```

伪类:disabled
在表单元素上面去添加一个disabled的属性，表示当前禁用这个元素，例如文本框不用输入，复选框不能选择。
当一个表单对象拥有该属性时，就会应用:disabled伪类
```html
<input id="search" type="text" disabled/>

#search:disabled{
}
```

伪类nth-child()
该伪类能根据子元素在父元素中的位置来应用伪类，
该伪类可以带参数，参数可以是数字、表达式
```html
nth-child(1)
# 表示子元素在父类中的第一个位置

nth-child(odd)

nth-child(even)

nth-child(an+b)
# n是步进，表示每隔多少个选中元素
```

伪类nth-last-child()
该伪类跟nth-child一样，只不过该伪类是从元素的末尾倒过来匹配的。


伪类nth-of-type
在一个父元素的所有子元素里面可以使用nth-of-type()，按照类型和位置选择这些子元素。

参数仍然同上，与之对应的伪类是nth-last-of-type()
```html
p:nth-of-type(1)
# 选择p标签类型的标签，并选择第一个
```



伪类first-child
该伪类能让我们选择父元素中的第一个子元素，参数同上。:last-child


伪类:first-of-type
父元素上某一个类型上面的第一个元素。

伪类:only-child
选择父元素里面的独生子，就是没有兄弟元素的元素。
:first-child:last-child组合起来使用时一致的，即选择的元素是父元素中的第一个元素也是最后一个元素。


伪类:empty
选择完全空白的元素，元素里面有一个空格也不会包含。

伪类：not()
一个否定的伪类，我们可以把简单的选择器作为一个参数给这个not()伪类，排除掉这个参数标签的元素
```html
li:not(:first-child){
}
# 选择除了第一个列表外的所有元素。
```



这些新的伪类主要用于筛选标签用的。


关于文字的伪类选择器
p::first-line{}
匹配段落中第一行的文字

::first-letter{}
匹配段落第一行第一个字

