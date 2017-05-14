## CSS3 selector
CSS3是对CSS2的一个扩展，这里主要讲CSS的选择器

### 类型选择器
类型选择器是指使用html标签的名字来作为选择器，能够统一的对标签进行样式定义。

```html
div{
	with:900px;
}
```

### 选择器群组，Groups of selectors
使用逗号分割开不同的选择器，对多个选择器进行统一的样式定义。
```html
h1,h2,h3,p{
	text-align:right;
	line-height:1.5;
}
```
### 通过选择器
使用*字符来代替所有html元素

### 类选择器，Class of selectors
能够定义通过的样式，以类的形式给元素使用。
```html
.container{
}

# 指定h2标签的title选择器的样式
h2.title{
}

# 在css3中元素可以这样使用类选择器
# 该标签会使用到俩个class选择器的样式
<div class='title brand-name'></div>

# 也能以组合的形式单独指定元素样式
.title.brand-name{}
```

### Id选择器
跟类选择器一样，一个文档中不能有同名的id选择器

### 属性选择器，Attribute selectors
[attribute]，表示选择拥有该属性的元素

[attribute1][attribute2]，同时拥有多个属性

[attribute='value']，选择某个属性等于特定的值，注意是特定。

[attribute~='value']，有的标签属性有多个值，以空格分开，该选择器表示选择拥有该属性拥有这个值得属性标签。

[attribute!='value']，属性的值是=后面的值，或者是以这个value开头的后面带-的值的属性标签。

```html
[data-album]{

}

li[data-album]{}
# 匹配li标签同时有data-album属性

li[data-album="single"]{}
# 匹配li标签同时又data-album属性且属性值等于single
```

[attribute^='value']，匹配属性值以value开头
[attribute$='value']，匹配属性值以value结尾的
[attribute*='value']，属性包含value值得标签

## 伪类
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


::before
选中元素之前地方，

::after
选择元素之后的内容


### 后裔选择器
div img
用空格分开不同的选择器，这种就是组合类型的选择器。
它表示选择前面选择器指定的后裔的元素，选择div元素里面的img标签后裔，img标签可以是div标签的子孙元素、子子孙元素。

只要是div img的后裔就会被选择。

div * img{}
表示不选择子元素，只会选择子元素中的其他后裔。

div > img 
只会选择div元素中的img儿子元素


h1 + h2
选择+号前面的这个元素的下一个临近的兄弟元素，主要要邻近，如果中间有其他元素阻隔，是无法选择的

h2 ~ h3
选择~号前面的这个元素的下一个邻近的兄弟元素，无视其他元素阻挡
