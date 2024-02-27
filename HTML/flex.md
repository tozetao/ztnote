flex意为弹性布局。

采用flex布局的元素，称为flex容器，flex容器内的子元素自动称为flex Item，并且子元素的float、clear和vertical-align属性将失效。

### Flex Container

flex容器有水平的主轴和垂直的交叉轴，容器还有6个属性设置，主要控制flex item在容器内的布局。

flex-direction

决定项目在主轴上的排列方向。

flex-wrap

决定项目如何换行。

justify-content

决定项目在主轴上如何对齐。

align-items

决定项目在交叉轴上如何对齐。

aligen-content

定义多跟轴线的对齐方式，如果项目如有一根主轴线，该属性不起作用。flex-wrap属性为wrap，如果项目超出容器大小将会换行，这时候就会有多根主轴线了。



缩写

flex-flow是flex-direction和flex-wrap属性的缩写，默认值为row norwrap



### Flex Item

flex item有6个属性可以设置，主要用于调整自身。



order

定义项目的排列顺序，值越小排名越靠前。



flex-grow

定义项目的放大比例，默认为0。即容器存在剩余空间，也不放大。

如果值为1，项目将等分剩余空间；如果一个项目的值为2，其余项目的值为1，前者占据的剩余空间会比其他项目多一倍。

flex-grow可以认为是项目占据容器空间宽度的比例。



flex-shrink

定义项目的缩小比例，默认为1，即如果空间不足，该项目将缩小。

如果所有项目的值为1，当容器空间不足时都将等比缩小；如果一个项目的值为0，其他项目为1，当空间不足时，前者不缩小。



flex-basis

定义在分配多余空间之前，项目占据的主轴空间，浏览器会根据这个属性，计算主轴是否有多余空间，它的默认值为auto，即项目本来的大小。

```css
.item {
    flex-basis: <length> | auto;
}
```

可以理解为设置项目的占据主轴的宽度。



flex缩写

flex是flex-grow、flex-shrink、flex-basis3个属性的缩写，默认值为0 1 auto，后俩个属性可选。



align-self

定义单个项目与其他项目不一样的对齐方式，可覆盖align-items属性。

默认值auto，表示继承自父类的align-items属性，如果没有父元素则等同于`stretch`。 





- 注意：子元素是float、clear和vertical-align属性失效，而不会影响margin和padding。















example：

```html
<style>
    .grid {
        display: flex;
        justify-content: space-between;
        align-content: space-between;

        background: #cdc;
    }

    .grid-cell {
        flex: 1;
        background: yellow;
        height: 50px;
        line-height: 50px;
    }

    .space {
        margin-left: 10px;
    }
</style>
<div class="grid">
    <div class="grid-cell">1/2</div>
    <div class="grid-cell space">1/2</div>
    <div class="grid-cell space">1/2</div>
    <div class="grid-cell space">1/2</div>
</div>
```







padding问题

在容器width为100%时，针对容器设置padding，项目有固定宽度时，容器的宽度会溢出，这时候需要改变盒子模型对于宽度的计算方式，box-sizing

有趣的是如果不设置容器的宽度，让浏览器来自己计算则不会出现该问题。

```html
<style>
    html, body{
        margin: 0;
        padding: 0;
    }

    .box {
        display: flex;
        justify-content: space-between;
        align-items: center;

        width: 100%;
        box-sizing: border-box;

        padding: 10px;
        background: #cdc;
    }

    .item {
        width: 50px;
        height: 30px;
        background: yellow;
    }
</style>

<div class="box">
    <div class="item">1</div>
    <div class="item">2</div>
</div>
```









参考链接：

http://www.ruanyifeng.com/blog/2015/07/flex-examples.html

http://www.ruanyifeng.com/blog/2015/07/flex-grammar.html?utm_source=tuicool)