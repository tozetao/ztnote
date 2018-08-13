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

定义多跟轴线的对齐方式，如果项目如有一根轴线，该属性不起作用。



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

它可以设为跟width或height属性一样的值，即项目占据固定的空间大小。



缩写

flex是flex-grow、flex-shrink、flex-basis3个属性的缩写，默认值为0 1 auto，后俩个属性可选。





```html

<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <style>
      html, body {
        margin: 0;
        padding: 0;
      }

      .box {
        display: flex;

        /* width: 600px; */
        width: 100%;
        height: 600px;
        background: #cdc;
        padding: 10px;

        /* flex-direction: column; */
        justify-content: space-between;
      }

      .item {
        background: burlywood;  
        width: 200px;
        height: 200px;
      }
    </style>
</head>

<body>
    <div class="box">
      <span class="item"></span>
      <!-- <div style="background: yellowgreen; height: 200px; flex: 1;"></div> -->
      <span class="item"></span>
    </div>

    <!-- <div style="display: flex; flex-direction: column; justify-content: space-between; background: yellowgreen; height: 100%;">
          <span style="width: 20px; height: 20px; background: tomato;"></span>
          <span style="width: 20px; height: 20px; background: tomato;"></span>
        </div> -->
</body>
</html>
```



http://www.ruanyifeng.com/blog/2015/07/flex-examples.html

http://www.ruanyifeng.com/blog/2015/07/flex-grammar.html?utm_source=tuicool)