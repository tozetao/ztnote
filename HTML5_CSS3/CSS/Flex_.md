## Flex布局
任何元素都可以指定成flex布局。

### 1. flex容器(flex container)
当一个元素定义成flex布局后，该元素被称为flex容器(flex container)，它的所有子元素自动称为Flex Item。
flex容器有水平方向的主轴和垂直方向的交叉轴，项目默认沿着主轴排列。

容器属性：
- flex-direction：决定项目的排列方向
- flex-wrap：如果项目在一条主轴下排列不下，该如何换行
- flex-flow：flex-direction和flex-warp属性的简写形式，默认值row nowarp
-  
- justify-content：定义项目与主轴的对齐方向，默认左对齐
- align-items：定义项目在交叉轴上的对齐方式
- align-content：定义了多根轴线(多行)的对齐方式，如果只有一根轴线或者容器没有高度则属性不生效。

总结：
- 控制item在主轴和交叉轴的对齐方式，item在交叉轴对齐上比较特殊，分为一条交叉线对齐和多条交叉轴对齐的情况。
- 控制item的排列方式和换行方式


### 2. item属性
- ordre
定义项目的排列顺序，数值越小排列越前

- flex-grow
定义项目的放大比例，默认0，表示有剩余空间也不放大。
如果项目都为1，item将等分剩余空间；
如果某个项目为2，其他项目为1，前者占据的剩余空间将比其他item多出一倍。


- flex-shrink：定义项目的缩小比例，默认1，表示空间不足情况下项目会缩小。
如果所有项目的flex-shrink的属性为1，当空间不足时都将等比例缩小；
如果一个项目的flex-shrink属性为0，其他为1，在空间不足时前者不会缩小，其他等比例缩小。
负值无效。

- flex-basis
定义item在分配多余空间时，项目占据主轴空间的大小。
浏览器会根据这个属性来计算主轴是否有多余空间，默认值auto，即项目本身的大小。

- flex
flex属性是flex-grow, flex-shrink 和 flex-basis的简写，默认值为0 1 auto。后两个属性可选。