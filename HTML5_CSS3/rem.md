在做h5适配时，会制定设计稿的标准尺寸，比如说750px * 1364px。
对于大于标准尺寸的屏幕，我们希望元素能够等比例的放大。
而对于小于标准尺寸的屏幕，我们希望元素能够等比例的缩小。

使用rem单位后，元素的大小就不是一个固定的数值了。
计算的公式为：像素(px)大小 = rem值大小 * 基准单位。

比如说我们的设计稿是750px宽度，有一个元素为176px*176px，基准单位是75px，
它的rem值为2.346666，对应不同屏幕大小的大小会发生如下变化：
64	152px
75	176px	2.346666rem
82	192px
随着你的基准值不同，元素的大小都会发生调整。

这就是rem的适配原理了。











设备像素：现实屏幕中的物理像素。
css像素：操作系统中的概念。

屏幕窗口：显示器屏幕的宽度，以设备像素度量。
浏览器尺寸：以css像素度量，定义了用户的内容区域，可供你css布局占用。





### layout viewport

布局窗口

度量：css像素

通过document.documentElement.clientWitth/Height获取layout viewport的尺寸。

layout viewport的宽度每个浏览器的实现都不同，然而html元素的宽度继承于layout viewport，所以我们的css要预先准备好要处理的屏幕（layout viewport）是不是远超过手机屏幕的宽度。

简单的说就是要预先设置好layout viewport的尺寸，这样css样式才能正确显示。





### visual viewport

虚拟窗口，是当前显示在屏幕上的部分页面。

度量：css像素。

用户会滚动页面来改变可见部分，或者缩放浏览器来改变visual viewport的尺寸。如果用户放大浏览器，更少的css像素就放入了屏幕，这时候虚拟窗框的宽高就变小了。

通过window.innerWidth/Height获取visual viewport的宽度。



### html

html元素的整体尺寸。

度量：css像素。



### Screen

屏幕尺寸。

度量：设备像素。

可以通过screen.width/height获取宽高。



### 滚动/位移

window.pageX/YOffset

度量：css像素。

在手机浏览器中，pageX/YOffset是指visual viewport相对于layout viewport的距离，这就是滚动和位移。



### viewport的meta标签

用于设置layout viewport的宽度。

```html
<meta name="viewport" content="width=device-width"/>
```

该标签的意思是设置layout viewport的width。device-witdh是由设备像素来度量，它是手机屏幕宽度，由screen.width获得。

如果创建一个页面不为它赋值width，那么这个页面会伸展开来占据100%的layout viewport的宽度。





问题：

screen获取的是屏幕的宽高，虽然度量单位是设备像素，但是要知道设备的真实分辨率，还要考虑设备像素比，比如iphone6，screen.width获取的是340，但是它的真实分辨率是340*2，因为它的DPR是2。

