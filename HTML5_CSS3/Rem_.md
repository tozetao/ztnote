### 单位概念

px、em、rem都是长度单位。

px：pixel，像素长度单位，是屏幕上显示数据的最基本的点。

pt：point，专用的印刷单位"磅"，它是一个绝对大小。



### DPR

DRP是设备像素比，DPR = 物理像素 / 设备独立像素

- 物理像素

  一个物理像素是显示器上的物理显示单元，物理显示单元可以认为是像素颗粒。例如iPhone6上有750*1334个物理像素颗粒。

- 设备独立像素

  英文：device independent pixels，简称dips

  可以认为是计算机中的一个点，这个点表示由程序使用的虚拟像素（例如css像素），有时把设备独立像素称为逻辑像素。

在iPhone6上，设备宽度和高度为375pt\*667pt，可以理解为设备独立像素；而其dpr为2，根据公式可以得知它的物理像素为750pt\*1334pt，例如：

```css
witdh: 2px;
height: 2px;
```

该css样式在不同屏幕上，css像素呈现的物理尺寸是一样的，不同的是css像素所对应的物理像素具数是不一样的。在普通屏幕上1个css像素对应1个物理像素，而在Retina屏幕下，1个css像素对应4个物理像素。



由于DPR的原因，设计稿会采用手机的物理像素来进行设计，比如基于iPhone6，设计稿会以650px宽度作为单位进行设计。而前端开发在计算元素的尺寸大小时，通常会除以DPR值的大小，计算出元素在屏幕上的实际尺寸大小。

```javascript
window.devicePixelRatio
// 设备像素宽度比

window.documentElement.clientWidth
// 设备的宽度

window.innerWidth
// 获取屏幕宽度
```



### rem

root em，简称rem。

rem是css3新增的一个相对单位，rem在为元素设定长度大小时，是相对于HTML根元素font-size设置的大小。例如HTML元素font-size是16px，那么1rem就等于16px。



rem基准值

使用rem布局的话，就需要根据屏幕的大小来确定root元素的基准值。

rem基准值 = 手机屏幕宽度 / 10，除以10是不想让rem基准值过大，当然也不可以不除。例如iPhone6的屏幕宽度是375px，rem基准值 = 375px/10 = 37.5px。



rem应用

确定rem基准值后，页面中的元素都可以通过rem单位来进行设置，它们会根据html元素的font-size大小值做相应的计算，从而实现屏幕的适配效果。

例如rem基准值是32px，一个元素的宽度是100px，那么该元素宽度 = 100px/32px，等于3.125rem。在实际的应用中，会使用前端构建工具来计算rem应用值，例如使用scss工具，通过定义function来计算。

```css
/* $rem是基准值 */
@function px2rem($px){ $rem : 37.5px; @return ($px/$rem) + rem;} 
```

注：直接应用会有一个缺陷，即像素1px问题。



rem进阶

视觉稿大部分是基于iPhone的，因此视觉稿的尺寸是对应物理像素的，在使用设计稿时会将标注的尺寸/2来进行css编码，然而在实际中是可以直接使用设计稿的尺寸来进行编码的。



```html
<meta name="viewport" content="width=device-width," />
```

- width

  指定viewport的宽度

- device-width

  指设备逻辑像素宽度，device-width = 物理像素 / (DPR * scale)，例如iPhone6的物理像素宽度是750，DPR为2，如果scale值为1，那么设备逻辑像素宽度为375；如果scale值为0.5，设备逻辑像素宽度为750.



为了让设计稿与页面大小（设备独立像素）保持一致，可以根据DPR的值，来动态的设置viewport的scale来动态的改变页面大小，flexible.js便是采用这种解决方案的。

- 保证页面大小与物理像素大小一致

  ```javascript
  var scale = 1 / devicePixelRatio;
  document.querySelector('meta[name="viewport"]').setAttribute('content','initial-scale=' + scale + ', maximum-scale=' + scale + ', minimum-scale=' + scale + ', user-scalable=no');
  ```

- 动态计算html的font-size

  ```javascript
  document.documentElement.style.fontSize = document.documentElement.clientWidth / 10 + 'px';
  ```

布局的时候，各元素的css尺寸 = 设计稿尺寸 / (设计稿横向分辨率 / 10)，font-size可能需要额外的媒介查询，并且font-size不使用rem，这一点跟网易是一样的。 





参考：

https://www.cnblogs.com/well-nice/p/5509589.html

http://www.cnblogs.com/PeunZhang/p/3407453.html#meta_1



rem缺陷

- 当用作图片或者一些不能缩放的展示时，必须要使用固定的px值，因为缩放可能会导致图片压缩变形等。 



弹性布局

关键元素高宽和位置不变，只有容器元素在做伸缩变换，对于这类布局开发的原则是：文字流式、控件弹性、图片等比缩放。