### 像素

px即像素，是画面中最小的点，不同设备上1个单位像素块的大小是不一样的，一个像素块就是一个颜色，像素有物理像素和设备独立像素之分。

- 物理像素 

  显示设备中最微小的物理部件，每个像素可以被操作系统设置自己的颜色和亮度。

- 设备独立像素

  由程序使用的虚拟像素，比如CSS像素，可以由系统转换为物理像素。

- 设备像素比

  设备像素比简称dpr。

  设备像素比 = 物理像素 / 设备独立像素

- px：像素，是一个虚拟长度单位。

- pt：点，是一个物理长度单位，1pt等于1/72英寸，它一般是印刷单位。



举例：iphone6的375*667是以设备独立像素的尺寸，它的DPR是2，所以它的物理像素是750\*1334。一般的设计师给出的设计稿也是以物理像素为大小的。



### rem

root em，简称rem。

rem是css3新增的一个相对单位，rem在为元素设定长度大小时，是相对于HTML根元素font-size设置的大小。例如HTML元素font-size是16px，那么1rem就等于16px。



- 如何将设计稿的尺寸转换为rem单位?

  要将像素单位转换为rem单位，根据rem的定义我们需要知道1rem等于多少像素，也就是它的基准值是多少。

  假设设计稿是750 * 1334，我们将设计稿分为100份，设定1rem等于10份，那么整个设计稿宽度为10rem。它的基准值为1rem等于75px。



- 如何转换一个元素的尺寸?

  比如有一个元素是200px * 200px，对应的rem尺寸是200px/75px * 200px/75px，用元素的尺寸除以基准值的大小即可。



### 1像素问题。

用iphone6举例，它的设备独立像素是375 *  667，物理像素是720 * 1334。

如果让virtual viewport（虚拟视窗）和layout viewport（布局视窗）的缩放比例一致，即缩放比例为100%，那么就会导致1个css像素占用4个物理像素，假设你的设计图是720px * 1334px，做出来的页面比如线条就会比原设计图的粗。





rem缺陷

- 当用作图片或者一些不能缩放的展示时，必须要使用固定的px值，因为缩放可能会导致图片压缩变形等。 







https://github.com/amfe/article/issues/17

https://www.w3cplus.com/css/viewports.html





### flexible

```html
<meta name="viewport" content="width=device-width," />
```

- width

  指定layout viewport（布局视窗）的宽度。

- device-width

  指设备宽度，device-width只与设备有关，device-width = 物理像素 / (DPR * scale)。
  
  例如iPhone6的物理像素宽度是750，DPR为2，如果scale值为1，那么设备宽度为375；如果scale值为0.5，设备逻辑像素宽度为750。



让布局视窗的宽度与设计稿的宽度保持一致其实就是让CSS像素与物理像素保持同一个比例。

为了保证这点，可以根据DPR的值，来动态的设置viewport的scale来动态的改变页面大小，flexible.js便是采用这种解决方案的。

- 保证页与物理像素大小一致

  ```javascript
  var scale = 1 / devicePixelRatio;
  document.querySelector('meta[name="viewport"]').setAttribute('content','initial-scale=' + scale + ', maximum-scale=' + scale + ', minimum-scale=' + scale + ', user-scalable=no');
  ```

- 动态计算html的font-size

  ```javascript
  document.documentElement.style.fontSize = document.documentElement.clientWidth / 10 + 'px';
  ```

布局的时候，各元素的css尺寸 = 设计稿尺寸 / (设计稿横向分辨率 / 10)，font-size可能需要额外的媒介查询，并且font-size不使用rem，这一点跟网易是一样的。 













