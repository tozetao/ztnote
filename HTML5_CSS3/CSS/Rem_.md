px、em、rem都是长度单位。

px：像素长度单位，像素px是相对于显示器分辨率而言的。



### rem

root em，简称rem。

rem是css3新增的一个相对单位，rem在为元素设定长度大小时，是相对于HTML根元素font-size设置的大小。例如HTML元素font-size是16px，那么1rem就等于16px。



设计稿/视觉稿

web app应用的设计效果图，由于应用页面会运行在不同机器屏幕上，所以设计稿会以一个基准的手机屏幕大小来进行设计。

注：设计稿是以分辨率来表示尺寸大小的。



rem基准值

要使用rem布局的话，就需要根据屏幕的大小来确定root元素的基准值。

rem基准值 = 手机屏幕宽度 / 10，除以10是不想让rem基准值过大，当然也不可以不除；例如iPhone6的屏幕宽度是375px，rem基准值 = 375px/10 = 37.5px。

- window.innerWidth，获取屏幕宽度



rem应用

使用rem时需要手动的去计算rem的具体大小，例如rem基准值是32px，一个元素的宽度是100px，那么该元素宽度 = 100px/32px，等于3.125rem。

在实际的应用中，会使用前端构建工具来计算rem应用值，例如使用scss工具，通过定义function来计算。

> @function px2rem($px){ $rem : 37.5px; @return ($px/$rem) + rem;} 

注：$rem是基准值



### DPR

DRP是设备像素比，DPR = 物理像素 / 设备独立像素

- 物理像素

  一个物理像素是显示器上的物理显示单元，物理显示单元可以认为是像素颗粒。例如iPhone6上有750*1334个物理像素颗粒。

- 设备独立像素

  英文：device independent pixels，简称dips

  可以认为是计算机中的一个点，这个点表示由程序使用的虚拟像素（例如css像素），有时把设备独立像素称为逻辑像素。

> window.devicePixelRatio，获取设备像素宽度

简单的说，手机屏幕的尺寸对应逻辑像素，而物理像素与手机使用的屏幕有关。

在iPhone6上，屏幕宽度是375px，即逻辑像素的大小，而它的物理像素是750px，所以它的DRP值为2。也就是说一个逻辑像素，在x轴和y轴上，各需要俩个物理像素来显示。

大部分设计稿采用iPhone6的屏幕来进行设计，所以分辨率大小为：750 * 1334 ，而前端开发在计算元素的尺寸大小时，通常会除以DPR值的大小，计算出元素在屏幕上的实际尺寸大小。



rem进阶

在明白DPR后，计算rem基准值时，我们会使用

viewport

scale



拿到了dpr之后，我们就可以在viewport meta头里，取消让浏览器自动缩放页面，而自己去设置viewport的content例如（这里之所以要设置viewport是因为我们要实现border1px的效果，加入我给border设置了1px，在scale的影响下，高清屏中就会显示成0.5px的效果）

meta.setAttribute('content', 'initial-scale='+ 1/dpr + ', maximum-scale='+ 1/dpr + ', minimum-scale='+ 1/dpr + ', user-scalable=no');



设置完之后配合rem，修改

@function px2rem($px){ $rem : 75px; @return ($px/$rem) + rem;}

双倍75，这样就可以完全按照视觉稿上的尺寸来了。不用在/2了，这样做的好处是：

1. 解决了图片高清问题。
2. 解决了border 1px问题（我们设置的1px，在iphone上，由于viewport的scale是0.5，所以就自然缩放成0.5px）
   

在iphone6下的例子：

我们使用动态设置viewport，在iphone6下，scale会被设置成1/2即0.5，其他手机是1/1即1.

meta.setAttribute('content', 'initial-scale='+ 1/dpr + ', maximum-scale='+ 1/dpr + ', minimum-scale='+ 1/dpr + ', user-scalable=no');



https://www.cnblogs.com/well-nice/p/5509589.html

http://www.cnblogs.com/PeunZhang/p/3407453.html#meta_1



rem缺陷

- 当用作图片或者一些不能缩放的展示时，必须要使用固定的px值，因为缩放可能会导致图片压缩变形等。 



弹性布局

关键元素高宽和位置不变，只有容器元素在做伸缩变换，对于这类布局开发的原则是：文字流式、控件弹性、图片等比缩放。