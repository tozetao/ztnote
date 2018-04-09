### rem
font size of the root element

rem是一个字体单位，设置了rem单位元素的字体大小是根据root元素(html)的字体大小来变化的，例如html字体大小是16px，那么1rem等于16px，它是根据自身的系数来计算的。

rem也可以作为高度、宽度等单位。

### 根据屏幕宽度的适配
由于rem会根据html元素的字体大小发生变化，因此将px单位替换成rem单位，根据手机端屏幕大小的不同来动态的修改html字体大小，达到适配效果。

具体实现以font-size=100和640px屏幕宽度作为标准，如果屏幕大于640px，则font-size=100，
如果屏幕小于640px，则将当前屏幕宽度大小除以640px，得到的比例系数乘以100算出当前手机html元素的字体大小，这样设计的话当手机屏幕宽度小于640px时，整个布局会根据对应的比例来缩小。

### 根据设备的DPR做适配
DPR适配仍然是基于rem单位来进行布局的，主要根据DPR的比例来动态的设置rem的基准值来达到自适应目的。



DPR是指设备像素与CSS像素的比例，例如iphone5的CSS像素是320px\*568px，DRP是2，对应的设备像素是640px*1136px，设计时按照1DPR=50px的比例来设置html的字体大小，这样是高清方案的适配。


640px宽度保证移动端俩边不会留白，是一个安全的最大宽度。
另外iphone5使用Retina屏幕，使用2px\*2px的设备像素来代表1px\*1px的css逻辑像素，它的css逻辑像素是320*568px，真实设备像素是640*1136px。

在布局的时候确定rem基准值为1rem=100px，原型图按照设备像素640px宽度来切图并布局即可。
由于是按照DPR布局，所以会根据不同屏幕的DPR值来缩放尺寸。

为什么效果图的宽度是640px或750px
苹果手机屏幕的DPR比例问题，iphone5的设备像素宽度是320，DPR是2，所以实际的效果图宽度是640px，这样方便切图布局且能还原高清效果。

并且rem的基准是是根据DPR比例变化的，1DPR=50px，如果效果图是640px，默认的高清方案1rem=100px，这个基准值能够快速的换算单位。







- 解决移动端真实1px像素问题
- 根据屏幕的DPR自动设置合适的高清设置

在布局的时候，



- 确定rem基准值：
- 确定rem数值

根据不同的屏幕大小或者不同的DPR确定rem基准值，即在当前设备上1rem等于多少像素，
布局的时候所有跟单位相关的属性都设置成rem单位的具体数值。

注意点：
- rem布局只适合固定尺寸
- 兼容IOS6以上和android2.1以上，基本覆盖所有流行的手机系统。



我有点明白了，使用rem布局的话，当确定一张设计图的时候，根据设计图的大小来布局就可以了。
最外层宽度自适应，其他的宽度使用rem尺寸即可。

我现在要使用vue.js做一个手机端布局，weiui他们是怎么做到的。？