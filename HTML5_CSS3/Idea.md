

响应式布局

即应用在不同设别上都能够正常展示，其核心思想是定义不同的设备宽度断点（不同的设别宽度区间），编写不同设备断点的CSS样式。这样通过媒体查询，当设备的宽度满足某个设备宽度断点（区间）时，对应的CSS就会被应用，从而使用不同的设备宽度。

比如Fomantic-UI的container容器，设置了以下4个设备宽度断点：

```css
@media only screen and (max-width: 767px) {}
@media only screen and (min-width: 768px) and (max-width: 991px) {}
@media only screen and (min-width: 992px) and (max-width: 1199px) {}
@media only screen and (min-width: 1200px) {}
```

这样就可以保证container容器在不同设备下有固定宽度。

同理在布局时也可以应用这种理念，设计页面在不同设备宽度断点下的表现，并编写对应样式，满足响应式设计需求。