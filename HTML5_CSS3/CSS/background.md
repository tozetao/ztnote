## background
为元素这是背景颜色或背景图片。

- 填充背景颜色
- 填充图片：该属性能够改变图片的平铺方式，能够以元素的左上角作为起点坐标，以水平方向或垂方向来控制图片的显示
```html
# 填充图片
background: url()

# 填充方向的设置
repeat-x，横向平铺
repeat-y，纵向平铺
no-repeat，不填充，只进行显示一次。

# 控制图片的显示方向
# 关键字的控制
background: url(./phone.jpg) no-repeat right top

# 百分比的控制
# 水平方向：从左到有分别是0-100%
# 垂直方向：上上到下分别是0-100%
background: url(./phone.jpg) no-repeat 100% 0

# px像素的控制
# 同百分比一致。
background: url(./phone.jpg) no-repeat right top
```
