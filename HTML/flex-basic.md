flex-basic影响容器可用空间的大小，因此也影响了子项的弹性量计算。

example：

```html
<style>
    .container {
      margin-top: 1rem;
      display: flex;
      width: 930px;
      border: 1px solid #cdc;
    }
    .container > div {
      padding: 20px 0;
      font-size: 20px;
      
      flex: 1;
    }
    
    .container > div:nth-child(1) {
      background-color: antiquewhite;
      flex-grow: 0;
    }
    .container > div:nth-child(2) {
      background-color: hsl(255, 88%, 76%);
    }
    .container > div:nth-child(3) {
      background-color: hsl(175, 55%, 65%);
    }
</style>
<body>
    <div class="container">
        <div>This is a Loooooooooooooooooooooooooooooooooooooong text</div>
        <div>Hello World</div>
        <div>Fast</div>
    </div>
</body>

```

我们用上面这个示例来解释，如果子项设置为 flex: 1 1 0%，第一个子项宽度计算过程如下：

```
容器的可用空间 = 930px
子项弹性量 = 930px / 3 * 1 = 
子项宽度 = 0(flex-basic) + 
```



如果子项设置为 flex: 1 1 160px，第一个子项宽度计算过程如下：

```
容器可用空间 = 930px - (160px * 3) = 
子项弹性量 = 
```



如果flex-basic设置为auto，且未设置width，flex-basic会等于子项最大内容宽度。而如果设置了width属性值，flex-basic则等于width值。



flex: initial，相当于flex: 0 1 auto. 

子项会收缩自身以适应容器，但不会去吸收flex容器中额外空间来自适应容器。

表现：当容器空间足够时，子项的宽度为最大内容宽度；当容器空间内容不够时，子项宽度会缩小，直到最小内容宽度。



flex-basic与width的关系：

当子项有明确设置了width、inline-size时，这时子项的宽度取决于俩种情况：

1. flex子项计算出来的宽度大于width，则采用计算出来的值。

2. flex子项计算出来的宽度小于width，则不采用计算的宽度值。如果width大于子项最小内容宽度，则采用最小内容宽度。如果最小内容宽度大于width属性值，则采用width属性值。

这有什么用? 

```css
/** 实现flex子项均分容器宽度：子项计算出来的宽度一定大于0，因此能够均分容器。**/
.items {
    flex: 1;
    width: 0，
}
/** 希望子项的宽度为最小内容宽度，或者是子项最小内容宽度超出你的预期时，不超过width属性值宽度。**/
.items {
    flex: 1;
    width: 180px;
}
```





相关计算公式：

```
子项的弹性量 = (flex容器总宽度 - flex-basic总和 - 已计算子项宽度总和) / flex-grow总和 * 子项的flex-grow值
子项的宽度计算 = flex-basic + 子项弹性量。
```





