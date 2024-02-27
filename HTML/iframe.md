### iframe
iframe会创建包含另外一个文档的内联框架，如果主页面是父文档，内联框架是子文档。
父子页面的css和js是完全隔离互不影响的，通信可以使用contentWindow和parent来进行通信。

适用场景：
- 沙箱隔离
- 引用第三方内容
- 独立的带有交互的内容，比如幻灯片

属性：
- scrolling：当iframe高度超出父文档元素高度时，iframe文档是否显示滚动条
- width：iframe高度
- height：iframe宽度

在父文档中访问子文档
```js
//获取iframe元素对象
var iframe = document.getElementById('iframe');
//获取iframe元素对象的文档对象
var iframe_document = iframe.contentWindow.document || iframe.contentDocument;
```

在子文档中访问父文档
```js
//父文档的document对象
var pdocument = window.parent.document
```

登录页面避免session失效问题
- if(window.top !== window.self){ window.top.location = window.location;}