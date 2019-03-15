@font-face是h5自定义字体模块，如果想要自定义自己的字体文件，通过该属性即可。

@font-face{
	//自定义字体名称
	font-family: family-name;
	
	//自定义字体路径, format用于自定义字体格式，帮助浏览器识别
	src: url format font-face-name

	//
	font-weight:normal;

	font-style:normal;
}

要点：
- 兼容多个浏览器，不同浏览器能够识别的字体格式不同
- 支持IE9的兼容模式
- 支持IE6的解析格式

```html
@font-face {
  font-family: 'icomoon';
  src:  url('fonts/icomoon.eot?44v19x');
  src:  url('fonts/icomoon.eot?44v19x#iefix') format('embedded-opentype'),
    url('fonts/icomoon.ttf?44v19x') format('truetype'),
    url('fonts/icomoon.woff?44v19x') format('woff'),
    url('fonts/icomoon.svg?44v19x#icomoon') format('svg');
  font-weight: normal;
  font-style: normal;
}

[class^="icon-"], [class*=" icon-"] {
  /* use !important to prevent issues with browser extensions that change fonts */
  font-family: 'icomoon' !important;
  speak: none;
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;

  /* Better Font Rendering =========== */
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

.icon-cart:before {
  content: "\e93a";
}
```
