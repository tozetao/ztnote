**空白字符的处理**

合并空白字符：在HTML中，连续的空格、制表符（tab）、换行符等空白字符通常会被浏览器合并为一个空格显示。这是因为在标准的HTML渲染中，连续的空白字符被视为一个单一的空格分隔符。

**`white-space` 属性对空白字符的影响**：

- `normal`（默认值）：连续的空白字符会被合并为一个空格，并且文本会自动换行以适应容器的宽度。
- `nowrap`：连续的空白字符会被合并为一个空格，但文本不会自动换行，即使超出容器的宽度。
- `pre`：空白字符会被保留，就像它们在 `<pre>` 标签中一样，包括空格、制表符和换行符，并且文本不会自动换行。
- `pre-wrap`：连续的空白字符会被合并为一个空格（但换行符会被保留为换行），并且文本会在必要时自动换行。
- `pre-line`：合并连续的空白字符（但保留换行符），并且只会在换行符处换行。
- `break-spaces`：保留所有空白字符，并且可以在空白字符处换行（这是一个较新的值，可能不是所有浏览器都支持）。

示例：

```html
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>White-space Example</title>
<style>
.normal { white-space: normal; }
.nowrap { white-space: nowrap; }
.pre { white-space: pre; }
</style>
</head>
<body>
<p class="normal">
    This    is    a    paragraph    with    multiple    spaces.
</p>
<p class="nowrap">
    This    is    a    paragraph    with    multiple    spaces.
</p>
<p class="pre">
    This    is    a    paragraph    with    multiple    spaces.
</p>
</body>
</html>
```

简单的说，white-spact控制了如何处理内容的空白字符以及是否换行，而像word-break之类的属性控制了文字内容超出容器后怎么去截断字符来换行。



**word-break**

该属性指定了怎么样在单词内断行。即文本超出容器后该如何断行。

- normal: 非cjk按照空格换行（即使该单词超出容器），cjk文本超出容器就换行。
- break-all: 以任意个字符的断点处进行换行
- keep-all: cjk文本不换行，非cjk文本表现与normal一样：

```html
<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<script src="http://g.tbcdn.cn/mtb/lib-flexible/0.3.4/??flexible_css.js,flexible.js"></script>
	<title>Document</title>
	<style type="text/css">
		* {
			box-sizing: border-box;
		}
		
		.container {
			width: 200px;
			border: 1px solid #cdc;

			font-size: 20px;
			font-weight: bold;
			padding: 6px;

			margin: 30px;

			/* 
			white-space
			指定是否换行，以及如何换行。
			指定空白子服是否合并，以及如何合并。

			如果要使单词可以在其内部被截断，可使用overflow-wrap、word-break或hyphens。
			*/
			white-space: pre-wrap;
			word-break: normal;	
		}
	</style>
</head>
<body>
	<div class="container">
		But    ere 	she from the   church-door and told us why: 
		'Wooooooooooooooooooooooooooooooooooo~'
		'It was a wicked woman's curse,' Quoth she, 'and what care I?' She smiled, and smiled, and passed it off Ere from the door she stept—
	</div>
</body>
</html>

<script>
</script>
```

