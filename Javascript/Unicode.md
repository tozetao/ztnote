```html
<!doctype html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport"
          content="width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>Document</title>
    <style>
        body {
            padding: 0;
            margin: 0;
        }
        .box1 {
            background-color: aqua;

            display: inline-block;
            width: 200px;
            height: 100px;

            margin-right: 8px;
        }

        .box2 {
            background-color: cadetblue;
            padding: 7px 15px;

            font-size: 12px;
            border-radius: 3px;

            display: inline-block;

            vertical-align: middle;
        }
    </style>
</head>
<body>
    <div id="c1"></div>
    <div id="c2"></div>
    
    <p>
        &lt;h2>
    </p>
    <p>
        &#65;
    </p>
</body>
</html>
<script>
    
    // 字符转换成unicode
    function charToUnicode(char) {
        const unicode = char.codePointAt(0)
        if (unicode > 0xffff) {
            return `\\u${char.charCodeAt(0).toString(16).padStart(4)}\\u${char.charCodeAt(1).toString(16).padStart(4)}`
        }
        return `\\u${char.charCodeAt(0).toString(16).padStart(4)}`
    }

    // 在字面量定义中，\u会被当成unicode码点进行处理。
    var c1 = '𠮷'
    var r1 = charToUnicode(c1)
    console.log(r1)      // \ud842\udfb7
    document.getElementById('c1').innerHTML = r1
    
    function charToHtmlEntity(char) {  
        const codePoint = char.charCodeAt(0);  
        const entity = char.replace(/[<>&"'`]/g, function(match) {
            const entityCode = match === '<' ? '&lt;' :  
                            match === '>' ? '&gt;' :  
                            match === '&' ? '&amp;' :  
                            match === "'" ? '&apos;' :  
                            match === '"' ? '&quot;' :  
                            '';  
            return entityCode;
        });  
        return '&#' + codePoint + ';';  
    }

    var c2 = '<img src="x" onerror="alert(1)">'
    var r2 = c2.split('').map(charToHtmlEntity).join('')
    console.log(r2)
    document.getElementById('c2').innerHTML = r2

    /*
        XSS攻击：允许用户提交可以执行的JS代码，HTML标签和样式代码。
        一般PHP防御XSS攻击，是将用户提交的数据中，包含< > ' " &等特殊字符转换成HTML实体编码，避免后台输出的数据会被当作HTML标签进行解析。


        但是富文本是允许用户提交html代码的，如何避免用户在标签用执行JS代码?
    */
</script>

```

