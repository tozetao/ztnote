<!-- 
一、javascript基础
	1. js与dom的关系
		浏览器有渲染html代码的功能，把html源码在内存里形成一个dom对象，就是文档对象。
		浏览器内部有一个js的解释器/执行器/引擎，如chrome，用v8引擎。
		我们在html里写一个js代码，js代码被引擎执行，而执行的结果，就是对dom的操作。
		而对dom操作的结果，就是我们看到的特效，如图片漂浮，文字变色。

		学习js要分清
		a js本身的语法
		b dom对象：文档对象，把body、div、p等节点树看成一个对象。
		c bom对象：把浏览器的地址栏、历史记录、dom对象等封装成一个对象。

		浏览器是宿主，但js的宿主不限于浏览器，也可以是服务端，如比较流行的服务器端js框架：node.js

	2. js语法
		2.1 变量
			临时存储数值的容器，变量存储的数值是可以变化的。
			a. var关键字
				用于声明变量。弱类型语言，没有严格的数据类型要求。
			b. 命名规则
				js的变量名可以用字母、数字和下划线声明。
				首字符不能是数字，同时不能使用javsscript关键字。
				
				变量名区分大小写。

		2.2 数据类型
			4种标量、2种复合类型、2种特殊类型
			2.2.1 四种标量（原生）
				string：字符类型。
				number：数值类型
				boolean：布尔类型，true、false
				undefined：未定义类型。对应原生类型，var i; i没有赋值的时候，就是undefined。
			2.2.2 俩种复合类型
				object、array
			2.2.3 特殊类型
				null
			PS：null对应对象，undefined对应原生类型。

		2.3 js的运算符
			2.3.1 算术运算符
				+ - * / % ++ --
				i++：先运算后自加
				++i：先自加在运算

			2.3.2 比较运算符
				> < >= <= == != === !==
				==：值判断是否相等
				===：除了值，本身的类型也要相等

			2.3.3 逻辑运算符
				与 && 或 || 非！
				在js中，逻辑运算，返回的是最早能判断表达式结果的值。
				example1：
					var a=false;
					var b=6;
					var c=true;
					var d=(a || b || c);//
					console.log(d);// 6
				example2:
					var e=false;
					var f=99;
					console.log(e && f);//false
				example3:
					window.hello=window.hello || window.word;
					//这段代码用于做兼容控制，若哪个值存在，则返回哪个值。

			2.3.4 赋值运算符
				= += -= *= /= %=
				+=：例：i+=j;  i = i+j;
				左边的变量与右边的变量，先进行运算，然后赋值给左边的变量

			2.3.5 字符串运算符
				+
				在拼接的时候，若碰到字符串，后面的拼接内容一律作为字符串处理。
				example:
				console.log(2+3+4+'hha'+5+6);//9hha56，

		2.4 流程结构
			2.4.1 顺序结构
				一行接着一行运行。
				注意：文档是按照顺序输出的，所以你的脚本代码最好写在元素的最下方，这样某些代码需要获取元素的时候，才不会发生空对象。
				或者使用下面代码
				window.onload=function(){
					document.getElementById('xxx');
				}
			2.4.2 分支结构
				if...
				if...elseif...elseif...else
				switch...
				//if example
					var i=3;
					if(i==1){
						document.write('今天是1');
					}else if(i==5){
						document.write('本周最后1天');
					}else{
						document.write('现在又是工作日');
					}
				//switch example
					switch(i){
					case 1:
						document.write('星期1');
						break;
					case 2:
						document.write('星期二');
						break;
					default:
						document.write('写代码');
					}
			2.4.3 循环结构
				for...
					for(var i=0;i<10;i++){}
				while...
					while(i<10){xxx; i++;}
				do...while...
					do{xxx;i++;}while(i<10)

				break：结束整个循环。
				continue：结束当前循环，继续下次循环。
				
				for...in...	相当于foreach，遍历数组或者对象
				example：
					var obj={name:'zhangsan',age:29,area:'bj'};//json对象
					for(var k in obj){
						console.log(k);//输出json对象的属性值
						console.log(obj.k);//错误写法，这里会被认为是输出obj的k属性的值
						console.log(obj[k]);//正确写法
					}

		2.5 函数
			2.5.1 系统函数
				alert(); confirm();print();
			2.5.2 自定义函数
				使用函数前要先定义才能调用。
				函数定义有三个部分:函数名,参数列表,函数体
				格式：
				function 函数名([参数1,参数2...]){
					函数执行部分;
					return 表达式;
				}
			2.5.3 形参与实参
				形参：在函数声明或定义时，设置的参数。
				实参：在函数调用时，传递的参数。
			2.5.4 arguments
				arguments是函数的参数数组列表。
				使用arguments属性在定义函数的时候  无须指明函数的参数列表 ，在函数调用时可以任意填充。实现一个函数的多种功能。
					
				example 1：
				function display(){
					document.write('姓名：'+arguments[0]+'；年龄：'+arguments[1]);
				}
				arguments参数是函数中存在的一个属性，其功能类似于数组，而非数组
				
				//example 2 循环arguments
				function display(){
					for(var i=0;i<arguments.length;i++){
						document.write(arguments[i]+"<br/>");
					}
				}
				display('张三','18','广东省广州市');

			2.5.5 函数本质
				是在内存中开辟一块连续的内存空间。
				//example 1
				function display(){
					alert('ninhao');
				}
				//var i = display();	//把函数执行的返回值，赋值给变量i
				var i=display;			//把函数的首地址返回给变量i
				i();					//通过括号来执行i变量所执行的函数
					
				内存分4个区
					栈区：变量，程序调用过程中，经常使用的。执行速度快，存储数据小。
					堆：对象，执行速度慢，存储数据大。
					代码区：函数
					全局区：变量、常量
				函数在内存中执行原理
					a. 把函数代码存储到内存的代码区中，display函数有自己对应的内存地址。
					b. var i=display，栈中开辟内存空间，存储变量i，把函数对应的内存地址(首地址)赋值给i
					c. i()，通过()js会认为这是一个函数，那么就会找变量i指向的内存地址，来执行函数。
					
			2.5.6 匿名函数
				js属于弱类型语言，在定义时，不需要指定数据类型，赋什么类型的值，就是什么类型的数据。
				//example
				var i=function(){}	=>这种就是匿名函数。
				
				自调用匿名函数
					//example
					(function(alert('hello');){})(); =>这种是自调用匿名函数
					(function(name){alert(name);})('zetao')；也可以这样传递参数进去
					
					function(){}()
					这种情况下会报错，因为在运算符里面，小括号的运算优先级别是最高的，这段代码会先执行小括号，所以会报错，当你给函数用小括号包起来的时候，就不会错了。
					
					自调用匿名函数的好处？
					避免函数冲突，重命名等问题。
					function good() {}  demo01.js
					function good() {}  demo02.js
					在上面代码中，分别加载俩个js文件，由于函数名一样，会产生冲突。
					
					(function good() {})()  demo01.js
					(function good() {})()  demo02.js
					通过这种方式，就不会发生冲突现象了。

		2.6 script的执行过程
				js代码段，是分段来执行的。
				1.script代码段执行流程
					a.读入第一个代码段
					b.编译：声明变量、声明函数、语法检查、代码优化，分析并得到代码树
					c.执行：执行编译的结果，变量的赋值，函数的调用都是代码的执行过程。
					d.读入下一个代码段，重复上面过程，直到没有代码段。
					PS：赋值、函数调用都是在执行阶段在做的。
					
				2.不同类型的错误对当前代码段的影响
					a.执行错误，下面的代码不会被执行
						alert('first');
						dis();	//没有dis函数，执行错误，下面的代码都不会被执行
						alert('second');
					b.编译错误，整个代码都不会被执行。
						alert('first');
						dis(;	//编译错误，不会被执行。
						alert('second');
						
					c.多个代码段，编译错误的代码端不会影响其他无错代码段的执行。
						<script language='JavaScript'>
						alert('first');
						dis(;	//编译错误，不会被执行。
						alert('second');
						</script>
						<script language='JavaScript'>
						alert('hello');	//这个代码段继续被执行
						</script>
					d.多个代码段，执行错误的代码段不会影响其他无错代码段，也不会对执行错误上面的代码产生影响。
						<script language='JavaScript'>
						alert('first');
						dis();	//执行错误，上面的代码仍会被执行
						alert('second');
						</script>
						<script language='JavaScript'>
						alert('hello');	//这个代码段继续被执行
						</script>

	3. 对象操作
		在js中，一切都可以当作对象，原生类型也可以做为对象，我们在对原生类型，比如字符串、数字、布尔值，在调用其方法的时候，js会包装成对象供我们使用。
		3.1 对象的访问方式
			var arr=new Array(1,2,3);//new Array对象
			//俩种赋值方式
			arr['name']='zhangsan';
			arr.age=30;
			//俩种访问方式
			console.log(arr.name);
			console.log(arr['age']);

		3.2 String 字符串对象
			length
				属性说明：
					返回字符串长度
			concat(string)
				方法说明：
					连接俩个或更多个字符串。
			indexOf(string)
				方法说明：
					返回子串出现的位置，失败返回-1。
				example：
				var str='hello word';
				str.indexOf('he');//返回0
				alert(str.indexOf('he') ? 'find':'not find');//输出not find，要注意索引会出现0。
				alert(str.indexOf('he')>-1 ? 'find':'note find')//正确写法

		3.3 Date对象
			Date对象需要手动声明。
			getYaer()：返回年份，2位或4位
			getFullYear()：返回年份，4位
			getMonth()：返回月份，0-11
			getDate()：返回日期，1-31
			getDay()：返回星期数，0-6
			getHours()：返回小时数，0-23
			getMinutes()：返回分钟数，0-59
			getSeconds()：返回秒数，0-59
			getMillseconds()：返回毫秒数，0-999，1秒=1000毫秒

		3.4 Math对象
			ceil
			floor
			min
			max
			pow
			random
			round
			sqrt

		3.5 数组对象
			数组：一组数据的集合，在内存表现一段连续内存地址。
			在js中，索引是从0开始的(0 1 2 ...)，若移除数组中任意一个元素，索引都会从头开始编排。

			一维数组
			二维数组
			
			多维数组
			$_FILES[‘photo’][‘temp_name’][0]
			$_FILES[‘photo’][‘temp_name’][1]
			
			3.5.1 定义
				语法：
				a. var arr=[1,2,3];
				b. var arr=new Array(1,2,3,4);
				c. var arr=new Array(size);
				arr[0]=xxx;arr[1]=xxx;arr[2]=xxx;
				建议使用a、b俩种方式即可
			
			3.5.2 访问数组
				js中只有索引数组，访问只能通过[]+索引来进行访问。

			3.5.3 模拟关联数组
				js的关联数组，其实就是array对象添加属性来进行模拟的一种方式。
				example：
					var arr=new Array(1,2,3,3,4,5);
					arr['name']='zetao';
					arr['age']=25;
					arr['height']=1.65;
					document.write(arr['name']);
					document.write(arr.age);
					document.write(arr.length);//输出6，属性不会被算在内。
				
				PS：可以通过for...in循环来访问索引和属性。

			3.5.4 函数
				arrayObject.push(newelement1,newelement2,....,newelementX)
					功能：在数组的尾部追加元素
				arrayObject.unshift(newelement1,newelement2,....,newelementX) 
					功能：在数组的头部追加元素
				arrayObject.splice(index,howmany,element1,.....,elementX) 
					功能：在指定位置，插入指定数量的元素
				arrayObject.pop() 
					功能：删除尾部元素
				arrayObject.shift() 
					功能：删除头部元素
				arrayObject.splice(index,howmany)
					功能：删除指定位置中的指定数量的元素
				arrayObject.slice(start[,end]) 
					截取指定位置的元素
				arrayObject.concat(arrayX,arrayX,......,arrayX)
						功能：把数组合并起来。
				数组与字符串的转换（implode explode）
				arrayObject.join(separator) 
					功能：相当于把数组转化为字符串、
					//example for join
					var arr=['张三','李四','王武'];
					var str=arr.join('|');
					document.write(str);
					
					var newstr=str.split('|');	//以目标字符串对字符串进行分割，返回一个分割好的数组
					document.write(newstr.length); 
		
		3.6 window对象
			window对象就代表着浏览器窗口，就是bom对象。
			window对象方法
				window.alert(message)
				window.confirm(message)
				window.close(message)
					关闭窗口
				window.print(message)
					打印信息
				window.prompt
				
				setintval
				clearitlval
				setTimeout(表达式,毫秒)
				clearTimeout(定时器对象)
			注：window对象是浏览器宿主对象,和js语言无关。

		3.7 window子对象
			3.7.2 navigator对象
				浏览器信息对象，是window对象的子对象。
				appCodeName
				appName
				appVersion
				cookieEnabled
				...
			3.7.3 location
				地址栏对象
				host：主机
				port：端口
				href：地址
				search：参数
				...
			3.7.4 history
				历史记录对象
				length：历史记录的数目。
				forward()：返回下一次访问的页面
				back()：返回上一次访问的页面。

			3.7.5 screen
				屏幕对象
				height：高度
				width：宽度
				availHeight：可用高度
				availHeight：可用宽度
				colorDepth：颜色
			
			3.7.6 document对象
				即html代码形成的对象，操纵此对象，可动态的改变页面的内容。
		
	4. js与dom对象
		dom对象便是document对象。
		要操作元素的样式，操作节点，做元素的事件处理，最重要的是先获取指定的元素，
		在得到该元素后，边可以进行一系列操作了。
		5.1 找对象
			5.1.1 id
				getElementById()：通过元素id来寻找节点对象。
			5.1.2 标签
				getElementsByTagName('p')
				说明：通过标签来寻找对象，返回一个Collection对象。
			5.1.3 name
				对于表单元素，使用使用name来查询。
				getElementsByName
				说明：根据name获取对象，返回NodeList对象。
			5.1.4 类名
				getElementsByClassName()
				说明：按照类名查找，返回对象集合
			5.2.5 寻找节点对象的子节点
				obj.children，这是一个非标准属性，但是标准浏览器支持很好的。

		5.2 操作对象的属性
			5.2.1 标签属性
				<img src='' alt='' title='' style='width:200px;height:100px;'/>
				img对象的属性：src、alt、title、style。
				普通属性通过 对象.标签 属性访问。
				标签属性与dom对象属性的对应关系，绝大部分俩者是相同的，如：imgobj.src属性对应<img src=''/>中的src属性。
				但也有例外，如：<div class='main'>中的class属性用divojb.className。

			5.2.2 css属性
				html元素的样式变化，在js中，是通过元素对象的style属性来改变的。
				css属性与dom对象属性的对应关系，2者通过obj.style.css属性名相对应，如obj.style.height、obj.style.width。

				如果css属性带有横线，如border-top-style，则把横线去除合并，合并横线后首字母大写。
				如：obj.style.borderTopStyle、obj.style.marginLeft

			5.2.3 获取运行时的style对象
				计算出的样式：
					对于一个元素，它有可能从父元素继承一些样式，也有可能是有class选择器、id选择器，所以浏览器就需要对元素的样式进行计算，最后再来渲染。

					运行时的style对象就是元素在内存中计算后的样式。
				
				方法：
					obj.currentStyle
					window.getComputedStyle(obj,伪元素)
					参数说明：
						obj：要获取计算后的样式的目标元素
						伪元素：期望的伪元素，如'after','fist-letter'等，一般设置为null。 
				注1：IE和Opera支持使用currentStyle，标准浏览器只支持getComputedStyle。
				
				注2：该style对象是只读的，无法直接更改样式，更改样式仍然需要元素的style属性。

				//获取元素在内存中计算后的样式
				function getStyle(obj,attr){
					var style=obj.currentStyle || window.getComputedStyle();
				}
		
		5.3 节点操作
			5.3.1 删除节点
				步骤：
				a. 找到节点对象
				b. 周到节点对象的父节点对象parentObj。
				c. parentObj.removeChild(子节点对象)。
			
			5.3.2 创建节点
				步骤：
				a. 创建对象
				b. 找到父对象parentObj。
				c. parentObj.addChild(对象)。

			5.3.3 暴力操作节点
				inserHTML：代表节点内的内容，能读能写。
				不是w3c规定的标准对象属性，但是各浏览器支持很友好。
				example：
					function add(){
						$('demo').innerHTML='';
					}
					<input type='button' value='add' onclick='add()'/>
					<ul id='demo'></ul>

	5. 定时器
		window.setTimeout('表达式','毫秒')
		函数说明：
			指定毫秒后执行一次语句
		
		setInterval(表达式，秒数)
		函数说明：
			指定几秒后循环执行表达式。
		
		clearTimeout()
		clearInterval()
		函数说明：
			用于清除定时器

		注：定时器不属于js的知识，它是window对象的方法。

二、js的事件
	行为、结构、样式相分离：便是将html代码、js代码、css样式进行分离。

	事件：用户对某个元素进行某个动作的时候，比如点击，这个过程就发生了一个事件，叫做点击事件。
	example：
		<input type='button' value='btn' onclick="alert('hello')"/>
		用户点击按钮，触发点击事件，js捕捉想要的事件，并调用对应的程序来处理。

	1. 常用事件
		文档加载、鼠标、键盘、表单等事件。
		onLoad
			当页面加载完毕后，执行（body）
		onUnload
			当页面关闭时，执行（body）
		onBlur
			失去焦点（text文本框）
		onFocus
			获取焦点（text文本框）
		onClick
			点击事件（按钮，html可视化的元素）
		
		onMouseOver
			鼠标经过（特效）
		onMouseOut
			鼠标离开（特效）
		onMouseDown
			鼠标按下
		onMouseUp
			鼠标弹起
		onMouseMove
			鼠标移动
		
		onChange
			当值改变时（下拉）
		onSelect
			元素选中时（主要用在仿复制）
		
		onkeypress
			键盘按下时 在按下的瞬间触发
		onkeydown
			键盘按下时 在按下还未弹出时触发
		onkeyup
			键盘弹起时
		onSubmit
			表单提交时
		onReset
			表单重置时

	2. 事件的绑定方式
		动态绑定与事件委托，都需要得到要操作的元素对象。

		2.1 将事件写在标签中
			example：
				<body>
					<div onlick='f1()' style='border:1px;'></div>
				</body>
				
				body标签整段代码组成结构。
				onclick是事件。
				style是样式。

		2.2	用事件属性绑定事件函数
			window.onload=function(){}
			好处：
				a. 完成行为的分离
				b. 可以用this引用当前元素对象
				c. 方便读取事件对象

		2.3 事件委托
			为元素的事件绑定要处理的程序。

	3. 事件对象
		在事件触发过程中，会自动生成一个事件对象，该对象保存了触发事件的相关信息。
		例如鼠标单击事件时：会保存鼠标x、y轴坐标信息。键盘按下事件时：会记录当前按键的ascii码。
		
		3.1 IE模型的语法
			window.event
			example ：
				window.onload=function(){
					$('content').onkeydown=function(){
						alert(window.event);	//弹出对象
					}
				}

		3.2 w3c模型的语法
			在w3c标准下，事件对象会由系统自动传递给事件函数的第一个参数。
			注：是第一个参数传入。
			语法：
				dom对象.事件=function(event){
					alert(event);
				}
			example 1：
				window.onload=function(){
					$('content').onkeydown=function(event){
						alert(event.type);
					}
				}
			example 2 判断键盘方向键：
				window.onload=function(){
					$('content').onkeydown=function(event){
						if(window.event){
							var code=window.event.keyCode;
						}else{
							var code=event.keyCode;
						}
						alert(code);
					}
				}
		
		3.3 事件对象的属性
			w3c标准模式
				target：代表当时触发事件时的具体对象。
			IE模式
				srcElement
			注：不同浏览器的属性各不相同。

	4. 事件监听(委托)
		说明：监听到某个元素触发的事件，会委托处理程序处理。
		事件监听的好处是：对一个元素对象的同一个事件，可以绑定多个处理程序。
		
		4.1 事件监听的语法
			IE监听
				obj.attachEvent(type,callback)
				参数说明
					obj：元素对象
					type：事件类型，如onclick、onfours等
					callback：回调用户，指绑定的处理程序。
				
				注1：触发顺序，后定义先触发，先定义后触发
				注2：只适用于IE浏览器。
					
				example :
					$('content').attachEvent('onclick',function(){
						alert(111);
					});
					$('content').attachEvent('onclick',function(){
						alert(222);
					});

			w3c浏览器
				obj.addEventListener(type,callback,capture)
				参数说明：
					obj：元素对象
					type：事件类型 onclick，但是在我们的W3C浏览器，是不需要写on的，直接写click即可
					
					callback：事件处理程序 函数（匿名函数）
					
					capture：以什么方式监听事件，有 冒泡模型和捕捉模型，默认值为false，采用冒泡模型，如果为true，将采用捕捉模型。

				注：addEventListener该事件监听程序，触发顺序和IE模型不同，其采用的是：先定义先触发，后定义后触发

				example :
					$('content').addEventListener('click',function(){
						alert(111);
					});
					$('content').addEventListener('click',function(){
						alert(222);
					});

			兼容代码
				function addEvent(obj,type,callback){
					if(obj.addEventListener){
						//存在addEventListener属性，是w3c浏览器
						obj.addEventListener(type,callback);
					}else{
						obj.attachEvent('on'+type,callback);
					}
				}

		4.2 事件监听删除
			IE模型
				obj.detachEvent(type,callback)
				参数说明：
					obj：元素对象
					type：字符串，事件名
					callback：回调函数名
			
			W3C模型
				obj.removeEventListener(type,callback)

			example：移除监听事件
				function removeEvent(obj,type,callback){
					if(obj.removeEventListener){
						//存在removeEventListener属性，是w3c浏览器
						obj.removeEventListener(type,callback);
					}else{
						obj.detachEvent('on'+type,callback);
					}
				}

	5. 事件的监听模型
		w3c标准下，事件的监听模型有俩种，冒泡模型和捕捉模型。
		注：目前IE浏览器只支持冒泡模型。

		5.1 冒泡模型和捕捉模型
			多个元素是以父子的嵌套形式组成，当子元素触发某个事件，会连锁的触发外层所有父元素的事件。
			冒泡方式，是从内到外触发。
			捕捉方式，是从外到内触发。

			addEventListener：第三个参数true代表捕捉模型，false为冒泡模型。
			example ：
				<style type='text/css'>
					#div1{
						width:400px;
						height:400px;
						background:yellow;
					}
					#div2{
						width:300px;
						height:300px;
						background:blue;
					}
					#div3{
						width:200px;
						height:200px;
						background:red;
					}
				</style>

				<script language="JavaScript">
					window.onload=function(){
						$('div1').addEventListener('click',function(event){
							//event.stopPropagation();
							alert('div1');
						},true);
						$('div2').addEventListener('click',function(event){
							//event.stopPropagation();
							alert('div2');
						},true);
						$('div3').addEventListener('click',function(event){
							alert('div3');
						},true);
						/*
							捕捉模型，点击div3时，其他俩个div也会被触发事件，是从外到内触发的。

							冒泡模型为false，触发顺序相反。
						*/
					}
				</script>

				<div id='div1'>
					<div id='div2'>
						<div id="div3"></div>
					</div>
				</div>

		5.2 事件停止传播
			在捕捉或冒泡的过程中，如果不像让事件继续传播下去，可以中途让事件结束传播。
			通过事件对象来阻止。

			a. IE模型取消方式
				window.event.cancelBubble=true;
			
			b. w3c模型取消方式
				event.stopPropagation()

			example :
				window.onload=function(){
					addEvent($('div1'),'click',function(){
						alert('div1');
					});
					addEvent($('div2'),'click',function(event){
						alert('div2');
						//event.stopPropagation();
						stopBubble();
					});
					addEvent($('div3'),'click',function(event){
						alert('div3');
						//event.stopPropagation();
						stopBubble();
					});
				}
		
		5.3 事件的执行顺序
			w3c标准下：
				同一个元素的多个监听程序，先定义的先触发，后定义的后触发
			
			ie标准下：
				同一个元素的多个监听程序，后定义的先触发，先定义的后触发，采用栈的形式。
			
			note：
				层级关系嵌套的多个元素 且这些元素都绑定了俩种模型的事件处理程序。
				
				当点击最内层的元素时，会先触发捕捉模型的程序，由外到内，直到触发到最内层的元素，会按照绑定程序的先后顺序来执行，
				
				然后再触发冒泡模型的事件处理程序。

	6. 元素的效果阻止
		在某些情况，我们需要对元素的默认效果进行阻止，如阻止表单的提交，a标准的跳转。
		这种情况下，也是通过对元素的事件对象来阻止。
		
		a. IE模型
			window.event.returnValue=false;
		b. w3c模型
			event.preventDefault();
		c. 代码兼容
			//取消自动跳转行为
			function prevent(event){
				if(window.event){
					//IE模型中的
					window.event.returnValue=false;
				}else{
					//w3c模型的
					event.preventDefault();
				}
			}
		
		PS：IE模型对于事件的处理都封装到window.event对象里面了。
			而w3c模型则封装到一个变量中，通过函数显式的传递给予别人调用。
		

三、js面向对象编程
	我们之前学习的，对dom对象的操作，这些并不是属于js这门语言的。而是浏览器引擎封装好，附加给js的。
	example：
		document.xxx
	这些方法的使用，都是dom对象的。
	
	1. 作用域
		example 1：
			var c=5;
			function t1(){
				var d=6;
				function t2(){
					var e=7;
					alert(c+d+e);
				}
				t2();
			}
			t1();	//
		
		在js中，函数的嵌套是非常普遍的，那么变量是如何寻找的呢？
		首先在函数内部找，再去函数外层找，直到最外层的全局(window)区域，一直寻找直到找到这个变量。如果没有找到的话，同时该变量有赋值行为，那么将这个变量赋予window对象，如果该变量只是声明，那么该变量是undefined。
		
		这种内内层往外层寻找变量的过程，叫做js的作用域链。

		在js中，变量在哪个函数中声明，该变量就属于这个函数的。
		内部可以访问外部的变量，而外部则不能访问内部变量，但是可以通过闭包来实现访问内部变量。
		
		example 2：
			var i=5;
			function fn1(){
				if(true){
					var i=3;
				}
				alert(i);	//弹出3
			}

		note：在js语言中，是无块级作用域这种说法。在解析一个变量的时候，会在函数内部所有代码中寻找声明这个变量的代码。

	2. var关键字
		var关键字是用于声明变量，注意是声明，并未赋值，=才会产生赋值行为。
		example1：
			var name;	//只是声明变量

		example2：
			alert(window.d);	//undefined
			alert(window.e);	//undefined
			function t(){
				d=5;
				var e=6;
			}
			
			//d没有var来进行声明，这是一个赋值行为，因此js会寻找该变量在哪个作用域中声明的。
			//t函数域内的变量未声明，会去外层window域中寻找，window也没有声明该变量，所以js就确定该变量属于window的。
			t();

			alert(window.d);	//5
			alert(window.e);	//undefined
		
		example3：
				var local=100;
				function fn1(){
					alert(local);		//undefined
					var local=1000;
					alert(local);		//1000，在函数内部寻找到被var声明的local变量，所以直接输出1000
				}
				fn1();
				alert(local);	//100

		
		输出undefined的原因：
			js的代码是自上而下执行，但是在执行之前，会有个词法分析，即声明变量和函数。
			所以执行到这行代码的时候，会找到fn1声明的变量local，输出undefined，代码继续往下执行，fn1的变量local才会被赋值。

	3. 词法分析
		js代码执行步骤：
			a. 编译(词法分析：声明变量、函数)
			b. 运行期(对变量赋值、调用函数)
			形参、变量和函数的声明，都是在词法分析进行的，赋值则在运行期间。

		js词法分析分为3步：
		a. 先分析参数
		b. 再分析变量声明
		c. 分析函数声明
		
		一个函数能够使用的局部变量，就从上面的3步分析而来。

		具体分析说明：
			a. 在函数运行前的一瞬间，会生成Active Object 活动对象。
			b. 把函数声明的参数，形成AO的属性，值全部是undefined，然后再接受实参，形成AO响应属性的值。
			c. 分析变量声明，如var age，
				如果AO上没有age属性，那么添加age属性，值是undefined，
				如果已经有age属性了，则不做任何影响。
			d. 分析函数声明，如function foo(){}，则把函数赋值给AO.foo属性。
			注：如果此前foo属性已经存在，会被覆盖掉。

		注：传值是属于运行期的过程，非词法分析的过程。

		example1：
			function t(age){
				alert(age);
			}
			t(5);	//
			词法分析：
				AO{age:undefined}，接受实参AO{age:5}
			运行过程：
				t(5) => alert(AO.age);
		
		example2：
			function t(age){
				var age=99;
				alert(age);
			}
			t(5);	//99
			词法分析：
				a. 形成AO对象
				b. 分析形参：将age作为AO属性=> AO={age:undefined}，接受实参：AO={age：5}
				c. 分析变量var age，AO对象已有AO属性，不做任何影响
			运行过程：
				AO.age = 99
				alert(AO.age);	//99

		example3：
			function t3(){
				var greet='hello';
				alert(greet);
				function greet(){}
				alert(greet);
			}
			t3();

			词法分析：
				a. 形成AO对象
				b. 分析形参：无形参
				c. 分析变量：AO={greet：undefined}
				d. 分析函数：AO={greet：function greet(){}}
				函数的声明会对AO对象的同名属性进行覆盖，这时候greet等于function
			运行分析：
				有三个语句需要运行
				AO.greet='hello'
				alert(AO.greet);	//hello
				alert(AO.greet);	//hello
			注：因为运行期间值被hello覆盖了，所以输出hello
		
		example4：
			function a(b){
				alert(b);
				function b(){
					alert(b);
				}
				b();
			}
			a(1);
			词法分析：
				a. 分析形参：AO={b:undefined}，然后接受参数：AO={b:1}
				b. 分析变量声明，a函数没有声明变量
				c. 分析函数声明，AO={b:function b(){alert(b);}}
			运行分析：
				alert(b);	//function
				b();		//b函数内没有声明变量，也无形参，所以会向外层寻找，即function。
		
		example5:
			function a(b){
				alert(b);
				b=function(){
					alert(b);
				}
				b();
			}
			a(1);
			词法分析：
				a. 分析形参：AO={b:undefined}，然后接受参数：AO={b:1}
				b. 分析变量声明，a函数没有声明变量
				c. 分析函数声明，a函数中没声明函数
			运行分析：
				alert(b);	//1
				b=function...	//将函数赋值给b
				b();		//b函数内没有声明变量，也无形参，所以会向外层寻找，在函数外找到b，即b=function，所以b是函数
		
		js被称为披着C外衣的Lisp语言，lisp是一种强大的函数式语言，函数的地位比较高，函数可以赋值给变量，可以作为参数来传递。
		example6：
			function t1(){}
			var t2=function(){}
			t1是一个函数的声明过程，虽然全局内会得到一个t1变量，值是function
			t2是一个赋值的过程，值是等号右边的表达式的返回结果，即函数。
		example7：
			(function(){})	//内层表达式，返回值是函数，由于函数是包在括号里面的，被当成表达式来执行
			(function{})()	//将返回的函数执行

			(function(window,undefined){})(window);
			function(window,undefined)：
				这里的window,undefined都是形参
			(window)
				将window对象传递进行
			
			函数的形参、变量都是绑定在AO对象上的，
			形参和变量同名，如果变量没有赋值，是不会发生覆盖的，只是作为一个AO对象的一个属性，如：AO.xxx。

			上面代码的好处：
			1. 使用匿名函数，不会污染window全局变量。
			2. 将window对象作为参数传递进去，是为了加快代码执行速度，这是因为jquery代码函数嵌套很多，window对象需要通过n层函数才能找到，所以直接把window以参数形式传递进来，这样window就在jquery内部的AO对象上了，
			3. undefined作为形参，但是又不传递实参是为了安全，因为在IE低版本中，undefined竟然可以重新赋值，如undefined=3，所以声明undefined局部变量(名字是undefined)，同时又不传参，值自然是undefined，这是为了防止外界对undefined值的污染。
		
		
		作用域链是指什么，指函数由内到外所产生的AO链，叫做作用域链，每次函数运行，都会产生AO对象。

		重要、重要、重要：词法分析的时候，是从外往内部分析AO链，执行的时候是从内往外找AO链。

		其他：面向对象、面向函数、面向集合(mysql,oracled..)的语言。
	
	4. arguments详解
		arguments是1个对象，是一个很像数组的对象。
		arguments的内容是函数运行时的实参列表。
		(
			function(a,b,c){
				console.log(arguments);		//可以看到有length属性，所以不是数组
				console.log(arguments[0]);	//参数的访问方式

				arguments[0]='china';
				console.log(a);	//china，形参与对应的arguments索引是相互映射的
			}
		)('hello','world','fuck','other')
		php中，只能通过形参给函数传递实参，而js中可以额外的传递实参，会把所有实参收集起来赋予arguments对象，即便该实参没有对应的形参。

		注：每个函数都会有arguments对象，因为arguments只会在内部寻找自身的参数，无法引用其他函数的arguments。

		注：在函数中，要访问函数内部的变量，有三种方式，AO对象、this对象、arguments对象。
			可以通过这三个对象来访问变量。
		
		如何获取函数运行时，真正收到的实参个数？
			通过arguments.length来获取函数运行时的实参个数，收到的实参个数，在多态中会用到。
		
		callee属性，代表当前运行的函数，这个属性的意义。
		example：让你运行一个函数，递归计算1到N的和，要求不使用函数名。

	5. this
		this指向的是对象，若是直接调用函数，则this指向window，会污染全局变量的。
		准确的说，this的值为null，但是被解释成window，而在ECMASCRIPT中，若this为null，则被解释成undefined。

		一般的程序语言中，this是指向你所调用的对象的本身。
		js中this的4中调用方式
		1. 作为普通的函数调用
			alert(window.x);	//undefined
			function t(){
				this.x=333;
			}
			t();
			alert(window.x);
			this的值指向window，准确的说，this为null，但是被解释成window，而在ECMASCRIPT5标准中，如果this为null，则解释成undefined。

		2. 作为对象的方法来调用
			var obj={x:888,y:45,t:function(){alert(this.x);}};
			obj.t();	//输出888

			var dog={x:'is dog'};
			dog.t=obj.t;
			dog.t();	//输出is dog，this是指向dog对象的。

			这时候this指向对象的调用者，即该对象。

		------------
		一个函数有this，注意，这个this是指向null的，你要明白函数是一个引用类型，将函数赋值给对象的一个属性，其实就是将函数的内存地址赋值给对象的属性。
		比如obj.x=function(){this}，这里的this会指向obj对象了。

		所以this作为对象的方法调用时，this指向其调用者。

		3. 函数作为构造函数调用时
			js中没有类的概念，创建对象是用构造函数来完成，或者直接用json{]来写对象。
			function Dog(_name,_nage){
				this.name=_name;
				this.age=_age;
				this.bark=function(){
					alert('i am '+this.name);
				}
			}

			var dog=new Dog('huzi',2);
			dog.bark();	//执行这个函数，该函数的this执行dog对象。

			这个过程发生了什么？
			1. 系统创建空对象{}。
			2. 把空对象construct属性指向dog函数，
			3. 把函数的this指向该空对象。
			4. 执行该函数。

		4. 函数通过call、apply调用
			语法格式：函数.call(对象,参数1,参数2，参数n)
			
			function t(num){
				alert('我的真是年龄是：':this.age);
				alert('但我一般告诉别人年龄是：'+this.age+num);
			}
			var human={name:'lisi ',age:28};
			human.t=t;
			human.t();	//this虽然指向了human对象，但是多了一个t属性。

			//临时让this执行human对象。
			var wangu={name:'wangwu',age:30}
			t.call(wangwu,5);		//通过这种方式，能临时执行对象。

			//t(-10);	//this 是指向window的，this本来是null

	6. 闭包
		example1：
			function t1(){
				var age=20;
				function t2(){
					alert(age);
				}
				return t2;
			}
			var tmp=t1();
			var age=99;
			tmp();	//20

			在大部分的语言中，t1被调用执行，则申请内存并把其局部变量push入栈，
			t1()函数执行完毕后，内部的局部变量，会随着函数的退出而销毁，因此age=20的局部变量就消失了。

			而在js中，t1执行过程中又生成了t2，而从作用域上来说，t2能访问到20，于是age=20并没有消失，而是与返回的t2函数形成了一个"环境包"，这个包是属于t2的，别人是访问不了的，所以叫做闭包。

			返回的t2是有自己的生态环境的，这种情况返回的函数，并非孤立的函数，甚至把其周围的变量环境，形成了一个封闭的环境包，所以叫做闭包。

			概括：函数的作用域取决于声明时，而不取决于调用时。

		example2：闭包计数器
			1. 建立一个全局变量
				window.cnt=0;
				调用++window.cnt
				这种方式会污染全局变量，其次若是引入了多人的js程序，别人的程序也有该变量，那么该计数器就被污染了。
			
			2. 用闭包来实现。
				function counter(){
					var count=0;
					return function(){
						return ++count;
					}
				}
				var inc=counter();

				简化
				var inc=function(){
					var cnt=0;
					return function(){
						return ++cnt;
					}
				}();

				在工作中，一般如何避免全局变量污染？
				1. 统一放在一个全局对象上，如juqery是放在$变量上的。
				2. 每个人用自己的命名空间。

				//个人命名空间，其实是把自己的变量、函数都放在一个对象中，
	
	7. 对象
		在js中，有对象没有类，但是有构造方法。
		
		在很多语言中，类=>对象，由类生成对象。
		而在js中，对象只是一个"属性字典"，就想PHP中的关联数组，因此我们可以直接实例对象，而不需要类。
		
		var person={name:'yuxuan',age:25};	//这种格式生成的对象，叫做json格式的对象。
		json对象的属性是可以增加和删除的。
		增加：person.say=function(){alert('i ca say');};
		删除：delete person.say;
		
	8. js的构造函数
		js是由构造函数来实例对象的。
		example：
			function Dog(){
				this.leg=4;
				this.brak=function(){
					alert('汪汪');
				}
			}
			var huzi=new Dog();	//这便实例出一个对象了。
			huzi.brak();
			
	9. 封装
		通过闭包，来实现对象的封装。
		function Girl(_name,_bf){
			this.name=_name;	//实例对象的时候，就为对象赋值

			//私有属性的模拟，该变量属于函数AO对象的，所以访问不到
			var secret=bf;

			this.showlove=function(){//提供一个访问接口，以闭包的形式来访问该属性
				return secret;
			}
			this.movelove=function(_secret){
				secret=_secret;
			}
		}
		var obj=new Girl('xiaohong','xiaogang');

	10. 继承
		js的继承是通过原型的概念来实现的。

		function Cat(){
			this.climb=function(){
				alert('我会爬树');
			}
		}
		function Tiger(){
			this.bark=function(){
				alert('我是百兽之王');
			}
		}
		var mao=new Cart();
		Tiger.prototype=mao;

		var huanan=new Tiger();
		huanan.climb();

		对象.__proto__ = 构造函数.prototype	

		Object原型对象：Object.prototype
			初始属性
			constructor：Object()
			__proto__：null

		Cat原型对象：Cat.prototype = new Cat().__proto__
			constructor：Cat()
			__proto__：Object原型对象

		cat实例对象：new Cat()
			对象属性
			__proto__：Cat原型对象
		
		如上所示，寻找car对象的某个属性时，会先在car对象中寻找，如果找不到，则在Cat原型对象寻找，Cat原型对象找不到会继续找Cat原型对象的Object原型对象，如果Object原型对象也没有的话，则停止寻找，这个就是js的原型链了。
		
		var cat = new Cat();
		说明：用Cat构造函数实例的对象。

		cat.__proto__ = Cat.prototype
		说明：cat对象的__proto__属性指向了Cat.prototype，也就是Cat的原型对象。该原型对象是一个只有constructor、__proto__属性的空对象。constructor是Cat构造函数，__proto__是Object的原型对象。

		Cat.prototype.__proto__ = Object.prototype
		说明：
			Cat原型对象的原型对象是Object构造函数的原型对象，不同的是Object原型对象是有属性的，有一些列Object构造函数声明的属性和方法，而__proto__则是null，表示原型链最顶级了。

		对象的属性和方法，就是沿着原型链来查找和调用的。
		而函数的属性和方法，是通过作用域链来查找的。

		注：继承的时候，你可以覆盖原型对象的属性，来达到重写父对象属性和方法

		10.1 原型冒充
			function Cat(_leg,_tail){
				this.leg=_leg;
				this.tail=_tail;
				this.climb=function(){
					alert('我会爬树');
				}
			}
			function Tiger(_leg,_tail,color){
				//把要继承的构造函数的语句执行一遍
				this.parent=Cat;
				//this.parent();
				this.parent.apply(this,arguments);

				delete this.parent;

				this.color=color;
			}
			var tiger=new Tiger();
		10.2 复制继承
			把父对象的所有属性赋值到自身对象上。
			function Cat(_leg,_tail){
				this.leg=_leg;
				this.tail=_tail;
				this.climb=function(){
					alert('我会爬树');
				}
			}
			function Tiger(_leg,_tail,color){
				this.color=color;
				this.extend=function(parent){
					for(var key in parent){
						this[key]=parent[key];
					}
				}
			}
			var tiger=new Tiger('yellow');
			tiger.extend(new Cat(4,1));

	11. 静态方法
		1. 构造函数通过new来创建对象
		2. 函数也是对象。
		你可以把方法和属性附加给构造函数，但是注意，该构造函数实例出的对象是无法访问函数的方法和属性的。
		function Person(age){
			this.age=age;
		}
		Person.ajax=function(){}	//该属性是Person函数的，跟实例的对象无关。
		
		还有例如Math.round();
		这是通过函数去访问属性和方法，不就是静态方法么。