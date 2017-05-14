<!--
一. html结构
	1. doctype
	2. head
		2.1 title
		2.2 meta charset
	3. body
二. div布局
	1. 布局原则
		从上到下、从左到右、从大到小。

	2. 盒子模型
		2.1 盒子模型的属性
			盒子模型拥有内容content、填充padding、边框border、边界margin4个属性。

			盒子的边框是border，里面是内容content，
			盒子与盒子之间的距离是margin，
			盒子与内容的距离是padding。
			
			盒子与盒子之间的距离，优先用margin，盒子与文字的距离，优先使用padding。
			PS：在html中，内容也是被当作元素来对待的，margin也会对内容其作用的。

		2.2 盒子的空间计算
			盒子高度=border-top+border-bottom+height+padding
			盒子宽度=border-left+border-right+height+padding
			example
			div{
				width:300px;
				height:300px;
				background:gray;
				border:50px solid blue;
				padding:50px;
			}
		2.3 padding
			padding，即填充，对盒子内容填充时，内容所使用的面积大小不变，仍为盒子width*height，但因为盒子被填充，所以盒子的面积会变大。
			盒子面积=填充面积+width*height

		2.4 margin的重叠现象
			所有相邻的俩个或更多盒元素的margin将会合并为一个margin共享之。
			这里相邻定义：同级或者嵌套的盒元素，并且他们之间没有非空内容、padding或border分隔。

			相邻的普通元素，上下边距，并非简单的相加，而是取其中较大的边距值。这种现象就叫做margin重叠。
			注：
				这里是普通元素才会发生这种现象，例如浮动元素就不是普通元素。同时相邻的俩个普通元素，可以是父子关系，也可以是平级关系。

			example1：
				#div1{
					height:100px;
					background:gray;
					margin-bottom:50px;
				}
				#div2{
					height:100px;
					background:pink;
					margin-top:40px;
				}
				<div id='div1'></div>
				<div id='div2'></div>
			example2：
				#father{
					width:1002px;
					height:100px;
					background:gray;
					margin-top:50px;
				}
				#parent{
					width:802px;
					height:80px;
					background:pink;
					margin-top:100px;
				}
				<div id='father'>
					<div id='parent'></div>
				</div>
			解决办法
				用父元素padding-top 代替子元素margin-top

	3. 流的概念
		流在网页中描述元素的排列方式。
		分为标准流、非标准流。
		标准流：元素像流水一样，是顺序出现的。
		非标准流：指某个元素脱离了标准流排列，比如相对定位，float的元素。

	4. 浮动布局
		4.1 float概念
			浮动有俩种：有左浮动、右浮动。
			example：
				float：left、right;

			一个元素浮动后就脱离了文档流，它会让出原先的位置进行漂浮，直到浮动元素的边框碰到父元素的边框。
			这是浮动元素相当于悬浮在其他元素上方，后方的非浮动元素会移动上来，浮动的元素会覆盖其他元素。
						
			注1：如果使用了浮动属性，任何元素都会被当作块元素来对待。
			注2：若前方的块元素浮动，字符内容是不会移动上去的，字符内容仍然会被挤开，哪怕该字符内容被其他块元素包裹。

		4.2 浮动清除
			清除浮动的原因
				元素浮动后会让出原来的位置进行漂浮，使其后方的非浮动元素移动上来。
				但是有时布局需要让其他元素能够识别浮动元素，或让元素排列在浮动元素下方，这时候就需要清除浮动。
			a. clear
				clear属性可以用于清除浮动
				example：
					clear：left、right、both。
			还有其他方法待研究。

	5. 块元素
		可以容纳文本、行内元素和其他块元素，块元素不管内容的多少，都会占据整行且换行显示。
		常见块元素如div、ul、li等。
		块元素特点
			a. 标准流的情况下，块元素不管内容的多少，都会占据整行。
			
			b. 标准、非标准流下，块元素的高度会根据内容的高度而自适应(未设定高度)，但是块元素无法对浮动内容自适应，同时若内容超出块元素，则内容会溢出。
			example：
				<div style="background:gray;">
					可以看到，未设置高度的div会根据内容的高度进行自适应
				</div>
			
			c. 浮动的块元素，不会占据整行内容，会根据内容大小而自适应宽高。如果没有内容且无指定宽和高，则看不到该块元素。
			example：
				<div style="background:gray;float:left;">
					浮动的div，会根据内容自适应宽高
				</div>

			PS1：容纳的文字是中文会自动换行，字母不会换行，会溢出，可以使用word-wrap:break-word;来实现强制换行。
			PS2：一个div是一个块，你可以认为是一张纸，内部有一个基线的概念，让你来显示内容，基于基线，文字是显示在垂直中间的。

	6. 内联元素
		行内元素用于容纳文本或其他行内元素，常见行内元素 span.
		
		特点：
			1、和其他元素都在一行上；
			2、高度、行高和顶以及底边距都不可改变；
			3、宽度就是它的文字或图片的宽度，不可改变。
		
		总的来说，内联元素一般都是基于语义级(semantic)的基本元素，它只能容纳文本或者其他内联元素，通常被包括在块元素中使用，常见内联元素有“a、b、br”等。

	7. 块元素和内联元素的转换
		display：block、inline。
			属性说明：
				display定义了元素是作为行内还是块元素来进行使用。
			参数说明：
				block：作为块元素使用。
				inline：当作行内元素使用。

三、css
	1. 选择器
		1.1 id选择器
			语法：
			#id选择器名称{
				...
			}
		1.2 类选择器
			语法：
			.class选择器名称{
				...
			}
			
			example ：
			.demo{
				clolor : read ;
			}
			<div class=”demo”></div>
		1.3 标签选择器
			html元素{
					…
				}
		1.4 派生选择器
			选择器层级关系的一种定义，可以对选择器下面的子标签、子选择器进行样式的定义。
			#demo span {}
				表示对demo选择器下面的span做一个样式定义。			
			#demo span span {}
			#demo #son {}
				有严密的层级关系
			#demo span span a{}
				可以有多级，开发中建议不要太多级，增加了复杂度。
		1.5 伪类选择器
		1.6 通配选择器
			*{
				…
			}
		1.7 多个选择器的公共部分，可以提取出来进行定义。
			example：
			.demo1,.demo2,.demo3{height:50px;}
			.demo1{width:10px;}
			.demo2{width:20px;}
			.demo3{width:30px;}
		1.8 选择器的优先级
			在元素拥有多个选择器下，若样式冲突，那么根据选择器的优先级来进行覆盖定义。

			级别：id选择器>class选择器>html选择器>通配符选择器。

			所以在网页设计中，我们默认使用class选择器。
			因为class选择器的样式优先级大于html、通配符选择器，所以样式可以进行覆盖，而当我们需要对class选择器进行样式覆盖的时候，就可以使用id选择器。

			PS：元素可以被多个class选择器修饰，但是只能被一个id选择器修饰。

		1.9 选择器冲突情况
			a. 不同的选择器以优先级来选择。
			b. 相同名称的选择器，以选择器的先后顺序为准，相同属性的样式前面的会被后面的覆盖，不同的属性则会进行组合定义。

	2. css效果
		2.0 border属性
			语法：
			border：[ border-width ] || [ border-style ] || [ border-color ]
			[ border-width ]： 
				设置或检索对象边框宽度。 
				<border-width> = <length> | thin | medium | thick
				<length>： 用长度值来定义边框的厚度。不允许负值 
				medium： 定义默认厚度的边框。 
				thin： 定义比默认厚度细的边框。 
				thick： 定义比默认厚度粗的边框。 

			[ border-style ]： 设置或检索对象边框样式。 
			[ border-color ]： 设置或检索对象边框颜色。 

		2.1 段落控制
			text-indent：首行缩进
			test-align：水平文字方向
			text-decoration：文字装饰线
			letter-spacing：字符间距
			text-transform：字母大小写转换
			line-height：

		2.2 font
			用于文字控制。

			color：颜色
			font-style：字体样式
				normal： 指定文本字体样式为正常的字体 
				italic： 指定文本字体样式为斜体。对于没有斜体变量的特殊字体，将应用oblique 
				oblique： 指定文本字体样式为倾斜的字体 
			font-variant	
				normal： 正常的字体 
				small-caps： 小型的大写字母字体 
			font-weight：文字粗细
				normal： 正常的字体。相当于number为400 
				bold： 粗体。相当于number为700。 
				bolder： 特粗体。也相当于strong和b对象的作用 
				lighter： 细体 
				<integer>： 用数字表示文本字体粗细。取值范围：100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900 
			font-size大小/line-height：行高

			font-family：字体

			font：style | weight | size/line-height | family
			
			
			字体是分为俩大类。
				衬线：用于正文，有棱有角，serif
				无衬线字体：用于标题，sans-serif
				
				问题：你设置的字体客户端未必有，没有的时候，客户端使用默认字体。
				example：
				font-family:'weiruan','yahei',sans-serif(让客户端选择一个无衬线字体)

		2.3 background
			背景控制
			color
			image
			repeat：
				repeat-x(水平方向平铺)
				repest-y
				repeat，横向、纵向都平铺
				no-repeat，不平铺
			attachment：fixed，使图片固定或者不固定
			pisition：值可以是px，也可以是方向，如left、right
			
			背景图片默认和div左上角对其显示，可以通过x轴和y轴来精确显示图片，也可以通过方向来控制图片。
			
			简写：background：color|url|repeat|
			你不写跳过也可以。
			如果背景图片和背景颜色都设置了，那么显示背景图片。

			PS：原点说明，在计算机中，左上角为原点，以左上角右边的为整数，左上角下边的为y轴的整数

		2.4 定位，position
			2.4.1 静态定位static
				即盒子模式，正常状态下的定位模式。
				
			2.4.2 相对定位relative
				相对该元素所在位置的定位（定位坐标位于自身左上角）
				依据 left ， right ， top ， bottom 等属性在正常文档流中偏移位置

				定位的时候，元素会脱离它的位置，但该元素所占据的空间不变。
			
			2.4.3 绝对定位absolute
				指相对于父元素来定位。
				用绝对定位时，父元素要有position属性，若父元素没有，会向上查找拥有position属性的父元素，没有的话，相对于body定位。
				
				PS1：使用绝对定位的元素脱离了标准流。
				PS2：z-index，用于确定绝对属性的元素的层级关系。

				官方说明：
					将对象从文档流中拖出，使用 left ， right ， top ， bottom 等属性相对于其最接近的一个最有定位设置的父对象进行绝对定位。
					如果不存在这样的父对象，则依据 body 对象。而其层叠通过 z-index 属性定义
			2.4.4 fixed
				总是以视窗的左上角进行定位（即body）。

		2.5 overflow溢出处理
			如果父元素是固定大小，若父元素里的内容特别多，那么内容会超出父元素的范围，这就是内容溢出。
			overflow，用于处理溢出内容
			overflow：scrool
			hidden，在ie中，可以用于处理div元素1px 1px不正常显示问题。

	3. css初始化
		相同的元素，在不同的浏览器下，显示的效果略有不同。
		这是浏览器对各元素的margin、border、font-size解析略有不同。
		为了解决这种问题，我们通过css强制让所有元素的属性值一致。

	4. css导入问题
		可以在css文件中，把其他css文件导入。
		@import url() 

四、html标签
	1. 无语义标签
		如div和span。
	
	2. 有语义标签
		p标签：文本段落标签，用于显示段落。
		
		img标签
			src：引入图片的路径
			alt：用于说明图片，在图片丢失的时候，说明是什么图片
			title：鼠标放上去就显示的文字。
			
			单闭合标签和双闭合标签，img是单闭合标签。			
			img是一个特殊的内联元素，img标签不知道内容是什么，
			所以是根据引入的内容来替换这个标签的
			
			img标签是内联元素，同时是替换元素，替换是指真正要显示的来自于图片，替换元素是能设置宽和高。

			img有一个默认的margin值，由于是内联元素，所以你可以专程块元素，来来清楚margin值。img也有border值。

			css初始化会清0

		ul、ol
			ul：无序列表，list-style-type，设置列表样式类型。
			ol：有序列表，list-style-type，设置列表样式类型。

			同样css会进行初始化

		table
			table用于显示格式化表格的数据是非常方便的。
			border-collapse：
				设置或检索表格的行和单元格的边是否合并在一起，还是按照标准的html样式分开。
				separate：边框独立
				collapse：相邻边被合并。
			
			border-spacing
				表格的行和单元格的边距。
				注：只有当border-collapse值为separate才生效。
						
			rowspan：设置单元格竖跨几步。
			colspan：设置单元格横跨几步。

			border：对table有用，对tr无效，可以设置单元格td。

			说明1：
				vertical-align：控制td内容的垂直居中，td单元格内容默认是垂直居中的。
			说明2：单元格宽高与自适应问题
				table无设置宽度、高度下，单元格是与内容自适应的。
				如果table有高度，行高等于table行的数量的平均值，如果有宽度，宽度等于单元格数量的平均值。
			说明3：
				对td使用padding，会扩充td单元格的大小并影响到整行元素，但是table的整体宽度和高度是不变的。

			example：
				table{
					width:300px;
					height:100px;
					
					border-collapse:separate;
					border-spacing:1px;
					border:1px solid red;	//这是表格的边框
				}
				td{
					border:1px solid red;	//这是单元格的边框
				}
				/*
					这段样式，你会发现单元格与表格的边框是有一定距离的，border-collapse用于设置单元格与行的边是否合并。
					border-spacing用于设置单元格与行的边在纵向和横向上的边距，例如table的border-collapse为separate的时候。
				*/
			
			input标签是一个块元素标签，会占据整行元素的，所以在单元格中，会把整个单元格沾满。

		超链接
			a标签
				href：要请求的网址。
				target：显示网址打开的方式，
				title：鼠标放上去显示的内容。
			
			PS：a标签是一个标准的内联元素。

		描点
			标记在文档哪里显示
			在需要标记的地方加入描点，通过a标签的name属性，name表示描点名。
			访问的时候通过#描点名来进行访问。
			<p>p1</p>
			<p>p2</p>
			<p>p3</p>

		伪类
			一个链接有4个状态：普通状态、访问过的状态、鼠标悬浮在上面的状态鼠标点击的状态、
			css允许我们针对a标签的4中状态设置各自的css属性，这个叫做伪类。
			a：link，鼠标普通状态
			
			a：visited，访问后过后的颜色
			a：hover，鼠标悬浮在上面的样式
			a：active，鼠标点击时候显示的样式，

			注意：active一般不用去设置样式
				一定要注意，他们是有顺序的，LVHA顺序
				a：link可以简写为a。
		
		字符实体
			html开发中，有一些字符，不适合直接写出，如大于小于号，容易引起浏览器解析错误。
			常用实体符号：&

			/*
			ul内的li元素浮动起来了，所有ul没办法判断内容的大小
			ul也是块元素，所以也是占据整行空间。
			li也是块元素，会占据整行空间。

			块元素在浮动的时候，就不会占据整行空间了，块元素会根据内容的宽高来自适应大小。
			同时内容在块元素中，如果块元素设置了宽、高，那么中文会自动换行，字母不会换行，超出部分直接溢出元素。

			div中的浮动元素浮动在左边，div没有设置宽度

			后续跟块状标签，div会浮动上来，但是div中的内容会被浮动元素的内容影响到的。
			框架集
		
		frameset：框架集
			框架集需要使用对应的doctype。
			html文档不需要有body，直接来写。
			<frameset rows="300px,*">
				<frame src=""/>对于300px的页面
				<frameset cols='200px,*'>
					<frame src="left.html"/>
					<frame src="right.html" name='right'/>
				</frameset>
			</frameset>

			rows：行如何进行分割
			cols：列该如何进行分割
			单位可以使用像素px，百分比%，百分比是对于整个页面。

			<a href="" target="right"></a>
			点击一个连接，会出现一个页面，会在right元素中显示。

五、浏览器的兼容问题
	1. 没有使用正确的doctype
	2. 各浏览器对不同标签的初始值不同：利用css初始化来解决问题
	3. 自身书写不规范引起
	4. 浏览器bug引起。
	5. ie常见bug
		5.1 盒模型bug
			解决方案：
				使用严格doctype声明，不同浏览器对div宽度和高度设置不一致。
		5.2 双倍margin bug，解决：_display:inline;
			ie6浏览器
			example：
			div{
				width:100px;
				height:100px
				border:1px solid;
				float:left;
				margin-left:50px;
			}
			<div></div>
			上述例子就是ie的双倍margin bug。
			对于左浮动元素，它的左外边距是定义的2倍。
			右margin是定义的俩倍。
			解决：_display：inline，只有ie能解析这个标签。


		5.3 不认识a：link
			解决：不加link

		5.4 三像素margin bug，
			解决：规范浮动与清除浮动。
			example：
			div{
				width:100px;
				height:100px
				border:1px solid;
				float:left;
				margin-left:50px;
			}
			#test1{
				float:left;
			}
			#test2{
			}
			<div id='test1'></div>
			<div id='test2'></div>
			这是开发人员代码不规范，标准浏览器中，test2 div会被div覆盖。
			但是在ie中，test2 div会一起浮动上来，跟在后面，有3个像素的距离。


	颜色的表示
		1.用数字来表示
		2.颜色有3元素组成的，red、green、blue，rgb
		3.3元素各在0-255之间变化。
		综上：本质是符合3原色
		1. 用16进制组合0-255==》【00，ff】
		2.用10进制来符合rgb（24，34，34）
		3.根据html提供的常用颜色名称来组合。


	尺寸的表示
		1.像素表示，px
		2. 百分比表示，根据父元素进行尺寸对比。
			块元素宽度默认占据父元素的100%
		3. em表示，相对大小，是指其父元素中的1个M的大小，简单理解为父元素的font-size就是1个em单位
	css画圆角
		在老式浏览器中是不支持的，目前主流浏览器都支持。
		通过border-redius来设置。
		border-redius：5px 5个像素直径的圆角。


做的练习太少，所以生疏。

-->

