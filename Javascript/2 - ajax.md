1. 远古时代的ajax
	ajax的本质，便是不刷新浏览器，在后台发送一个请求过去。
	方法1：
	​	HTTP 204(no content)表示响应执行成功，但没有数据返回，浏览器不用刷新，不用导向新页面。
	​	
		利用HTTP协议204状态码的特点，可以不通过XMLHttpRequest对象来实现ajax。
	
		example：
			header("HTTP/1.1 204 Not Content");
		
		HTTP 205(reset content) 表示响应执行成功，重置页面（Form表单），方便用户下次输入。
	
	方法2：img标签
	​	我们发现，使用img标签其实也是会去服务器端进行请求，所以我们可以通过这个标签来发送请求。

		example
			var hh=document.createElement('img');
			//只要设置了src，浏览器就会去请求这个资源
			hh.setAttribute('src','./hh.jpg');
	
			//document.getElementById('pro').appendChild(hh);
	
	方法3：利用style、srcipt标签加载的特性，完成请求，原理与img标签加载一样。

	方法4：利用iframe特性
	​	ajax注册，提交表单，页面无刷新。
	​	如果使用上述几个方法，虽然可以使用get传参，但是get的url长度是有限的，若要传递大量数据，需要使用post传参。
	​	example：
	​		<form action="demo1.php" method="post" target='regzone'>
	​			用户名：<input type="text" name='username'/><br/>
	​			邮件地址：<input type="text" name='pwd'/><br/>
	​			<input type="submit" value='submit'/>
	​		</form>
	​		<iframe name='regzone' width='0' height='0'></iframe>

		点击后，相当于让iframe去加载这个请求，所以你可以输出javascript脚本，来控制页面显示。
	
	总结：
	​	不使用XMLHttpRequest对象，依然可以使用js来实现对后台服务器的请求，同时不会刷新或跳转页面。
	​	
		所谓ajax，既是指在页面不刷新的情况下，利用XMLHttpRequest对象请求服务器，发送请求，或者说是js的网络化。
	思考：
	​	ajax上传文件能实现么？
	​	分析：
	​		从HTTP协议的角度来看，上传文件，则需要把文件的内容发送到服务器。
	​		如果可以，XMLHR对象在POST数据时，把文件的内容也发送过去了
	​			-> XHR对象获取了你要上传文件的内容 
	​			-> javascript读取了你本地的文件内容 
	​			-> 出于安全原因，js是无法读取到文件内容的
	​			-> 所以ajax上传文件是无法实现的。
	​		
			ajax上传插件是如何实现的？
			一般用3个方式来实现，
			a. 使用iframe，file标签提交后，跳转到iframe页面，用iframe伪装的。
			b. 用flash实现的，如swfuplpaded插件
			c. 用html5实现(已经增加了文件读取API，才使ajax上传有了真正的可能)。

2. XMLHttpRequest对象
	一个专门的HTTP请求工具，IE678都是有自己的写法，IE9则标准了。
	2.1 XHR对象方法
	​	open(method,url[,async])
	​		方法说明：
	​			设置请求方法，请求资源
	​		参数说明：
	​			method：请求的方法，常见的get和post
	​			url：请求资源
	​			async：异步或同步请求，异步：true，同步：false，默认异步。
	​		注：异步的区别
	​			var xhr=createXHR();
	​			xhr.open('get','demo1.php',true);
	​			xhr.send();	
	​			alert(xhr.responseText);

				//异步的话，send在发送请求过程中，代码会继续往下执行，所以若服务器端处理请求有2秒延迟，那么代码会直接会弹出null，因为异步不会等服务器响应完毕后才执行代码。
				//同步的话，send发送请求会等响应完毕，代码再往下执行。
				
				//所以使用异步，需要配合ajax对象的状态码来进行判断，以及回调函数的使用。
	
		setRequestHeader(header,value)
			方法说明：设置请求头信息
			参数说明：
				header：请求头
				value：请求头信息
			
		send(content)
			方法说明：
				发送请求，只有执行到ajax对象的send方法时，才会真正发送请求
			参数说明：
				content：请求主体内容。
	
	2.2 XHR对象属性
	​	readyState 
	​		XHR对象的状态码
	​			Ajax对象从创建到初始化到发送请求到接收数据时，它的状态码会发生改变
	​			0：对象刚创建
	​			1：open方法被成功调用
	​			2：成功接受响应头信息
	​			3：成功接受响应头主体，该状态可能出现多次，因为主体内容的长度有可能不知道，chunk传输数据，就会这样。
	​			4：请求完成，断开连接。

		onreadystatechange
			事件属性，当XHR对象的状态码发生改变时所触发的回调函数
	
		status
			http的响应状态码
		statusText
			状态码对应的文字秒数
	
		reponseText
			服务器响应的数据
	
		responseXML
			对于大量的格式化文档，可以用XML来传输或交换，由后台程序把数据封装在XML文档中，js接受XML对象并解析内容。
	
		onabort
		abort
			忽略，中断请求。
	
		onprogress
			HTML5最新标准的XHR对象的实现才有的属性
		upload: 
			HTML5上传文件，就全靠upload属性，该属性是一个XMLHttpRequestUpload对象，不仅能上传文件，还能计算上传过程。
		responseType:
			响应类型
		getResponseHeader();

	2.3 XHR对象创建
	​	a. ie浏览器
	​		var xhr=new ActiveXObject('Microsoft.XMLHTTP');
	​	b. w3c浏览器
	​		var xhr=new XMLHttpRequest();

	2.4 请求发送示例
	​	example ：get request
	​		function createXHR(){
	​			//var xhr;
	​			//判断客户端浏览器的代理头信息中是否有‘MSIE’字符串
	​			//如果有，说明是IE浏览器
	​			if(window.navigator.userAgent.indexOf('MSIE')>0){
	​				return new ActiveXObject('Microsoft.XMLHTTP');
	​			}else{
	​				//如果没有是其他类型浏览器
	​				return new XMLHttpRequest();
	​			}
	​			/*
	​				//其他实现方式
	​				try{
	​					return new ActiveXObject('Microsoft.XMLHTTP');
	​				}catch(e){
	​				}
	​				try{
	​				}catch(e){
	​					return new XMLHttpRequest();
	​				}
	​				alert('请换一个');
	​			*/
	​		}
	​		var xhr=createXHR();
	​		xhr.open();
	​		xhr.onreadystatechange=function(){
	​			if(this.readyState==4 && this.status=200){//处理返回的数据
	​				//根据返回内容的类型来写响应的代码
	​				this.responseText
	​				this.responseXML
	​			}
	​		}
	​		xhr.send();

		example：post request
			var xhr=createXHR();
			xhr.open('post','./demo.php',true);
	
			xhr.onreadystatechange=function(){
				if(xhr.readyState==4 && xhr.status=200){//处理返回的数据
					//根据返回内容的类型来写响应的代码
					xhr.responseText
					xhr.responseXML
				}
			}
			//设置请求头
			xhr.setRequestHeader(‘content-type’,’application/x-www-form-urlencoded’);
	
			var data="username="+name+"&pwd="+pwd;
			//发送请求内容
			xhr.send(data);

3. ajax返回值之json与XML
  ajax返回值类型，不考虑HTML5的最新特性，返回值有俩种：普通文本、XML文档
  3.1 普通文本
  ​	作为普通文本返回类型时，有以下几种使用方式
  ​	a. 后台返回大段的html代码，直接innerHTML页面。
  ​	b. json格式的文本，使用eval()函数，将字符串解析成json对象。
  ​		对于php服务器端，json_encode()等函数用于将数组和对象转换成json格式的文本。
  ​		json_decode()：解码
  ​	c. 简短的标志字符串，如0、1、OK等。

  3.2 xml
  ​	xml文档使用responseXML属性来进行解析。
  ​	example：
  ​		var xhr=createXHR();
  ​		xhr.open('get','demo1.php',false);
  ​		xhr.onreadystatechange=function(){
  ​			if(this.readyState==4 && this.status==200){
  ​				//在这里处理响应内容
  ​				//将响应的内容解析成xml对象，responseXML：解析成xml对象
  ​				alert(this.responseXML);
  ​				var xmldom=this.responseXML;
  ​				var chs=xmldom.getElementsByTagName('book')[0].childNodes

  				childNodes：对于空白字符、换行符都解析成一个文本节点
  				children：不标准，但是很实在，多数浏览器也支持，children是直接得到元素节点，忽视文本。
  	
  				alert(chs.firstChild.firstChild.wholeText);
  			}
  		}
  		xhr.send(null);

4. 异步原理与XHR状态码
	4.1 XHR.readyState
	​	XHR对象的状态码
	​	0：对象刚创建
	​	1：当open方法被成功调用的时候，状态为1，在这个状态中可以设置请求头。
	​	2：接受到服务器的头信息，状态为2
	​	3：接受到服务器的主体信息，状态为3
	​	4：接受完毕，断开连接，状态为4

	4.2 异步原理
	​	js的代码是逐行执行的
	​	code		web
	​	1.send()	执行5s
	​		由于异步线程不阻塞，代码继续执行
	​	2.执行
	​	3.执行

		js是如何实现异步的原理？
		这是因为异步(线程)不阻塞，send之后后续的代码可以继续执行，而当服务器返回数据的时候，这时候会触发XHR对象的回调机制，会去调用我们实现的回调函数。
	
		浏览器对DOM文档的渲染，也是从上到下执行代码进行渲染的。
		所以使用ajax的最大好处便是由于异步不阻塞，不会阻塞页面的渲染，当然js的代码是执行在文档中实现的。

5. ajax跨域
  XHR对象是无法跨域请求的。
  但是script、img、这俩个标签都可以加载不同网址的资源，因此我们可以动态创建这俩个标签来实现跨域。

  form、img、srcipt，这三个标签是否可以去跨域请求资源？

  example：
  ​	对于ajax跨域，服务器端需要传送jsonp格式的数据。
  ​	ajax.google.com?key=demo&callback=callbackFunc
  ​	
  	callback：请求的时候，传送过去的回调函数名字，服务器将会把数据封装成一个json对象格式的数据，放到回调函数中作为参数来给客户端调用，这种数据的格式就是jsonp。
  	
  	function sear(){
  		var scr=document.createElement('script');
  		var url="请求资源的地址";
  		scr.setAtrribute('type','text/javascript');
  		src.setAttribute('src',url);
  	
  		document.getElementByTagName('head')[0].append(src);
  		//添加到head节点中，发出请求。
  	}
  	
  	function callbackFunc(res){
  		console.log(res);//可以看到响应的数据。
  	}

6. ajax上传
	本质上来说，ajax是无法实现上传文件的，因为js无法读取本地文件的。
	6.1. iframe上传
	​	jquery-uploaded-file，这是jquery的上传插件
	​	/*
	​		分析：
	​		1. 捕捉表单提交的动作
	​		2. 创建一个iframe
	​		3. 把表单的target修改，指向该iframe
	​		4. 服务器端响应并删除iframe元素
	​	*/
	​	function ajax_upload(){
	​		var ifname="up"+Math.random();
	​		$("<iframe name='"+ifname+"'></iframe>").appendTo($('body'));
	​		$('form:first').attr('target',ifname);
	​	}
	​	不足之处，客户端无法知道上传的进度。
	
	6.2 HTML5
	​	HTML5，低版本的浏览器都不支持，现代浏览器大部分支持。
	​	用XHR2 + FILE API + FormData来实现上传功能。
	​	a. FormData 
	​		"表单数据"对象，html5新增的API，能以表单作为对象，自动的把表单的数据打包，当ajax发送数据时，发送此FormData，达到发送表单内各数据项的目的。
	​		append()
	​			以键值的形式封装数据，可以封装二进制数据。
	​		
			example：				
				var fm=document.getElemetsByTagName('form')[0];
				var fd=new FormData(fm);	//将form进行封装
	
				fd.append('name','zhangsan');	//添加信息
			
		b. files、file
			files：获取form标签后所有的属性。
			file：files的元素就是file对象。
			example：
				document.getElementsByTagName('')[0].files
			说明：
				files是一个文件列表，选中多个文件或1个文件都是一个文件列表。
				files数组中每个元素都存储了一个file对象，该对象真正的存储了文件的具体信息。
	
		c. 上传示例
			//创建表单对象
			var fd=new formData();
			//获取文件对象
			var pic=document.getElementByTagName('form')[0].files[0];
			//把文件内容追加到表单对象中
			fd.append(pic.name,pic);
	
			xhr.send(fd);	//具体的实现就不写了，这样去请求连接就可以了。
		
		d. URL
			将二进制对象读成浏览器显示的资源。
			var tmpimg=document.createElement('img');
			tmpimg.src=window.URL.createObjectURL(pic);
			append();//添加到某个节点上面。
		
		e. 进度条的文件上传
			分析：
				进度条需要最基本的俩个信息，总大小、以上传大小俩个信息。
				在HTML5中，有一个"上传过程"的事件，事件中可以读取到这俩个信息。
			具体思路：
				 在上传过程中，不断的触发上传事件来调用函数，函数读取以上传/总大小，同时一直更新页面的进度条
			onprogress：上传过程事件，XHR对象upload的属性
			example：
				function upload(){
					var file=document.getElementById('pic').files[0];
					//console.log(file);
	
					//将文件内容封装到FormData对象中
					var fd=new FormData();
					fd.append('pic',file);
	
					var xhr=createXHR();
					xhr.open('post','file_upload.php');
					xhr.onreadystatechange=function(){
						if(this.readyState==4){
							alert(this.responseText);
						}
					}
					//为上传事件绑定监听函数
					xhr.upload.onprogress=function(e){
						if(e.lengthComputable){
							var parcent=100*e.loaded/e.total;
							document.getElementById('bar').style.width=parcent+"%";
						}
					}
					//xhr.setRequestHeader("Content-Type","multipart/form-data");
					
					xhr.send(fd);
				}
			注：HTML5中，ajax的跨域有了新的请求规则，能够跨域取决于对方的应答，对方的服务器如果愿意接受远程过来的ajax请求或某几个域名过来的ajax请求，会在响应头中加
			Access-Control-Allow-Originc：*、baidu.com
	
		f. 大文件切割上传
			需要的API：
				file对象 -> 继承自 -> blob对象
				Blob对象有slice方法，可以截取二进制对象的一部分，返回截取的二进制数据。
			思路：
				截取10M，上传。
				判断文件有没有截取完毕。
				while (有数据){
					截取，ajax上传
				}
				
				用定时器，不断调用上传方法。
			example：


	6.3 swf插件
		swfupload插件、php apc

7. comet反向ajax
	别名服务器推技术，server push。
	在实时聊天、消息推送中经常被使用。

	7.1 长连接+chunk
	7.2 长连接+轮询
	​	所谓的轮询是一个这种的方案。
	​	客户端发出请求，服务器端保持连接(长连接)，但有客户端数据的时候则返回数据，同时断开连接。这个时候，客户端会再发一个请求过来，重复上述过程。


ajax遗留问题
​	异步和同步的区别
​	浏览器的渲染原理：浏览器渲染线程、js引擎线程，俩者的关联。主要是ajax切割文件的进度条效果产生的问题。
​	while(sta>totalSize){
​		xhr=new XMLHttpRequest();
​		xhr.open();	//请求处理
​		xhr.send();	//发送数据
​		
		DOM operating	//DOM节点操作。
		//会发现，DOM的渲染是在程序
	}



/*
​	问题1：异步的问题
​		在切割上传的时候，若使用异步发送的方式，有可能是4个并行发送的，无法确认响应的先后顺序，也就是分割数据的顺序无法保证。
​		可以对切割的文件进行编号，服务器再进行组合。

	问题2：浏览器渲染原理
		上述代码你会发现，进度条是没有变化的，知道文件最后一块切割上产完毕，浏览器才会去渲染。
		js引擎是单线程的
		浏览器对于内容也有个渲染线程去渲染的
	
		若js对操作一个DOM节点，要等效果实现的话，
*/


缓存服务器
​		一般来说，缓存的内容都会被主服务器压缩过，那么缓存服务器是怎么判断该用户能读取这种压缩的内容呢？Vary，通过这个响应头，这是服务器和服务器之间的交换，通过读取用户的accept-encoding来判断是否能解析的格式。
​		vary：accept-encoding

	4. 缓存问题
		1. ie的缓存问题
			在ie浏览器中，每次发送ajax的请求的结果会被浏览器缓存下来，保存在浏览器的临时文件夹下，如：
	
			localhost/demo.php?first=15&second=2
	
			第一次访问时IE会把这个请求处理的结果缓存到本地，下次发送请求时，如果还是与上次请求的url完全一样，那么浏览器会自动找到之前的缓存文件，并且使用。
		
			PS：ie的缓存问题，似乎在win7之后的版本就不会发生了。
		2. 解决方案
			a. 随机数
				var url='demo.php?name='+name+'&_'+Math.random();
				PS：使用Math.random()，会在本地产生大量的缓存文件，从概率上讲，随机数可能会重复。
			b. 时间戳
				var url='demo.php?name='+name+'&_'+new Date().getTime();
				同样的，这种解决方案会产生大量的缓存文件
			c. 设置请求头
				setRequestHeader('If-Modified-Since','0');
				Ajax请求时如果之前有缓存文件，本次的请求url与之相同（同时Modifed时间与服务器文件事件一致），会直接使用缓存文件
				我们可以设置If-Modified-Since请求头，这样一来，ajax就会重新发生http请求。


				If-Modified-Since
					标准的HTTP请求头标签，在发送HTTP请求时，把浏览器端缓存页面的最后修改时间一起发到服务器去，服务器会把这个时间与服务器上实际文件(即请求的处理文件)的最后修改时间进行比较。
					
					如果时间一致，那么返回HTTP状态码304（不返回文件内容），客户端接到之后，就直接把本地缓存文件显示到浏览器中。
	
					如果时间不一致，就返回HTTP状态码200和新的文件内容，客户端接到之后，会丢弃旧文件，把新文件缓存起来，并显示到浏览器中。
	
				自己理解的流程：
					在ie中，请求连接不变的情况下，第一次请求服务器，服务器会返回数据和文件最后的修改时间，ie浏览器接收数据并进行缓存。
	
					第二次请求，ie浏览器发现请求链接不变，所以带上modified请求头信息，服务器处理请求，发现俩次的事件是一致的，返回304状态码，不返回文件内容，ie浏览器根据状态码使用本地缓存文件。
	
			d. 禁用缓存
				在php中的响应头中写入禁用缓存
				header("Cache-Control: no-cache, must-revalidate");
				这种方法从根本上禁止了缓存。

-->
<?php
function test(){
​	echo 'this is my test'.
}
