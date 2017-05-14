<?php
/*

// $str1 = "1223111111";
// $reg1 = "/(11)\1/i";		//记住在双引导之间，\ 是会转义的，跟单引号不一样。
// $reg1 = "/(11)\\1/i";	



1. 概念
	正则表达式由匹配符+限定符+定位符组成。
	可以理解成一个字符串模板，通过这个模板来匹配字符。

	a. 匹配符 => 确定匹配的字符
	b. 限定符 => 确定匹配的字符个数
	c. 定位符 => 确定匹配范围

2. 定界符
	定界符，通常以正斜杠"/"作为定界符的开始和结束，也可以使用#。
	example：/
		$regex = '/^http:\/\/([\w.]+)\/([\w]+)\/([\w]+)\.html$/i';
		$str = 'http://www.youku.com/show_page/id_ABCDEFG.html';
		$matches = array();
		//preg_match中的$matches[0]将包含与整个模式匹配的字符串。  
		if(preg_match($regex, $str, $matches)){
		    var_dump($matches);
		}
		echo "\n";

	example：#
		//使用"#"定界符的代码如下.这个时候对"/"就不转义
		$regex = '#^http://([\w.]+)/([\w]+)/([\w]+)\.html$#i';
		$str = 'http://www.youku.com/show_page/id_ABCDEFG.html';
		$matches = array();

		if(preg_match($regex, $str, $matches)){
		    var_dump($matches);
		}
		echo "\n";

3. 修饰符
	用于改变正则表达式的行为。
	表达式'#^http://([\w.]+)/([\w]+)/([\w]+)\.html$#i'中的组后一个i就是修饰符，表示忽略大小写，还有一个就是我们经常用到的是"x"，表示忽略空格。
 
	//example：
		$regex = '/HELLO/';
		$str = 'hello world';
		$matches = array();
		//无修饰符
		if(preg_match($regex, $str, $matches)){
		    echo 'No i:Valid Successful!',"\n";
		}
		//有修饰符i
		if(preg_match($regex.'i', $str, $matches)){
		    echo 'YES i:Valid Successful!',"\n";
		}

4. 元字符
	用于限定字符个数。

	* 	0到多，{0,}
	+ 	1到多，{1,}
	? 	0或1次，{0,1}

	{n}：匹配n个字符
	{n,}：匹配n到多个字符。
	{n,m}：匹配n-m个字符。

	如[\w]{3,5}、[\w]*、[\w]+，这些[\w]后面的符号都表示限定符。
	{3,5}表示3到5个字符，{3,}超过3个字符，{,5}最多5个字符，{3}3个字符
	
	//example：{3,5}
		//匹配3-5个h字符，在贪婪模式下，字符串满足匹配条件且超过5个字符，会匹配5个字符。
		$regex='/h{3,5}/';	
		$str = 'hhhhhhhh';
		$matches=array();
		if(preg_match($regex, $str, $matches)){
			var_dump($matches);	//输出hhhhh
		}

	//example：{3,}
		$regex='/h{3,}/';	//匹配3到多个字符
		$str = 'hhhh';
		$matches=array();
		if(preg_match($regex, $str, $matches)){
			var_dump($matches);	//贪婪模式下，输出4个h字符
		}

	//example：{5}
		$regex='/h{5}/';	//匹配最多5个字符
		$str = 'hhhhh';
		$matches=array();
		if(preg_match($regex, $str, $matches)){
			var_dump($matches);	//输出5个h
		}

	//example：*
		$regex='/ah*a/';	//匹配0到多个h字符
		$str1 = 'aa';	
		$str2 = 'aha';	
		$matches=array();
		preg_match($regex, $str1, $matches);
		var_dump($matches);	//输出aa
		preg_match($regex($regex, $str2, $matches);
		var_dump($matches);	//输出aha
	
5. 匹配符
	用于匹配某类字符中的一个字符。

	\d ：匹配一个数字字符。[0-9]
	\D ：匹配一个非数字字符。[^0-9]

	\w ：匹配0-9，a-z，A-Z中的任意一个字符，相当于[0-9a-zA-Z]
	\W ：匹配一个非单词字符，相当于[^0-9a-zA-Z_]

	\s ：匹配空白字符、空格、换行符、制表位中的任意一个。
	\S ：匹配一个非空白字符。 
	.  ：匹配除 "\n" 之外的任何单个字符
	
	example：
		$str = 'aB1_';
		$regex = '/\w/';
		$m =array();
		preg_match_all($regex, $str ,$m);
		var_dump($m);	//输出a、B、1、_

6. 字符域
	在表达式中，[a]用方括号括起来的部分就是字符域，表示只匹配一个字符。
	注：在字符域中，
	
	3.1 只匹配一个a字符
		[a]：只会匹配一个a
	
	3.2 匹配某个范围的任意一个字符
		[a-z]	：匹配a-z中的任一字符
		[A-Z] ：匹配A-Z中的任一字符
		[0-9] ：匹配数字0-数字9中的任一数字（\d）
		[0-9a-z] : 匹配数字0-数字9或a-z中任一字符
		[0-9a-zA-Z] : 匹配数字0-数字9或a-z或A-Z中的任一字符
		[abcd] ：匹配字符a或字符b或字符c或字符d
		[1234] ：匹配数字1或数字2或数字3或数字4
		
		example:
			$str = 'helloWORLDwangWU';
			$regex = '/[^a-z]+/';
			$m =array();
			preg_match_all($regex, $str ,$m);
			var_dump($m);	//输出RORLD和WU
	
	3.3 匹配指定范围外的某个字符
		^配合上档键用来否定某些字符。
		
		[^a-z] ：匹配除a-z中的任一字符
		[^0-9] ：匹配除0-9中的任一字符
		[^abcd] ：匹配除字符a或字符b或字符c或字符d以外的任一字符

		[^>]+
			匹配除了>字符的1到多个字符。
			example：
				$str = '<html>hahahahhahaha</html>';
				$regex = '/[^>]+/';
				$matches=array();
				preg_match_all($reges,$str,$matches);
				var_dump($matches);	//<html 和 hahahahhahaha</html

7. ^，上档键
	用于字符域中表示取反，否定的意思，例如：
	[^abcd]：匹配除了abcd字符以外的任意一个字符。

	放在表达式之前，表示以当前这个字符开始，例如：
	(/^n/i)，表示以n开头。

8. 定位符
	定位表达式所匹配字符的起始和结束，也可以创建只在单词内或只在单词的开始或结尾处出现的正则表达式。
	
	起始
		^：匹配输入字符串的开始位置。
	结束
		$：匹配输入字符串的结束为止。
	单词边界
		\b：匹配一个单词边界，开始、空格、结尾称为单词边界。
		
	
	var reg=/^xxxx$/i'
	表示以x开始，以x结束。
	example ：	
		$str = "anhmyello,my an name is zhangsan";
		$reg1 = "/h\w+l/i";		//这个会匹配字符串中的内容，会输出hmyel
		$reg2 = '/^h\w+l$/';	//null，因为只匹配h开头，l结尾的字符串
	
	example：单词边界
		var reg=/^\ban$/i;
		//表示在字符开始的边界或字符中以空格开始的边界开始匹配。

		var reg=/an\b/gi;
		//表示在字符串结尾的边界开始来匹配。

		var reg=//\ban\b/gi;
		//表示在字符串以单词边界开始、单词边界结尾来匹配。

		var str="anhmyello,my an name is zhangsan";
		var reg=/\ban\b/gi;	//输出an

	7.2 \B：匹配非单词边界，即不是以开始、空格、结尾的单词边界的匹配。
			var reg=/an\B/gi;
			//表示不是以空格、结尾的单词边界的匹配
			var reg=/\Ban\B/gi;
			//表示以非单词边界开始，非单词边界结尾的字符串。
			
			var str="anhmyello,my an name is zhangsan";
			alert(str.length);
			var reg=/an\B/gi;
			var result;

			while(result=reg.exec(str)){
				document.write(result+'<hr>');
				document.write(reg.lastIndex+'<hr>');

9. 转义
	有时候，我们需要匹配特殊字符，例如：
	/	.	()	{}	'  "  []  *  ?  +  ^  $  -
	但是这些字符在正则表达式中是有其意思的，这个时候可以使用反斜杠进行转义，才能正常匹配。

10. 子表达式
	在正则表达式中，通过一对小括号()括起来的表达式，叫做子表达式。
	子表达式主要用于捕获内容，将匹配到的内容存储到缓冲区中，通过反向引用方便使用，例如：
	$reg = '/\w(\w)\w/i';

	10.1 反向引用
		通过\n，可以取出缓冲区中匹配到的数据。
		n是一个数字，例如\1，表示第一个匹配到的子表达式的内容。

		example ：子表达式
			$str='abc1234fsdfds';
			$reg='/abc\d(\d)(\d)/i';

			$m = array();
			preg_match_all($reg, $str, $m, PREG_PATTERN_ORDER);
			var_dump($m);	//输出abc123，1，2

			原表达式：/abc\d\d\d/
			俩个子表达式：\d，\d
			第一次匹配到abc123，子表达式在原表达式匹配到的内容上进行匹配，第一个子表达式匹配到2，第二个子表达式匹配到3

		example：反向引用
			$str='f123php123fdsfdsf1456php789fdsf';
			$reg='/(\d\d\d)php\1/i';
			$m = array();
			preg_match_all($reg, $str, $m, PREG_PATTERN_ORDER);
			var_dump($m);

			a. 原表达式先匹配：子表达式(\d\d\d)匹配3个数字，存储缓冲区，再匹配php3个字符，\1引用缓冲区的内容补充原表达式，所以匹配到的内容是123php123
			b. 追加缓冲区内容，按照子表达式顺序。

		example：3
			$str1 ='abc12334fsdfds';	
			$str2 ='abc12234fsdfds';
			$reg='/abc\d(\d)\1/i';		
			//以子表达式匹配到的内容作为原表达式的补充来进行原表达式的匹配。
			//注：$str1作为字符串匹配是匹配不到数据的，因为\1缓冲区的内容是2，

			$m = array();
			preg_match_all($reg, $str2, $m, PREG_PATTERN_ORDER);
			var_dump($m);	//输出abc123，1，2

11. 选择匹配符
12. 预查

13. 贪婪模式与非贪婪模式
	13.1 贪婪模式
	13.2 非贪婪模式
		
	贪婪与非贪婪模式影响的是被量词修饰的子表达式的匹配行为。
	贪婪模式在整个表达式匹配成功的前提下，尽可能多的匹配，而非贪婪模式在整个表达式匹配成功的前提下，尽可能少的匹配。
	
	属于贪婪模式的量词，也叫做匹配优先量词，包括：*、+、?、{m,}、{m,n}
	在一些使用NFA引擎的语言中，在匹配优先量词后加上?，即变成属于非贪婪模式的量词，也叫做忽略优先量词，包括：“{m,n}?”、“{m,}?”、“??”、“*?”和“+?”。 
	
	//example：
		//贪婪匹配
		var reg=/\d{3}/gi;	//匹配已知数目的字符串
		
		var reg=/\d{3,}/gi;	//匹配最少有多少个字符，输出23423234234
		
		var reg=/\d{3,6}/gi;	//匹配某一数量范围内的字符串，输出234232，34234

		//非贪婪匹配
		//匹配成功下会尽可能的少匹配，会3个数字3个数字的输出
		var reg=/\d{3,6}?/gi;	

		var str='fdjsk2jf2p23423234234fdf3';
		var result=str.match(reg);
		alert(result);
	

	http://www.jb51.net/article/31491.htm
	好帖子，回头来继续整理。

	观察第二个第三个表达式，我们发现，当我们去匹配某个数量返回内的数据，正则默认会匹配最多个那个结果。这种情况叫做贪婪模式。
	贪婪模式：优先匹配最多的那个结果。
	非贪婪模式：匹配最少的那个情况，可以采用?的形式实现。



*/



// $str='f123php123fdsfdsf1456php789fdsf';
// $reg='/(\d)(\d\d)php\1/i';
// $m = array();
// preg_match_all($reg, $str, $m, PREG_PATTERN_ORDER);
// var_dump($m);




/*
正则表达式的注释
	格式：(?# 注释内容)
	用途：主要用于复杂的注释

	//连接mysql数据库的正则表达式

	$regex = '/
    ^host=(?<!\.)([\d.]+)(?!\.)                 (?#主机地址)
	\|
	    ([\w!@#$%^&*()_+\-]+)                       (?#用户名)
	\|
	    ([\w!@#$%^&*()_+\-]+)                       (?#密码)
	(?!\|)$/ix';
	 
	$str = 'host=192.168.10.221|root|123456';
	$matches = array();
	 
	if(preg_match($regex, $str, $matches)){
	    var_dump($matches);
	}
	 
	echo "\n";
 */











/*
6. 通配符(lookarounds)
	断言某些字符串中的某些字符的存在与否！
	lookarounds分两种:lookaheads(正向预查 ?=)和lookbehinds(反向预查?<=)。
	6.1 格式
		正向预查:(?=) 相对应的 (?!)表示否定意思
		反向预查:(?<=) 相对应的 (?<!)表示否定意思
		前后紧跟字符

	$regex = '/(?=c)d(?=e)/';  // d 前面紧跟c, d 后面紧跟e
	$str = 'abcdefgk';
	$matches = array();
	 
	if(preg_match($regex, $str, $matches)){
	    var_dump($matches);
	}
	echo "\n";
	
	//否定意义
	$regex = '/(?<!c)d(?!e)/';  // d 前面不紧跟c, d 后面不紧跟e
	$str = 'abcdefgk';
	$matches = array();
	 
	if(preg_match($regex, $str, $matches)){
	    var_dump($matches);
	}
	 
	echo "\n";
*/







// 一个过滤类，能过滤xss，进行富文本过滤，sql过滤
// 一个验证类，验证数据的格式
// pdo
// mysqli
