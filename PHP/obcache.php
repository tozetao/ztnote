<?php
/*

1. ob缓存的配置
	output_buffering
		php配置参数，值是 on/off 或者字节大小，on的话所有脚本都会使用输出缓存控制，不限制缓存的大小，off的话，则脚本的输出会即时发送到客户端。
		如果是字节大小，那么会开启缓存，且当缓冲区达到该字节数后脚本会自动刷新缓冲区并输入到客户端。
	
	implicit_flush
		设定ON意味着，当脚本有输出时，自动立即发送到客户端。相当于在echo后自动加flush()。

ob_start(output_handle)
	打开php的输出缓存，当输出缓存激活后，脚本将不会输入内容(除http标头外)，相反需要输出的内容会被存储在内部缓冲区中。

	output_handle：
		该参数是一个回调函数，在当前缓冲区中的内容被冲刷或请求结束之际输出缓冲区内容被冲刷到浏览器的时候会被调用。
		当被调用的时候，它将收到当前缓冲区的内容被预期返回一个新的输出缓冲区作为结果，这个新的缓冲区内容将会输出给浏览器。

ob_get_contents()
	该函数可以得到内部缓冲区的内容。

ob_flush()
	冲刷输出缓冲区的内容，此函数不会关闭缓冲区的内容。

ob_end_flush()
	输出缓冲区的内容并关闭缓冲区，也就是说在调用该函数后，缓冲区的内容会被清空并关闭。


flush()
	flush()函数是操作web服务器的缓冲区。
	严格来讲，只有php作为apache的一个模块被使用的时候，才有实际作用。


ob_xx系列函数，是操作php自身的缓冲区

说明：
	有些apache的模块，比如mod_gzip会自己进行输出缓存，这会导致flush()函数产生的结果不会立即发送到客户端浏览器。

	有些浏览器会缓存一定字节在进行页面的输出，例如IE会在接受到256个字节以后才开始显示页面。
	
	一些web服务器的output_buffering默认是4069或者更大，即缓冲区的输出内容必须达到4069字节服务器才会将数据刷新，推送出去。

example1：
	ob_end_clean();

	ob_start();
	print str_repeat(" ", 4096);	//刷爆web服务器，浏览器缓存空间大小。
	echo 'one';
	echo '<br/>';
	ob_flush();
	flush();
	sleep(1);

	echo 'two';
	echo '<br/>';
	ob_flush();
	flush();
	sleep(1);

	echo 'three';
	echo '<br/>';
	ob_flush();
	flush();
	sleep(1);
 
example2：
	function demo($data, $type){
		return 'is demo<br/>';
	}
	ob_start('demo');

	echo 'start';
	ob_flush();

	echo 'second';
	ob_flush();

	ob_flush();
	ob_end_flush();
	//上述代码会返回4行is demo。

example3：
	ob_end_flush();	//关闭缓冲区

	ob_start();		//开启第一个缓冲区
	print str_repeat(" ", 4096);
	echo 'parent';
	echo '<br/>';
	ob_flush();
	flush();

	//ob_end_clear();		//关闭第一个缓冲区

	ob_start();		//开启第二个缓冲区
	echo 'son1';
	echo '<br/>';
	file_put_contents('demo.txt', ob_get_contents(), FILE_APPEND);
	ob_flush();
	flush();
	sleep(1);



	echo 'son2';
	echo '<br/>';
	ob_flush();
	flush();
	sleep(1);

	echo 'son2';
	echo '<br/>';
	ob_flush();
	flush();
	sleep(1);

	echo 'son2';
	echo '<br/>';
	ob_flush();
	flush();
	sleep(1);
	ob_end_clean();
	

	输出缓冲区是可以堆叠的，只要确保你正确的调用了ob_end_flush()函数。
	
	在上面的例子中，如果不关闭第一个缓冲区，就是堆叠缓冲区，这时候子缓冲区的内容是不会随着flush输出到浏览器，而是等程序结束将内容一起输出。

	如果关闭第一个缓冲区，则程序回复正常。

 */