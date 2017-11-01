<?php

/*
curl
	能跟各种服务器使用各种类型的协议进行连接和通讯，
	curl_getinfo()
		返回响应信息
 */
function http_get(){
	$ch =curl_init();
	curl_setopt($ch, CURLOPT_URL, 'www.baidu.com');
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
	curl_setopt($ch, CURLOPT_HEADER, 0);
	$output = curl_exec($ch);
	curl_close($ch);
	return $output;
}

//2. 发送post请求
function http_post(){
	$url = 'localhost/post_output.php';
	$post_data = array(
		'name'=>'lisi',
		'age'=>25
	);
	$ch = curl_init();
	//set operation，设置操作的意思
	curl_setopt($ch,CURLOPT_URL,$url);	//设置链接
	curl_setopt($ch,CURLOPT_RETURNTRANSFER,1);	//不自动输出页面
	curl_setopt($ch,CURLOPT_POST,1);
	curl_setopt($ch,CURLOPT_POSTFIELDS,$post_data);
	$output = curl_exec($ch);	//执行，发送请求
	curl_close($ch);
	echo $output;	//讲输出一个数组
}

//3. 上传文件
function uploadFile(){
	//只需要将文件以参数的形式传递进行就可以上传了
	$post_data = array (
	    "foo" => "bar",
	    // 要上传的本地文件地址
	    "upload" => "@C:/wamp/www/test.zip"
	);
}
	
	
	
	
	
//请求头的设置
function http_header(){
	$ch = curl_init();
	$url = "http://apis.baidu.com/apistore/weatherservice/cityname?cityname=".urlencode('北京');
	$header = array(
		'apikey:2d15be1790358201c2134a008a96ed9a'
	);

	curl_setopt($ch,CURLOPT_HTTPHEADER,$header);
	curl_setopt($ch,CURLOPT_RETURNTRANSFER,1);
	curl_setopt($ch,CURLOPT_URL,$url);
	$res = curl_exec($ch);
	$info = curl_getinfo($ch);
	var_dump(json_decode($res,true));
}

//4. curl可以批处理句柄，可以同时或异步打开多个链接。



function http_proxy(){
	$ch　=　curl_init();
	$url = "http://www.cmx8.cn";
	// curl_setopt($ch,　CURLOPT_URL,　$url);
	curl_setopt($ch,　CURLOPT_HEADER,　1);
	curl_setopt($ch,　CURLOPT_RETURNTRANSFER,　1);
	curl_setopt($ch,　CURLOPT_HTTPPROXYTUNNEL,　1);
	// curl_setopt($ch,　CURLOPT_PROXY,　'proxy.lxvoip.com:1080');
	// curl_setopt($ch,　CURLOPT_PROXYUSERPWD,　'user:password');
	$data　=　curl_exec();
	curl_close($ch);
}

$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, 'localhost/curl.php');
curl_setopt($ch, CURLOPT_RETURNTRANSFER,false);		//决定curl_exec()
$data = curl_exec($ch);
curl_close($ch);


function postCurl($url,$body,$header,$type="POST"){
	//1.创建一个curl资源
	$ch = curl_init();
	//2.设置URL和相应的选项
	curl_setopt($ch,CURLOPT_URL,$url);//设置url
	//1)设置请求头
	//array_push($header, 'Accept:application/json');
	//array_push($header,'Content-Type:application/json');
	//array_push($header, 'http:multipart/form-data');
	
	//设置为false,只会获得响应的正文(true的话会连响应头一并获取到)
	curl_setopt($ch,CURLOPT_HEADER,0);
	curl_setopt ( $ch, CURLOPT_TIMEOUT,5); // 设置超时限制防止死循环
	//设置发起连接前的等待时间，如果设置为0，则无限等待。
	curl_setopt($ch,CURLOPT_CONNECTTIMEOUT,5);
	//将curl_exec()获取的信息以文件流的形式返回，而不是直接输出。
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
	//2)设备请求体
	if (count($body)>0) {
		//$b=json_encode($body,true);
		curl_setopt($ch, CURLOPT_POSTFIELDS, $body);//全部数据使用HTTP协议中的"POST"操作来发送。
	}
	//设置请求头
	if(count($header)>0){
	  	curl_setopt($ch,CURLOPT_HTTPHEADER,$header);
	}
	//上传文件相关设置
	curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true);
	curl_setopt($ch, CURLOPT_MAXREDIRS, 3);
	curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);// 对认证证书来源的检查
	curl_setopt($ch, CURLOPT_SSL_VERIFYHOST, 0);// 从证书中检查SSL加密算
	
	//3)设置提交方式
	switch($type){
		case "GET":
			curl_setopt($ch,CURLOPT_HTTPGET,true);
			break;
		case "POST":
			curl_setopt($ch,CURLOPT_POST,true);
			break;
		case "PUT"://使用一个自定义的请求信息来代替"GET"或"HEAD"作为HTTP请									                     求。这对于执行"DELETE" 或者其他更隐蔽的HTT
			curl_setopt($ch,CURLOPT_CUSTOMREQUEST,"PUT");
			break;
		case "DELETE":
			curl_setopt($ch,CURLOPT_CUSTOMREQUEST,"DELETE");
			break;
	}
	
	
	//4)在HTTP请求中包含一个"User-Agent: "头的字符串。-----必设
	curl_setopt($ch, CURLOPT_USERAGENT, 'SSTS Browser/1.0');
	curl_setopt($ch, CURLOPT_ENCODING, 'gzip');
	curl_setopt ( $ch, CURLOPT_USERAGENT, 'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0)' ); // 模拟用户使用的浏览器
	//5)
	
	
	//3.抓取URL并把它传递给浏览器
	$res=curl_exec($ch);
	$result=json_decode($res,true);
	//4.关闭curl资源，并且释放系统资源
	curl_close($ch);
	if(empty($result))
		return $res;
	else
		return $result;
}