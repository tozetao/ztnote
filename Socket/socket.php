<?php
// 不同请求类型的模拟

//get 请求模拟
function sock_get($url){
	/*
		模拟流程
			1. 打开一个socket
				fsocketopen()，连接指定的主机地址，例如：
				localhost或者www.baidu.com

			2. 模拟HTTP GET请求头
				GET test.php?name=lisi HTTP/1.1 \r\n
				HOST localhost
			
			3. 写入socket

			4. 读取响应内容
				包括响应协议和输出内容。
	 */
	$data = parse_url($url);
	$handle = fsockopen($data['host'],80,$errno,$errstr);
	//检测socket是否正常
	if(!$handle){
		echo $errstr."：".$errno;
		exit;
	}
	$out = "GET ".$data['path']."?".$data['query']." HTTP/1.0\r\n";
	$out .= "HOST:".$data['host']."\r\n\r\n";
	echo $out;
	$rs = fwrite($handle, $out);
	// echo $rs;
	while(!feof($handle)){
		echo fgets($handle);
	}
}
// sock_get("http://localhost/test2.php");

//post请求模拟
function sock_post($url,$content)
{	
    $info = parse_url($url);
    $fp = fsockopen($info['host'],80,$errno,$errstr,3);
    if(!$fp){
    	echo $errstr."：".$errno;
    	exit;
    }
    $head = 'POST '.$info['path']." HTTP/1.0\r\n";
    $head .= "HOST: ".$info['host']." \r\n";
    $head .= "Content-type: text/xml\r\n";
    $head .= "Content-Length:".strlen(trim($content))."\r\n";
    $head .= "\r\n";
    $head .= trim($content);

    $rs = fputs($fp,$head);
    while(!feof($fp)){
    	$line = fgets($fp);
    	echo $line;
    }
    /*
	    POST /test2.php HTTP/1.0
		HOST: localhost 
		Content-type: text/xml
		Content-Length:306
		\n\r：这里是换行
		下面是POST请求的内容
     */
}

//fsocket模拟post提交
$url = "http://localhost/test2.php";
$content = "<xml>  
    <ToUserName><![CDATA[ebyben1413005325]]></ToUserName>  
    <FromUserName><![CDATA[ga_6281708af4c6]]></FromUserName>  
    <CreateTime>1413341614</CreateTime>  
    <MsgType><![CDATA[text]]></MsgType>  
    <Content><![CDATA[首页]]></Content>  
    <MsgId>1234567890123456</MsgId></xml>";
sock_post($url,$content);
