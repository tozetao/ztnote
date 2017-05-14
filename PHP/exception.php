<?php

/*
try
	捕获异常的代码
throw
	抛出异常，每个throw必须对应一个catch
catch
	捕获异常，并包含一个异常信息的对象。

set_exception_handler()
	设置顶级异常处理器，设置处理所有未捕获异常的用户定义函数。
 */

function checkNum($num){
	if($num > 1){
		throw new Exception('value must be 1');
	}
	return true;
}

function myException(){
	echo 'this is my exception';
}
set_exception_handler('myException');

checkNum(2);

// 