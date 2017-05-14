<?php

// $regex = '/\(?0\d{2}\)?[- ]?\d{8}|0\d{2}[- ]?\d{8}/i';	
// $n1 = '022 23423423';
// preg_match($regex, $n1, $matches);
// print_r($matches);


// $regex = '/a/i';
// $content = 'abcda123a';
// preg_match($regex, $content, $matches);
// print_r($matches);		//输出a

// $regex = '/a?/i';
// $content = 'dabcda123a';
// preg_match($regex, $content, $matches);
// print_r($matches);		//输出空字符串

$regex = '/\d{3}[1\-\)]?/i';	//匹配3个数字，其后跟随1或-或)，-在字符域中有特殊意义的，需要转义
$content = '123)-1fjkdjfl';
preg_match($regex, $content, $matches);
print_r($matches);