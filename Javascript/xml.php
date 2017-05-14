<?php 
//echo xml
header('Content-Type: text/xml;');  
$str=<<<EOF
<?xml version="1.0" encoding="UTF-8"?>
<book>
<name>php in action</name>
<price>99</price>
</book>
EOF;

echo $str;
?>