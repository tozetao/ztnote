```php

 sql='select * from users_location where latitude > '.lat.'-1 and latitude < '.lat.'+1 and longitude > '.lon.'-1 and longitude < '.lon.'+1 order by ACOS(SIN(('.lat.' * 3.1415) / 180 ) *SIN((latitude * 3.1415) / 180 ) +COS(('.lat.' * 3.1415) / 180 ) * COS((latitude * 3.1415) / 180 ) *COS(('.lon.'* 3.1415) / 180 - (longitude * 3.1415) / 180 ) ) * 6380 asc limit 10'; 
```

```php

查找附近网点geohash算法及实现 （PHP版本）
2014年12月19日 10:21:53
阅读数：4241
参考文档：
http://blog.csdn.net/wangxiafghj/article/details/9014363geohash  算法原理及实现方式

http://blog.charlee.li/geohash-intro/  geohash：用字符串实现附近地点搜索
http://blog.sina.com.cn/s/blog_7c05385f0101eofb.html    查找附近点--Geohash方案讨论
http://www.wubiao.info/372        查找附近的xxx 球面距离以及Geohash方案探讨

http://en.wikipedia.org/wiki/Haversine_formula       Haversine formula球面距离公式

http://www.codecodex.com/wiki/Calculate_Distance_Between_Two_Points_on_a_Globe   球面距离公式代码实现

http://developer.baidu.com/map/jsdemo.htm#a6_1   球面距离公式验证  

http://www.wubiao.info/470     Mysql or Mongodb LBS快速实现方案

geohash有以下几个特点：
首先，geohash用一个字符串表示经度和纬度两个坐标。某些情况下无法在两列上同时应用索引 （例如MySQL 4之前的版本，Google App Engine的数据层等），利用geohash，只需在一列上应用索引即可。

其次，geohash表示的并不是一个点，而是一个矩形区域。比如编码wx4g0ec19，它表示的是一个矩形区域。 使用者可以发布地址编码，既能表明自己位于北海公园附近，又不至于暴露自己的精确坐标，有助于隐私保护。
第三，编码的前缀可以表示更大的区域。例如wx4g0ec1，它的前缀wx4g0e表示包含编码wx4g0ec1在内的更大范围。 这个特性可以用于附近地点搜索。首先根据用户当前坐标计算geohash（例如wx4g0ec1）然后取其前缀进行查询 （SELECT * FROM place WHERE geohash LIKE 'wx4g0e%'），即可查询附近的所有地点。
Geohash比直接用经纬度的高效很多。
Geohash算法实现（PHP版本）
[php] view plain copy
<?php  
class Geohash  
{  
    private $coding="0123456789bcdefghjkmnpqrstuvwxyz";  
    private $codingMap=array();  
    public function Geohash()  
    {  
        for($i=0; $i<32; $i++)  
        {  
            $this->codingMap[substr($this->coding,$i,1)]=str_pad(decbin($i), 5, "0", STR_PAD_LEFT);  
        }  
    }  

    public function decode($hash)  
    {  
        $binary="";  
        $hl=strlen($hash);  
        
        for($i=0; $i<$hl; $i++)  
        {  
            $binary.=$this->codingMap[substr($hash,$i,1)];  
        }  

        $bl=strlen($binary);  
        $blat="";  
        $blong="";  
        
        for ($i=0; $i<$bl; $i++)  
        {  
            if ($i%2)  
                $blat=$blat.substr($binary,$i,1);  
            else  
                $blong=$blong.substr($binary,$i,1);  
        }  
        
         $lat=$this->binDecode($blat,-90,90);  
         $long=$this->binDecode($blong,-180,180);  
        // $lat=$this->binDecode($blat,2,54);  
        // $long=$this->binDecode($blong,72,136);  
        $latErr=$this->calcError(strlen($blat),-90,90);  
        $longErr=$this->calcError(strlen($blong),-180,180);  
        $latPlaces=max(1, -round(log10($latErr))) - 1;  
        $longPlaces=max(1, -round(log10($longErr))) - 1;  
        $lat=round($lat, $latPlaces);  
        $long=round($long, $longPlaces);  
        return array($lat,$long);  
    }  

    public function encode($lat,$long)  
    {  
        $plat=$this->precision($lat);  
        $latbits=1;  
        $err=45;  
        
        while($err>$plat)  
        {  
            $latbits++;  
            $err/=2;  
        }  
        
        $plong=$this->precision($long);  
        $longbits=1;  
        $err=90;  
        while($err>$plong)  
        {  
            $longbits++;  
            $err/=2;  
        }  
        
        $bits=max($latbits,$longbits);  
        $longbits=$bits;  
        $latbits=$bits;  
        $addlong=1;  
        
        while (($longbits+$latbits)%5 != 0)  
        {  
            $longbits+=$addlong;  
            $latbits+=!$addlong;  
            $addlong=!$addlong;  
        }  

        $blat=$this->binEncode($lat,-90,90, $latbits);  
        $blong=$this->binEncode($long,-180,180,$longbits);  
        $binary="";  
        $uselong=1;  
        
        while (strlen($blat)+strlen($blong))  
        {  
            if ($uselong)  
            { 
                $binary=$binary.substr($blong,0,1);  
                $blong=substr($blong,1);  
            }  
            else  
            {  
                $binary=$binary.substr($blat,0,1);  
                $blat=substr($blat,1);  
            }  
            $uselong=!$uselong;  
        }  

        $hash="";  
        for ($i=0; $i<strlen($binary); $i+=5)  
        {  
            $n=bindec(substr($binary,$i,5));  
            $hash=$hash.$this->coding[$n];  
        }  
        return $hash;  
    }  
    
    private function calcError($bits,$min,$max)  
    {  
        $err=($max-$min)/2;  
        while ($bits--)  
            $err/=2;  
        return $err;  
    }  
    
    private function precision($number)  
    {  
        $precision=0;  
        $pt=strpos($number,'.');  
        if ($pt!==false)  
        {  
            $precision=-(strlen($number)-$pt-1);  
        }  
        return pow(10,$precision)/2;  
    }  
    
    private function binEncode($number, $min, $max, $bitcount)  
    {  
        if ($bitcount==0)  
            return "";  
        $mid=($min+$max)/2;  
        
        if ($number>$mid)  
            return "1".$this->binEncode($number, $mid, $max,$bitcount-1);  
        else  
            return "0".$this->binEncode($number, $min, $mid,$bitcount-1);  
    }  
    
    private function binDecode($binary, $min, $max)  
    {  
        $mid=($min+$max)/2;  
        if (strlen($binary)==0)  
            return $mid;  
        $bit=substr($binary,0,1);  
        $binary=substr($binary,1);  
        if ($bit==1)  
            return $this->binDecode($binary, $mid, $max);  
        else  
            return $this->binDecode($binary, $min, $mid);  
    }  
}  
?>  
测试实例
[php] view plain copy
<?php  
require_once("db_config.php");  
require_once('geohash.class.php');  
$geohash=new Geohash;  
//经纬度转换成Geohash  
//获取附近的信息  

$n_latitude  =  34.236080797698;  
$n_longitude = 109.0145193757;  
echo "当前位置为：经度108.7455，纬度34.3608<br/><br/>  
以下网点离我最近：";  
//开始  
$b_time = microtime(true);  
//方案A，直接利用数据库存储函数，遍历排序  
//方案B geohash求出附近，然后排序  
//当前 geohash值  
 $n_geohash = $geohash->encode($n_latitude,$n_longitude);  

//附近  
$n = 3;  
$like_geohash = substr($n_geohash, 0, $n);  
$sql = 'select * from retailersinfotable where geohash like "'.$like_geohash.'%"';  

$query = mysql_query($sql);  
    if(mysql_num_rows($query))  
    {  
        while($row=mysql_fetch_array($query))  
        {  
            $data[] = array (  
                            "latitude" =>$row["Latitude"],  
                            "longitude"=>$row["Longitude"],  
                            "name"     =>$row["RetailersName"],  
                    );  
        }  
}  

//算出实际距离  
 foreach($data as $key=>$val)  
{  
    $distance = getDistance($n_latitude,$n_longitude,$val['latitude'],$val['longitude']);  
    $data[$key]['distance'] = $distance;  
    //排序列  
    $sortdistance[$key] = $distance;  
}   
 //距离排序  
array_multisort($sortdistance,SORT_ASC,$data);  
$e_time = microtime(true);  
echo "（计算耗时：" ;  
echo $e_time - $b_time;   

echo "秒）<br/>";  
//var_dump($data);  
  foreach($data as $key=>$val)  
{  
    echo "</br>";  
    echo $val['distance']. " 米-------".$val['name'];  
}  

/** 
*  @desc 根据两点间的经纬度计算距离 
*  @param float $latitude 纬度值 
*  @param float $longitude 经度值 
*/  
function getDistance($latitude1, $longitude1, $latitude2, $longitude2)   
{  
    $earth_radius = 6371000;   //approximate radius of earth in meters  
    $dLat = deg2rad($latitude2 - $latitude1);  
    $dLon = deg2rad($longitude2 - $longitude1);  
     /* 
       Using the 
       Haversine formula 
       http://en.wikipedia.org/wiki/Haversine_formula 
       http://www.codecodex.com/wiki/Calculate_Distance_Between_Two_Points_on_a_Globe 
       验证：百度地图  http://developer.baidu.com/map/jsdemo.htm#a6_1 
       calculate the distance 
     */   
    $a = sin($dLat/2) * sin($dLat/2) + cos(deg2rad($latitude1)) * cos(deg2rad($latitude2)) * sin($dLon/2) * sin($dLon/2);  
    $c = 2 * asin(sqrt($a));  
    $d = $earth_radius * $c;  
    return round($d);   //四舍五入  
}  
?>  

```







http://www.zhongchuanxinxi.com/bp/bpg/?state=7b227063223a312c22706964223a383132312c22736964223a323031383336383833377d#