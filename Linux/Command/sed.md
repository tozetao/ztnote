## sed
sed是一种流编辑器，能够执行sed脚本来处理文本文件。

- 命令格式

  > sed  -options  sed_script  file

  sed_script是sed脚本，file是待操作的输入文件，options是sed命令选项



options是选项，用于控制sed脚本的执行结果，选项可以有：

- -n，-quiet，--slicent：仅显示script、处理过的效果，sec默认输出编辑行和未编辑行
- -i：将修改作用于文件
- -f：调用sed脚本文件处理文本信息，例如：sed -f script.sed input_file
- -e：添加sed脚本到程序的运行列表



## sed script

sed脚本是操作文件的逻辑，编写sed脚本时会先定位文本，再编写编辑文本的命令。



### 文本定位

文本定位是属于sed脚本的一部分，通过它来定位文本位置，sed浏览文件时，默认从第一行开始，
有俩种方式定位文本：
- 使用行号，可以是一个简单数字，或是一个行号范围
- 使用正则表达式，在俩个斜杠之间编写正则表达式

example：
```
sed -n "1p" demo.txt
# 匹配第一行内容，-n是仅返回脚本执行的结果，所以会输出在命令窗口

sed -n "1,3p" demo.txt
# 匹配某个区间的行数

sed -n "/[1-9]/p" demo.txt
# 匹配有数字1-9字符的行数
# 注：p是匹配行
```



### sed编辑命令

文本定位定位要操作的文本，编辑命令决定如何操作匹配的文本，例如替换、附加、删除等等；
sed编辑命令有：
- p：打印匹配行
- a\：在匹配行下面插入文本
- i\：在匹配行上面插入文本
- c\：把选定的行改为新的文本
- s：替换指定字符
- d：删除定位行
- =：打印匹配行号

example s：替换
```
sed "s/book/books/" demo.txt
# 将book替换成books，屏幕会输出未替换的行和已替换的行

sed -n "s/book/books/p" demo.txt
# 将book替换成books，屏幕会显示替换的行 

sed -i "s/book/books/p" demo.txt
# 直接编辑文件，将所有行的第一个book字符替换成books

sed -n -i "s/book/books/p" demo.txt
# 将所有行的第一个book字符替换成books，并将匹配的行输出到文件中
# 注：-i可以理解成将缓冲区的内容定向到文件中
```

exampel a\：追加(行下)
```
sed "/[1-9]/a\append" demo.txt
# 将append字符追加到匹配包含1-9数字的行下面

sed "1,3a\append" demo.txt
# 在1-3行的每行下面添加文本
```

example c\：
```
sed "1c\replace all" demo.txt
# 将第一行内容全部替换
```


### sed替换标记
- g：表示行内全面替换

example：
```
sed -i "s/book/books/g" demo.txt
# 直接编辑文件，将所有行的所有book字符替换成books
```


### sed元字符集



### sed文件
有的时候如果你的sed脚本过长，这时候可用将其编辑成一个sed文件来执行。
example：
```
#! /bin/sed -f

# script body
/[1-9]/a\
location /caipiao/test\
{\
	cotent\
}


# 假设脚本保存为demo.sed文件

demo.sed input_file.
```






### 运行模式

sed处理文本时，把当前处理的行存储在临时缓冲区中，称为“模式空间”（pattern space），接着用sed命令处理缓冲区中的内容，处理完成后，把缓冲区的内容送往屏幕。

接着处理下一行，这样不断重复，直到文件末尾。文件内容并没有改变，除非你使用重定向存储输出。







### Example

- sed脚本示例

  ```shell
  sed -i 's/\([0-9]\{1,3\}\.\)\{3\}[0-9]\{1,3\}/game_ip/' /file
  # 替换ip地址为game_ip
  
  sed -i "s/http:\/\/.*\.\(com\|net\|cn\)\/company_api/http:\/\/$address\/company_api/" file
  # 替换一个php脚本路径 
  ```

  

- sed脚本文件

  ```shell
  #!/bin/sed -f
  /enable-php.conf/a\
  \
  	location ~ /caipiao/(guess/)$\
  	{\
  	    index index.html;\
  	    rewrite ^ /company_api/index.php/guess last;\
  	    break;\
  	}
  
  s/try_files/#try_files/
  
  /try_files/a\
  \t    include pathinfo.conf;
  ```

  假设该sed脚本文件名为exec.sed，执行该sed脚本命令为：sed  -i  -f  ./exec.sed



- 批量跟新的shell脚本

  ```shell
  #!/bin/bash
  
  . /var/www/config/config.sh
  
  #路径
  local_path="/var/www/server_shell"
  server_path="/home/wwwroot/default"
  
  test_host="120.76.28.170"
  
  function batch_command(){
      rsync -vztprog --progress --exclude=server_shell/batch.sh -e ssh $local_path  root@$1:$server_path
      # ssh -i /var/www/shell/keys/key root@$1 "$server_path/server_shell/temp.sh"
      ssh -i /var/www/shell/keys/key root@$1 "sed -i 's/cow\/gg/pig\/gn/' /usr/local/nginx/conf/nginx.conf; sed -i 's/cow\/cpbsn/pig\/cpbsn/' /usr/local/nginx/conf/nginx.conf; service nginx restart;"
  
      
      # 安装cron服务，添加定时任务
      # ssh -i /var/www/shell/keys/key root@$1 "$server_path/server_shell/install_cron.sh;echo '$2 * * * * $server_path/server_shell/check_domains.sh' > /dev/null 2>&1 >> /var/spool/cron/root; service crond reload;"
      
      # 配置nginx文件
      #ssh -i /var/www/shell/keys/key root@$1 "$server_path/server_shell/change_location.sh"
      
  
  
      # url美化
      # ssh -i /var/www/shell/keys/key root@$1 "$server_path/server_shell/rewrite_company_api.sed -i /usr/local/nginx/conf/nginx.conf; service nginx restart;"
      # ssh -i /var/www/shell/keys/key root@$1 "sed -i \"s/= 'index.php'/= ''/\" $server_path/zc/company_api/application/config/config.php;"
      # ssh -i /var/www/shell/keys/key root@$1 "mysql -uroot -pzcdb@098 < /home/wwwroot/default/server_shell/my.sql;"
  }
  
  
  echo "1: 设置170服务器"
  echo "2: 设置指定服务器"
  echo "3: 设置所有服务器"
  
  read chose
  
  if [ $chose == "1" ]; 
  then
      echo $test_host
      batch_command $test_host 1
  elif [ $chose == "2" ]; 
  then
      echo '请输入ip地址'
      read target
      echo $target
      batch_command $target
  else
      size=${#web_servers[@]}
      for((i=0; i<$size; i++));
      do
  	echo ${web_servers[$i]}
  	batch_command ${web_servers[$i]} $i
      done
  fi
  ```

  