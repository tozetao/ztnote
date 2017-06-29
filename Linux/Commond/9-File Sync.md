## ssh使用rsa建立安全验证

ssh-keygen，ssh命令，用于生成建立安全信任关系的证书

ssh-keygen -t rsa
注：passphrase：输入空，不要证书密码
上述命令将生成id_rsa私钥和id_rsa.pub公钥，存放在/root/.ssh目录下。




之后将公钥存放到server服务器的root/.ssh目录中，并将内容附加到authorized_keys文件中，如果没有则新建，再添加公钥内容。
私钥是存储在本地服务器.ssh目录中，这样建立ssh连接就不需要linux密码了。





## scp
代码的上传使用scp命令，讲本地机作为中转机器，有修改或新增的文件使用scp命令来上传即可。


## rsync








grant all privileges on *.* to root@'localhost' identified by 'zcdb@098';
flush privileges;
