<?php
/*
接口设计
--------
	login()
		用户登录
		http://www.oneym.cn/media/index.php/mobile/User/login/name/13570250744/pwd/123
		参数：
			name：用户名
			pwd：密码
	
	getRegSign()
		向指定手机发送验证码，并返回该验证码。
		http://www.oneym.cn/media/index.php/mobile/User/getRegSign/name/13570250744
		参数：
			name：用户名，即手机号码

	register()
		用户注册
		http://www.oneym.cn/media/index.php/mobile/User/register/name/13570250744/pwd/123/sign/6940
		参数：
			name：用户名
			sign：手机验证码
			pwd：密码
			push_id：推送标识
	
	findPwd
		http://www.oneym.cn/media/index.php/mobile/User/findPwd
		参数：
			name：用户名
			sign：验证码
			pwd：新的密码

	


相关参考点
-----------
1. 流媒体
	将连续的影像和声音经过压缩处理后放到服务器上，让用户能够一边下载一边观看。
	
	一个完成的流媒体系统需要3个部分组成：编码器、流服务器和播放器。
	编码器通过对内容来源(如MP3文件或者麦克风输入)进行编码，并将编码过的内容发送到服务器，流服务器再将他们传输给用户，这样用户的客户端播放器就可以在线播放了。


	推流
		将视频、音频以数据的形式发送到一个地址，这个地址是一个推流地址

	拉流
		观众获取一个地址，使用播放器播放，这个地址是一个拉流地址。
		拉流方式：播放器、浏览器、ffplay等。

		拉流地址格式一般有：RTMP、HLS、FLV

	原理
		简单原理就是后台不停生成H.264编码的mp4文件流，然后前端就像播放普通mp4文件一样使用video标签播放即可。


	ffmpeg的使用

405365176
a000000

huashenjishubu@163.com	
huashenhudong





 */