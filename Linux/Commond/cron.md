## cron调度服务
cron是系统调度进程，能够在无人干预情况时运行作业。cron进程是系统服务，可以用service命令来管理它。

crontab命令允许用户向cron进程提交、编辑、删除相应的作业，每个用户都可以编辑crontab文件，编写自己的调度信息，例如每分钟执行，每小时执行等。

### 1. cron config
/etc/cron.allow，/etc/cron.deny，这俩个配置文件管理系统用户执行cron服务的权限。
allow文件是允许用户使用cron服务，deny是禁止用户使用cron服务，allow的优先级高于deny，如果俩个文件同时存在，allow生效，deny不生效。

### 2. crontab
crontab格式：分<>小时<>日<>月<>星期<>命令，<>是空格的意思，用于区分。
- 分：0-59
- 小时：1-23
- 日：1-31
- 月：1-12
- 星期：0-6,0表示星期天
- 命令：shell脚本命令，linux命令
在crontab中，-表示时间范围，逗号","表示多项时间，*是连续时间段，例如：
```
0,15,30,45 * * * * echo 'is demo' > /root/test.txt
# 表示每天每15分钟执行命令

0,30 18-23 * * * /apps/bin/dbcheck.sh
# 每天的18点至23点，每个30分钟执行脚本

10 1 * * 6,0 /bin/find -name "core" -exec rm {} \;
# 每周六日的1:10执行find命令
``` 

crontab [-u user] -e -l -r
- -e：编辑crontab定时任务
- -l：查询crontab任务
- -r：删除当前所有用户的crontab任务
- [-u user]：可以不写，默认是当前用户

crontab filename，将一个crontab文件给cron进程处理。

## at命令
at命令允许用户想cron守护进程提交作业，使其在一段时间后运行。
at命令会保存当前的环境变量，ctontab只提供默认环境变量，at命令作业的所有输出将以电子邮件的形式发送到用户，当然可以重定向。

命令格式：at [-f script] [-m -l -r] [time] [date]
- -f：script 是所要提交的脚本或命令。
- -l：列出当前所有等待运行的作业，atq命令具有相同的作用。
- -r：清除作业，为了清除某个作业，还要提供相应的作业标识（ID）；有些UNIX变体只接受atrm作为清除命令。
- -m：作业完成后给用户发邮件。
- time：at命令的时间格式非常灵活；可以是H、HH . HHMM、HH : MM或H:M，其中H和M分别是小时和分钟。还可以使用a.m.或p.m.。
- date：日期格式可以是月份数或日期数，而且at命令还能够识别诸如today、tomorrow这样的词。

time example：
```
at 5.00am May23
at 11.20pm
at now +2 hour
at 9am tomorrow
at 15:00 May24
at now + 10 minutes
```

at命令可以用命令行方式和提示符方式来运行。



### &命令
在前台运行某个作业时，终端会被占用，而在后台运行时就不会，&命令可以将作业放在后台运行。
注1：&命令执行时，如果有标准输出是会输出在终端上的，建议使用文本重定向
注2：

格式：命令&

example：
```
find /etc -name "pass*" &
# 在后台执行这段命令

command >out.file 2>&1 &
# 2>&1表示所有的标准输出和错误输出都将被重定向到一个叫做out.file 的文件中。
```

### nohup
nohup是n ohang up，不挂起的意思，如果你要退出终端时运行一个进程，就使用该命令，在你离开终端后，该进程会一直运行到结束。

格式：nohup command &

nohup执行的命令默认输出在nohup.out文件中，除非你重定向。
