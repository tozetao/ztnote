在开发应用时，一般会有个约定：不要信任任何来自不受自己控制的数据源的数据，例如：

- $_GET
- $_POST
- $_REQUEST
- $_COOKIE
- php://stdin
- php://input
- file_get_contents()
- 远程数据库
- 远程API
- 来自客户端的数据

## SQL注入

### SQL注入场景
一般通过拼接的方式来拼写SQL语句在不过滤的情况下是很危险的，初级程序员可能会这么写代码：
```
$sql = sprintf('update user set pwd = '%s' where id = %d', $password, $int);
echo $sql;
```

上述的风险是很大的，攻击者可以这样注入攻击：

```
POST /user?id=1 http/1.1
Content-length:17
Content-type: application/x-www-form-urlencoded

pwd=123';--
```
mysql执行的sql语句是：update user set pwd = '123';--' where id = 1，这样子就将所有用户的密码都改为123了。

### SQL注入防范
1. 转义MySQL特殊字符
2. 使用PDO


## 数据验证
与过滤不同，验证数据只是为了确定用户输入的数据是否符合我们预期需要的，例如参数是否非空？是否满足某种格式？

相关函数
```
filter_var()
	php提供的验证函数，可用于验证变量是否url、email、

filter_input()

```


### 作业 ###
- xss过滤方法
- 验证输入过滤类
- pdo demo
- mysqli demo
- sql过滤类
- 框架与过滤的整合