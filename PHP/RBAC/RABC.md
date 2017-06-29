
## RABC
对于权限可以抽象为：who对what进行how操作，可以认为是某个主体在某个领域拥有某权限。

who：user、role
what：资源、模块、类、菜单、按钮
how：指的是权限

privilege表
- id
- master，对应who，即用户、角色、部门等
- master_value，对应who的id
- access，对应what，即资源
- access_value，对应资源的id
- operation，权限

一个菜单对应一个唯一的MenuNo，每一个叶子节点的菜单项对应一个页面（url），按钮归属于某个菜单项，就是菜单项页面上的按钮。


### 1. 菜单管理
界面以表格的形式显示菜单列表，要求对应有增删改的功能，
页面表格顶部是搜索栏，

menu表
- id
- url，对应的url，默认-
- name，菜单的名字
- ishide，是否隐藏
- pid，父级菜单id
- status，用于排序

button表
- id
- name
- url
- ishide，是否隐藏
- menu_id，所属菜单id

对于菜单与按钮的显示隐藏，由超级管理员管理，设置后其他人则无法观看。


开发进度：完成界面的开发
- 添加菜单：完成
- 更新菜单：完成
- 查询菜单：完成
- 删除菜单：

菜单、按钮都是资源，对于菜单和按钮的编辑就是对系统的权限的编辑，
例如新增一个用户管理菜单，系统将拥有一个用户管理的权限，设计的时候必须给系统内置一些权限，比如给系统内置菜单（权限）管理，否则将界面是没有菜单管理功能的。

### 2. 权限的授予
权限可以给部门、角色、用户进行授予。

角色列表、部门列表将会有分配权限的按钮，
分配时界面将所有的权限以一颗树显示出来，由管理员勾选授权，点击提交进行创建，便完成权限的授予。

角色管理开发：
- 新增角色
- 更新角色
- 角色列表
- 删除角色

### 3. 权限的验证
获取URI，匹配privilege表中的url字段，如果用户拥有该权限，那么则放过，否则禁止访问。


### 4. 侧边栏的生成
侧边栏会根据用户拥有的权限来进行生成，将查询出用户所拥有的权限，根据权限对应的菜单来进行生成

注：如果用户拥有某个权限，但是该权限（菜单）在系统中被删除了，那么界面是不会生成的。

缓存：第一次访问将为用户生成侧边栏的缓存，以后将优先读取该缓存，如果管理员重新分配权限后要自动刷新该用户的缓存。


## 表的创建
create table privilege(
	id int primary key auto_increment,
	master enum('role', 'department', 'user') not null,
	master_value int not null,
	access varchar(10) not null,
	access_value int not null,
	operation tinyint not null default 1
)engine=myisam,charset=utf8

create table menu(
	id int primary key auto_increment,
	url varchar(200) not null,
	name varchar(10) not null,
	ishide tinyint not null,
	pid int not null default 0,
	status middleint not null
)engine=myisam,charset=utf8

create table button()engine=myisam,charset=utf8

create table role(
	id int primary key auto_increment,
	name varchar(20) not null,
	description varchar(100) not null
)engine=myisam, charset=utf8
角色表，一个用户对应多个角色，一个角色对应多个用户，跟用户是多对多的关系


create table user_role(
	id int primary key auto_increment,
	user_id int not null,
	role_id int not null
)engine=myisam,charset=utf8


create table adminuser(
	id int primary key auto_increment,
	name varchar(20) not null,
	password varchar(100) not null,
	login_at timestamp(14) not null default 0
)engine=myisam,charset=utf8
后台用户表




## 一些理解
表单验证：
在网页前端中，是没有数字类型这个说法的，传递过来的都是字符串，0在客户端浏览器中也是字符串，所以从客户端传递过来的字段如果是空字符串，那么是无意义的，需要进行过滤。

url参数验证：
设置要要进行验证的参数的验证规则，再相应给客户端。




侧边栏菜单项：

商品模块
	商品列表：新增商品、删除商品、修改商品

一般侧边栏会有多个模块，每个模块下面会有多个菜单项，每个菜单项对应一个url页面，
把按钮归属于菜单项上，其实就是挂在一个页面上，例如增删改查按钮。

上面的例子中，商品列表就是菜单项，旗下有3个按钮功能，

把菜单权限分配给用户/角色，master是用户或角色类型，master_value是用户或角色的id，access是menu，acess_value是menu的id，operation可以是enable或disable，表示启用或者禁用。