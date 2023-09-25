# Users

A *User* is an access account with corresponding capabilities within the WordPress installation. Each WordPress user has, at the bare minimum, a username, password and email address.

用户是一个访问账户，在 WordPress 安装中具有相应的功能。每个WordPress用户至少有一个用户名、密码和电子邮件地址。

Once a user account is created, that user may log in using the WordPress Admin (or programmatically) to access WordPress functions and data. WordPress stores the Users in the `users` table.

一旦创建了用户账户，该用户就可以使用WordPress管理员（或程序）登录，访问WordPress的功能和数据。WordPress将用户存储在 "users "表中。



## [Roles and Capabilities](https://developer.wordpress.org/plugins/users/#roles-and-capabilities)

Users are assigned [roles](https://developer.wordpress.org/plugins/users/roles-and-capabilities/#roles), and each role has a set of [capabilities](https://developer.wordpress.org/plugins/users/roles-and-capabilities/#capabilities).

用户被赋予角色，每个角色都有各自的功能。

You can create new roles with their own set of capabilities. Custom capabilities can also be created and assigned to existing roles or new roles.

您可以创建带有自己功能集的新角色。也可以创建自定义功能，并将其分配给现有角色或新角色。

In WordPress, developers can take advantage of user roles to limit the set of actions an account can perform.

在 WordPress 中，开发人员可以利用用户角色来限制账户可以执行的操作集。



## [The Principle of Least Privileges](https://developer.wordpress.org/plugins/users/#the-principle-of-least-privileges)

最少权限原则

WordPress adheres to the principal of least privileges, the practice of giving a user *only* the privileges that are essential for performing the desired work. You should follow this lead when possible by creating roles where appropriate and checking capabilities before performing sensitive tasks.

WordPress 遵循最少权限原则，即只赋予用户执行所需工作所必需的权限。您应尽可能遵循这一原则，在适当的情况下创建角色，并在执行敏感任务前检查权限。