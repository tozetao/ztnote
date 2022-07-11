归功于Doctrine，在应用中Symfony提供了你需要使用的数据库的所有工具，这是一组最好的处理数据库操作的PHP库。这些工具支持像MySQL和PostgreSQL这类的关系型数据库或像NongoDB这类NoSQL数据库。

数据库是一个广泛的话题，所以这篇文档将其分为3篇文章：

- 这篇文章展示了在Symfony应用中与关系型数据库交互的一种推荐方式。
- 如果你需要更低级别去访问和执行关系型数据库的原生SQL操作（比如PHP的PDO），阅读其他文章（[this other article](https://symfony.com/doc/5.4/doctrine/dbal.html) ）
- 如果你需要与MongoDB数据库交互，阅读[DoctrineMongoDBBundle docs](https://symfony.com/doc/current/bundles/DoctrineMongoDBBundle/index.html) 



### Installing Doctrine

首先，通过[Symfony pack](https://symfony.com/doc/5.4/setup.html#symfony-packs) orm和MakerBundle安装Doctrine支持，这有助于生成一些代码：

```
$ composer require symfony/orm-pack
$ composer require --dev symfony/maker-bundle
```



#### Configuring the Database

数据库连接信息被存储为一个名为DATABASE_URL的环境变量。对于开发者你可以在.env文件中找到或自定义该变量。：

```
# .env (or override DATABASE_URL in .env.local to avoid committing your changes)

# customize this line!
DATABASE_URL="mysql://db_user:db_password@127.0.0.1:3306/db_name?serverVersion=5.7"

# to use mariadb:
DATABASE_URL="mysql://db_user:db_password@127.0.0.1:3306/db_name?serverVersion=mariadb-10.5.8"

# to use sqlite:
# DATABASE_URL="sqlite:///%kernel.project_dir%/var/app.db"

# to use postgresql:
# DATABASE_URL="postgresql://db_user:db_password@127.0.0.1:5432/db_name?serverVersion=11&charset=utf8"

# to use oracle:
# DATABASE_URL="oci8://db_user:db_password@127.0.0.1:1521/db_name"
```

注：如果用户名、密码、主机或数据库名包含任何在URI中被认为是特殊字符（比如+、@、$、/等），你必须对其进行编码。保留字符的完整列表见RFC 3986，或使用urlencode函数对其进行编码。在这种情况下，你需要在config/packages/doctrine.yaml文件中移除resolve:前缀以便面错误：url:'%env(resolve:DATABASE_URL)%'



现在你的连接参数已设置，Doctrine可以根据db_name创建数据库：

```
php bin/console doctrine:database:create
```

在config/packages/doctrine.yaml中有许多你可以配置的选项，包括server_version，这些可能会影响Doctrine功能。

提示：可以运行 php bin/console doctrine命令去查看所有Doctrine命令。



Creating an Entity Class

假设你正在构建一个展示商品的应用。不用考虑Doctrine或数据库，你都知道需要一个Product对象去表示这些商品。

你可以使用make:entiry命令去创建这个类以及你需要的字段。该命令会询问你一些问题，像下面这样回答它们：

```
$ php bin/console make:entity

Class name of the entity to create or update:
> Product

New property name (press <return> to stop adding fields):
> name

Field type (enter ? to see all types) [string]:
> string

Field length [255]:
> 255

Can this field be null in the database (nullable) (yes/no) [no]:
> no

New property name (press <return> to stop adding fields):
> price

Field type (enter ? to see all types) [string]:
> integer

Can this field be null in the database (nullable) (yes/no) [no]:
> no

New property name (press <return> to stop adding fields):
>
(press enter again to finish)
```

现在你有一个新的/src/Entity/Product.php类：

```php
// src/Entity/Product.php
namespace App\Entity;

use App\Repository\ProductRepository;
use Doctrine\ORM\Mapping as ORM;

/**
 * @ORM\Entity(repositoryClass=ProductRepository::class)
 */
class Product
{
    /**
     * @ORM\Id()
     * @ORM\GeneratedValue()
     * @ORM\Column(type="integer")
     */
    private $id;

    /**
     * @ORM\Column(type="string", length=255)
     */
    private $name;

    /**
     * @ORM\Column(type="integer")
     */
    private $price;

    public function getId(): ?int
    {
        return $this->id;
    }

    // ... getter and setter methods
}
```

注：在MySQL5.6或更早版本，在使用InnoDB表时针对索引前缀会有一个767字节的限制。具有255个字符长度和utf8mb4编码的字符串列超出了该限制。这意味着任何字符串类型和unique=true的列必须将其最大长度设置为190。否则你将看到该错误: "[PDOException] SQLSTATE[42000]: Syntax error or access violation: 1071 Specified key was too long; max key length is 767 bytes".

这个类被称为Entity。很快你就可以将Product对象保存到product表或从product表中检索Product对象。Entity的每个属性都映射到该表中的一列。这通常时通过注解完成的，你在每个属性上可以看到@ORM\..的注解。

![](https://symfony.com/doc/5.4/_images/mapping_single_entity.png)

make:entity是使编码更轻松的工具，你只会在添加或移除字段、方法或者更新配置时才需要编码。

Doctrine支持各种类型字段，每个字段都有其自己的选项。要查看完整的列表，可以查阅[Doctrine's Mapping Types documentation](https://www.doctrine-project.org/projects/doctrine-orm/en/current/reference/basic-mapping.html)。如果你想要使用XML替代注解，在config/packages/doctrine.yaml文件中添加type:xml和dir: '%kernel.project_dir%/config/doctrine'到实体映射中。

注：不要使用保留的SQL关键字作为表或列的名称（例如group或user）。参见[Reserved SQL keywords documentation](https://www.doctrine-project.org/projects/doctrine-orm/en/current/reference/basic-mapping.html#quoting-reserved-words) ，了解如何避免这些关键字。或者在类上面用@ORM\Table(name="groups")改变表名，或者用name="group_name"选项配置列名。

