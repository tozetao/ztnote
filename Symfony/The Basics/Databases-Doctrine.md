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



### Migrations: Creating the Database Tables/Schema

Product类已经完全配置完毕，并且可用于保存到product表中。如果你仅定义了这个类，你的数据库实际上还没有这张表，你可以使用早已安装的DoctrineMigrationsBundle去添加这张表:

```
php bin/console make:migration
```

如果一切正常，你应会看到下面这些信息：

```
SUCCESS!

Next: Review the new migration "migrations/Version20211116204726.php"
Then: Run the migration with php bin/console doctrine:migrations:migrate
```

如果你打开这个文件，可以看到它包含了要更新数据库的SQL语句。去运行这些SQL语句来执行迁移:

```
php bin/console doctrine:migrations:migrate
```

该命令会执行在你的数据库中所有还未执行过的迁移文件。当你部署时你应该在生产环境中去运行该命令以保证你的生产数据库的最新状态。



### Migrations & Adding more Fields

但如果你需要给Product添加一个新的字段属性，比如描述，该怎么办？你可以编辑这个类来添加新的属性。但是，你也可以再次使用make:entity。

```
$ php bin/console make:entity

Class name of the entity to create or update
> Product

New property name (press <return> to stop adding fields):
> description

Field type (enter ? to see all types) [string]:
> text

Can this field be null in the database (nullable) (yes/no) [no]:
> no

New property name (press <return> to stop adding fields):
>
(press enter again to finish)
```

该操作会添加description属性和getDescription()、setDescription()方法。

```php
// src/Entity/Product.php
  // ...

  class Product
  {
      // ...

+     /**
+      * @ORM\Column(type="text")
+      */
+     private $description;

      // getDescription() & setDescription() were also added
  }
```

新属性已映射，但是不存在于product表中。对此可以再生成一个新迁移：

```
php bin/console make:migration
```

这时候生成的文件中的SQL语句应该是这样的：

```sql
ALTER TABLE product ADD description LONGTEXT NOT NULL
```

迁移系统是智能的。它将所有实体与数据库的当前状态进行比较，并生成同步它们所需的 SQL！像之前一样，执行你的迁移：

```shell
php bin/console doctrine:migrations:migrate
```

这会执行一个新的迁移，因为DoctrineMigrationsBundle知道第一个迁移操作早已执行。在幕后是管理migration_versions表来进行跟踪的。

每当你改变了你的schema，运行这俩个命令去生成迁移并执行它。当你部署时要提交这些迁移文件并执行它。

注：如果你更倾向于手动添加一个新的属性，make:entity命令也能为你生成getter和setter方法：

```
php bin/console make:entity --regenerate
```

如果你做了一些改变并想重新生成所有getter和setter方法，也可以传递 --overwrite选项。



### Persisting Objects to the Database

是时候保存对象到数据库中了! 让我们创建一个控制器来进行实验：

```
php bin/console make:controller ProductController
```

在控制器中你可以创建一个新的Product对象，使用它来保存数据并持久化到表中。

```php
// src/Controller/ProductController.php
namespace App\Controller;

// ...
use App\Entity\Product;
use Doctrine\Persistence\ManagerRegistry;
use Symfony\Component\HttpFoundation\Response;

class ProductController extends AbstractController
{
    /**
     * @Route("/product", name="create_product")
     */
    // ManagerRegistry $doctrine会告诉Symfony注入Doctrine服务到控制器中。
    public function createProduct(ManagerRegistry $doctrine): Response
    {
        // 获取Doctrine的实体管理对象，这是Doctrine中最重要的对象。它负责从数据库中获取或保存对象。
        $entityManager = $doctrine->getManager();

        $product = new Product();
        $product->setName('Keyboard');
        $product->setPrice(1999);
        $product->setDescription('Ergonomic and stylish!');

        // 告诉Doctrine去管理$product对象。该语句并不会立刻触发对数据库的操作。
        $entityManager->persist($product);

        // actually executes the queries (i.e. the INSERT query)
        // 当flush()被调用时，Doctrine会查看它所管理的所有对象，查看这些对象是否需要被持久化到数据库中。
        // 在这个例子中，$product对象的数据在数据库中并不存在，
        // 所以entity manager执行了一个插入操作，在产品表中创建了新的一行记录。
        $entityManager->flush();

        return new Response('Saved new product with id '.$product->getId());
    }
}
```

试试看！http://localhost/product，你刚在product表中创建了第一行数据。为了证明你可以执行查询数据库：

```
php bin/console dbal:run-sql 'SELECT * FROM product'
# on Windows systems not using Powershell, run this command instead:
# php bin/console dbal:run-sql "SELECT * FROM product"
```

注：如果flush()调用失败会抛出Doctrine\ORM\ORMException异常。详见[Transactions and Concurrency](https://www.doctrine-project.org/projects/doctrine-orm/en/current/reference/transactions-and-concurrency.html).

无论你是创建或者是更新对象，工作流程总是相似的：Doctrine是智能的，足以判断你的entity是要执行插入或者更新操作。





### Validating Objects

[The Symfony validator](https://symfony.com/doc/5.4/validation.html) 会重用Doctrine元数据去执行一些基本验证任务：

```php
// src/Controller/ProductController.php
namespace App\Controller;

use App\Entity\Product;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Validator\Validator\ValidatorInterface;
// ...

class ProductController extends AbstractController
{
    /**
     * @Route("/product", name="create_product")
     */
    public function createProduct(ValidatorInterface $validator): Response
    {
        $product = new Product();
        // This will trigger an error: the column isn't nullable in the database
        $product->setName(null);
        // This will trigger a type mismatch error: an integer is expected
        $product->setPrice('1999');

        // ...

        $errors = $validator->validate($product);
        if (count($errors) > 0) {
            return new Response((string) $errors, 400);
        }

        // ...
    }
}
```

尽管Product实体没有显示定义任何验证配置（[validation configuration](https://symfony.com/doc/5.4/validation.html)），但是Symfony会自省Doctrine映射配置，推断出一些验证规则。例如考虑name属性在数据库中不能为null，一个NotNull的约束会自动被添加到该属性中（如果它不包含该约束）。

下表总结了Doctrine元数据和Symfony自动添加相应的验证约束之间的映射关系。

| Doctrine attribute | Validation constraint                                        | Notes                                                        |
| :----------------- | :----------------------------------------------------------- | :----------------------------------------------------------- |
| nullable=false     | [NotNull](https://symfony.com/doc/5.4/reference/constraints/NotNull.html) | Requires installing the [PropertyInfo component](https://symfony.com/doc/5.4/components/property_info.html) |
| type               | [Type](https://symfony.com/doc/5.4/reference/constraints/Type.html) | Requires installing the [PropertyInfo component](https://symfony.com/doc/5.4/components/property_info.html) |
| unique=true        | [UniqueEntity](https://symfony.com/doc/5.4/reference/constraints/UniqueEntity.html) |                                                              |
| length             | [Length](https://symfony.com/doc/5.4/reference/constraints/Length.html) |                                                              |

因为Form compontent和API Platform内部使用了Validator component，所以你的form和web APIs也将自动受益于这些自动验证约束。

自动验证是非常好的特性可以提交你的工作效率，但是它不能完全替代验证配置。你仍然需要添加一些验证约束来确保用户提供的数据是正确的。



### Fetching Objects from the Database

从数据库中取回一个对象是更容易的事情。假设你希望能够在/product/1去查看你的新商品：

```php
// src/Controller/ProductController.php
namespace App\Controller;

use App\Entity\Product;
use Symfony\Component\HttpFoundation\Response;
// ...

class ProductController extends AbstractController
{
    /**
     * @Route("/product/{id}", name="product_show")
     */
    public function show(ManagerRegistry $doctrine, int $id): Response
    {
        $product = $doctrine->getRepository(Product::class)->find($id);

        if (!$product) {
            throw $this->createNotFoundException(
                'No product found for id '.$id
            );
        }

        return new Response('Check out this great product: '.$product->getName());

        // or render a template
        // in the template, print things with {{ product.name }}
        // return $this->render('product/show.html.twig', ['product' => $product]);
    }
}
```

另一种可能性是使用Symfony的Autowiring并由依赖注入容器注入的ProductRepository。

```php
// src/Controller/ProductController.php
namespace App\Controller;

use App\Entity\Product;
use App\Repository\ProductRepository;
use Symfony\Component\HttpFoundation\Response;
// ...

class ProductController extends AbstractController
{
    /**
     * @Route("/product/{id}", name="product_show")
     */
    public function show(int $id, ProductRepository $productRepository): Response
    {
        $product = $productRepository
            ->find($id);

        // ...
    }
}
```

试试看：localhost:8000/product/1

当你查询特定类型的对象时，你总会使用"Repository"。你可以把repository看作是一个PHP类，它唯一的工作就是帮你获取某类实体。

一旦你拥有了一个repository对象，你就有了一些辅助方法：

```php
$repository = $doctrine->getRepository(Product::class);

// look for a single Product by its primary key (usually "id")
$product = $repository->find($id);

// look for a single Product by name
$product = $repository->findOneBy(['name' => 'Keyboard']);
// or find by name and price
$product = $repository->findOneBy([
    'name' => 'Keyboard',
    'price' => 1999,
]);

// look for multiple Product objects matching the name, ordered by price
$products = $repository->findBy(
    ['name' => 'Keyboard'],
    ['price' => 'ASC']
);

// look for *all* Product objects
$products = $repository->findAll();
```

你可以为更复杂的查询添加自定义方法。稍后将在[数据库和 Doctrine ORM](https://symfony.com/doc/5.4/doctrine.html#doctrine-queries)部分详细介绍。

注：当渲染一个HTML页面时，页面底部的网络调试工具栏将显示查询的数量和执行的时间。如果数据库查询的数量过高，图标会变成黄色，表明有些东西可能不正确。点击该图标，可以打开Symfony分析器，看到所执行的确切查询。如果你没有看到网页调试工具栏，请运行以下命令安装Symfony分析器包： composer require --dev symfony/profiler-pack。

