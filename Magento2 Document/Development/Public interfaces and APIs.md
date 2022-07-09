What is an interface?

public interface是一组第三方开发者可以调用、实现或构建为插件的代码。Adobe保证在没有重大版本更改的清空下，此代码不会再后续版本中更改。

模块的公告接口用@api注解标记。

第三方开发人员应只使用这些接口，即带有@api注解的接口。你可以使用其他接口，但这些接口可能会在后续的Adobe Commerce和Magento开源版本中被修改或删除。有关更多信息，请参阅向后兼容性。



> Example of public interface annotation

```php
<?php
/**
 * Copyright © Magento, Inc. All rights reserved.
 * See COPYING.txt for license details.
 */

namespace Magento\CatalogRule\Api;

/**
 * Interface CatalogRuleRepositoryInterface
 * @api
 * @since 100.1.0
 */
interface CatalogRuleRepositoryInterface
{
...

```



What is an API?

应用程序接口是一个模块向其他模块提供的一组接口和实现。

Example：一个接口的实现

The Magento_CatalogRule module. The Magento\CatalogRule\Api\CatalogRuleReporitoryInterface interface.

```php
<?php
/**
 * Copyright © Magento, Inc. All rights reserved.
 * See COPYING.txt for license details.
 */

namespace Magento\CatalogRule\Api;

use Magento\CatalogRule\Api\Data\RuleInterface;
use Magento\Framework\Exception\CouldNotDeleteException;
use Magento\Framework\Exception\CouldNotSaveException;
use Magento\Framework\Exception\NoSuchEntityException;

/**
 * Interface CatalogRuleRepositoryInterface
 * @api
 * @since 100.1.0
 */
interface CatalogRuleRepositoryInterface
{
    /**
     * @param RuleInterface $rule
     * @return RuleInterface
     * @throws CouldNotSaveException
     * @since 100.1.0
     */
    public function save(RuleInterface $rule): RuleInterface;

    /**
     * @param int $ruleId
     * @return RuleInterface
     * @throws NoSuchEntityException
     * @since 100.1.0
     */
    public function get(int $ruleId): RuleInterface;

    /**
     * @param RuleInterface $rule
     * @return bool
     * @throws CouldNotDeleteException
     * @since 100.1.0
     */
    public function delete(RuleInterface $rule): bool;

    /**
     * @param int $ruleId
     * @return bool
     * @throws CouldNotDeleteException
     * @since 100.1.0
     */
    public function deleteById(int $ruleId): bool;
}
```

一个接口的实现是再di.xml文件中用<preference/>声明。

```xml
<config xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="urn:magento:framework:ObjectManager/etc/config.xsd">
...
    <preference for="Magento\CatalogRule\Api\CatalogRuleRepositoryInterface" type="Magento\CatalogRule\Model\CatalogRuleRepository"/>
...
</config>
```

Magento\CatalogRule\Model\CatalogRuleRepository实现了CatalogRuleRepositoryInterface接口的默认方法：save，get，delete，deleteById。

```php
<?php
/**
 * Copyright © Magento, Inc. All rights reserved.
 * See COPYING.txt for license details.
 */

namespace Magento\CatalogRule\Model;

use Magento\CatalogRule\Api\Data;
use Magento\Framework\Exception\CouldNotDeleteException;
use Magento\Framework\Exception\CouldNotSaveException;
use Magento\Framework\Exception\NoSuchEntityException;
use Magento\Framework\Exception\ValidatorException;
use Magento\CatalogRule\Api\CatalogRuleRepositoryInterface;

class CatalogRuleRepository implements CatalogRuleRepositoryInterface
{
    ...

    /**
     * @inheritdoc
     */
    public function save(Data\RuleInterface $rule): Data\RuleInterface
    {
        ...
    }

    /**
     * @inheritdoc
     */
    public function get(int $ruleId): Data\RuleInterface
    {
        ...
    }

    /**
     * @inheritdoc
     */
    public function delete(Data\RuleInterface $rule): bool
    {
        ...
    }

    /**
     * @inheritdoc
     */
    public function deleteById(int $ruleId): bool
    {
        ...
    }
}
```



API types

下面的列表将被是为APIs：

- Directory structure（目录结构）
- Configuration files structure（配置文件结构）
- Events
- Client API
- Provider API (SPI)

目录结构和配置文件结构视为API类型的原因是扩展开发人员会使用它们。开发人员编写配置，并将其静态文件放在指定目录。因此，如果配置文件结构或目录结构在后续版本中发生更改，模块和扩展可能会中断。

