Magento是模块化的系统，使得第三方开发者能够自定义和重写应用程序框架的核心部分。然而这种灵活性是有代价的。业务逻辑往往会在系统的各层之间泄露，这表现为重复和不一致的代码。

商家可能不愿意升级Magento系统，因为他们购买的定制扩展可能与新版本不兼容。此外，第三方开发者可能发现很难跟踪和报告定制扩展对其他扩展的依赖性。

为了解决这些问题，Magento引入了服务契约。



> What is a service contract?

服务契约是为模块定义的一组PHP接口。服务契约包含维护数据完整性的数据接口，以及对服务请求者（如控制器、web services和其他模块）隐藏业务逻辑细节的服务接口。



> Benefits

服务契约增强了Magento的模块化。它们使第三方开发人员能够通过composer文件来显示系统的依赖性，从而保证了Magento系统个版本的兼容性。这种兼容性确保商家可以轻松升级Magento。这些服务契约确保了其他模块和第三方扩展可以实现定义良好，可维护性的API。此外，服务契约使得将服务配置为web API更简单。

数据实体是服务契约附带的一个好处。通常支持这些实体的数据库表可能很复杂。例如某些属性可能存储在EAV表中，因此可能一组MySQL表定义一个实体。

与底层的关系数据库Schema的数据模型（data model）相比，服务契约中的数据实体显示的数据模型要简单多了。最终你能够为不同的数据集合使用不同的存储技术。例如你可以使用NoSQL数据库来替代产品表。



Service contract design patterns

在编程社区中，设计模式是一种推荐的编写代码的方法，其中包括何时使用或不使用该模式。将设计模式视为有条件的最佳实践。

服务契约的设计模式告诉您要定义哪些类型的接口，以及如何以及在何处定义和实现这些接口。

注：服务契约数据接口现在是可变的。



Interface types and locations

服务契约必须定义数据接口（维护数据完整性）和服务接口（对服务请求者隐藏业务逻辑）。

数据接口定义了返回数据实体信息、返回搜索结果、设置验证规则和返回验证结果。你必须在模块的Api/data子目录中定义服务契约的数据接口。

服务接口包括Management、Reporitory和Metadata接口。你必须在模块的Api子目录中为服务契约定义服务接口。



Data interfaces

数据接口定义在模块的Api/Data子目录下。

例如，Customer模块的数据接口位于/app/code/Magento/Customer/Api/Data。

注：SimpleDataObjectConverter严格遵循Key（表的字段）的camel case到snake case的转换原则。你不应该在列名中的字符数组字符之间使用下划线（_）。例如，使用default_shipping1而不是default_shipping_1，因为数据接口方法defaultShipping1将被转换为default_shipping1。



Data search result interfaces

将搜索条件传递给getList()调用时，搜索结果接口将会作为搜索结果返回。（when you pass search criteria to a getList() call, a search results interface is returned with the search results.）

为了类型提示，你必须为每个数据实体定义一个接口。也就是说，CustomerSearchResultsInterface的getItems()方法将会返回一个CustomerInterface数据实体类型的数组。在GroupSearchResultInterface中，getItems()会返回一个GroupInterface数据实体的数组。



Service interfaces

服务接口包含几种子类型接口：

- Reporitory interfaces
- Management interfaces
- Metadata interfaces

关于文件命名标准，遵循PHP编码标准（https://developer.adobe.com/commerce/php/coding-standards/php/）。

一个模块的服务接口应该放置在Api顶级目录中。



Repository interfaces

Repository提供了对持久数据实体的访问。例如，Customer模块的持久数据实体包括Customer、Address和Group。因此Customer模块的Reporitory接口有：

- CustomerRepositoryInterface
- AddressRepositoryInterface
- GroupRepositoryInterface

Repository接口必须提供这些函数：

- save(data entity interface)

  如果未指定实体ID，则创建记录。如果指定了实体ID，则更新指定实体ID的记录。

- get(id)

  通过id执行一个数据库检索并返回一个数据实体接口。比如CustomerInterface和AddressInterface

- delete(data entity interface)

  删除一个实体，该实体包含一个key（ID）。

- deleteById(id)

  通过key（ID）删除一个实体。

每个数据实体都有相应的接口。因此在相应的接口中的getById()函数可以返回确切的类型。



Management interfaces

Management interfaces提供与reporitories无关的管理功能。例如：

- AccountManagementInterface

  定义了createAccount(), changePassword(), activate()和isEmailAvailable()函数。

- AddressManagementInterface

  定义验证一个地址的validate()函数。



Metadata interfaces

Metadata interfaces提供了检索元数据的方法，这些接口与repositories无关。例如：

- AttributeMetadataInterface

  提供customer元数据属性，并定义数据数组和方法的Key的常量。参阅（https://github.com/magento/magento2/blob/2.4/app/code/Magento/Customer/Api/Data/AttributeMetadataInterface.php）

- ProductMetadataInterface

  提供应用产品的元数据。定义了getVersion()、getEdition(), getName()方法

- AddressMetadataInterface

  检索关于customer地址元数据属性信息的接口。定义了常量ATTRIBUTE_SET_ID_ADDRESS、ENTITY_TYPE_ADDRESS、DATA_INTERFACE_NAME。