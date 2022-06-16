### Asynchronous and deferred operations

异步操作不是PHP原生的，但是仍然可能会有一些重的任务要同时执行，或者延迟知道它们完成为止。

为了使编写异步代码更容易，Magento Open Source提供了DeferredInterface，以便于异步操作一起使用。这允许客户端代码与异步操作一起工作，就像它与标准操作一样。



#### DeferredInterface

```php
interface DeferredInterface
{
    /**
     * @return mixed Value.
     * @throws \Throwable
     */
    public function get();

    public function isDone(): bool;
}
```

当客户端代码需要结果时，将调用get()方法来检索结果，isDone()可以用来查看代码是否已经完成。

有俩种类型的异步操作，其中DeferredInterface可以描述结果。

对于正在进行的异步操作，调用get()将会等待它们完成并返回其结果；对于延迟操作，get()实际将会启动操作，等待他完成然后返回结果。

有的时候开发人员需要对过长的异步操作进行更多的操作，这就是为什么会有一个扩展的延迟变体接口：Magento\Framework\Async\CancelableDeferredInterface：

```php
interface CancelableDeferredInterface extends DeferredInterface
{
    /**
     * @param bool $force Cancel operation even if it's already started.
     * @return void
     * @throws CancelingDeferredException When failed to cancel.
     */
    public function cancel(bool $force = false): void;

    /**
     * @return bool
     */
    public function isCancelled(): bool;
}
```

者的接口是为那些需要太长实践的操作准备的，可以取消该操作。



#### client code

假设服务A、服务B和服务C都执行异步操作，比如HTTP请求，那么客户端代码就会是这样。

```php
public function aMethod() {
    //Started executing 1st operation
    $operationA = $serviceA->executeOp();

    //Executing 2nd operations at the same time
    $operationB = $serviceB->executeOp2();

    //We need to wait for 1st operation to start operation #3
    $serviceC->executeOp3($operationA->get());

    //We don't have to wait for operation #2, let client code wait for it if it needs the result
    //Operation number #3 is being executed simultaneously with operation #2
    return $operationB;
}
```

而且没有看到回调！

有了延迟客户端，代码可以同时启动多个操作，等待需要的操作完成，并将承诺的结果传递给另一个方法。



#### ProxyDeferredFactory





#### Using DeferredInterface for background operations

如上所述，第一类异步操作是在后台执行的操作。DeferredInterface可以用于给客户端代码一个尚未收到结果（not-yet-received result）的承诺，并通过调用get()方法等待它。

来看一个示例：对多个产品发货：

```php
class DeferredShipment implements DeferredInterface
{
    private $request;

    private $done = false;

    private $trackingNumber;

    public function __construct(AsyncRequest $request)
    {
        $this->request = $request;
    }

    public function isDone() : bool
    {
        return $this->done;
    }

    public function get()
    {
        if (!$this->trackingNumber) {
            $this->request->wait();
            $this->trackingNumber = json_decode($this->request->getBody(), true)['tracking'];

            $this->done = true;
        }

        return $this->trackingNumber;
    }
}

class Shipping
{
    ....

    public function ship(array $products): array
    {
        $shipments = [];
        //Shipping simultaneously
        foreach ($products as $product) {
            $shipments[] = new DeferredShipment(
                $this->client->sendAsync(['id' => $product->getId()])
            );
        }

        return $shipments;
    }
}

class ShipController
{
    ....

    public function execute(Request $request): Response
    {
        $shipments = $this->shipping->ship($this->products->find($request->getParam('ids')));
        $trackingsNumbers = [];
        foreach ($shipments as $shipment) {
            $trackingsNumbers[] = $shipment->get();
        }

        return new Response(['trackings' => $trackingNumbers]);
    }
}
```

在这里同时发送多个发货请求，并在稍后收集结果。如果你不想编写自己的DeferredInterface实现，可以使用CallbackDeferred去提供回调，当get()被调用时该回调会别使用。



#### Using DeferredInterface for deferred operations

第二类异步操作是被推迟的操作，只有在绝对需要结果时才执行。

例如：

假设你正在为一个实体创建仓库（Repository），并且你有一个按照ID返回单个实体的方法。你希望在同个请求-响应过程中对请求多个实体的情况进行性能优化，这种情况你不会单独加载它们。

```php
class EntityRepository
{
    private $requestedEntityIds = [];

    private $identityMap = [];

    ...

    /**
     * @return Entity[]
     */
    public function findMultiple(array $ids): array
    {
        .....

        //Adding found entities to the identity map be able to find them by ID.
        //把找到的实体添加到identifyMap中，可以通过ID找到它们。
        foreach ($found as $entity) {
            $this->identityMap[$entity->getId()] = $entity;
        }

        ....
    }

    public function find(string $id): Entity
    {
        //Adding this ID to the list of previously requested IDs.
        $this->requestedEntityIds[] = $id;

        //Returning deferred that will find all requested entities
        //and return the one with $id
        return $this->proxyDeferredFactory->createFor(
            Entity::class,
            new CallbackDeferred(
                function () use ($id) {
                    if (empty($this->identityMap[$id])) {
                        $this->findMultiple($this->requestedEntityIds);
                        $this->requestedEntityIds = [];
                    }

                    return $this->identityMap[$id];
                }
            )
        );
    }

    ....
}

class EntitiesController
{
    ....

    public function execute(): Response
    {
        //No actual DB query issued
        $criteria1Id = $this->entityService->getEntityIdWithCriteria1();
        $criteria2Id = $this->entityService->getEntityIdWithCriteria2();
        $criteria1Entity = $this->entityRepo->find($criteria1Id);
        $criteria2Entity = $this->entityRepo->find($criteria2Id);

        //Querying the DB for both entities only when getStringValue() is called the 1st time.
        return new Response(
            [
                'criteria1' => $criteria1Entity->getStringValue(),
                'criteria2' => $criteria2Entity->getStringValue()
            ]
        );
    }
}

```





#### Examples

请参阅我们的异步HTTP客户端Magento\Framework\HTTP\AsyncClientInterface和Magento\Shipping\Model\Shipping以及各种Magento\Shipping\Model\Carrier\AbstractCarrierOnline实现，以了解如何使用DeferreInterface处理异步代码。
