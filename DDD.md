实体（Entity）

通过唯一标识的对象，例如Order有orderId，User有userId。

核心特征：标识是唯一的，即使属性变化，只要ID不变，对象仍然代表同一个实体。



值对象（Value Object）

没有唯一标识的对象，通过属性定义，如地址（Address）。



### 聚合（Aggregate）

聚合是领域模型的一部分。

聚合是一组强关联对象的集合，包含一个聚合根（Aggreate Root）（通常是个实体）和其他对象（实体或值对象），聚合通过聚合根统一操作。

核心特征：聚合内的对象需要保持一致性（例如订单和订单项必须一起删除或者创建）。

核心规则：

1. 每个聚合必须有且仅有一个聚合根（Aggregate Root），即一个实体，比如Order。
2. 聚合内部可以包含其他值对象或仅属于该聚合的实体。例如OrderItem是Order的内部实体，但通常建议设计为值对象。
3. 不能直接引用其他聚合的实体对象，只能通过ID关联或保存必要的快照（值对象）。





比如Order是一个实体，同时也是聚合的根（Aggreate Root）。聚合可能包含多个实体和值对象，例如Order聚合可能包含Order实体（聚合根）和OrderItem（值对象）。

```php
class Order {
    private $orderId;  // 实体唯一标识
    private $items = [];  // 值对象集合（OrderItem）

    public function addItem(Product $product, $quantity) {
        $this->items[] = new OrderItem(
            $product->getId(),
            $product->getName(),
            $product->getPrice(),
            $quantity
        );
    }
}

// 值对象：订单项
class OrderItem {
    private $productId;
    private $productName;
    private $price;
    private $quantity;

    public function __construct($productId, $productName, $price, $quantity) {
        $this->productId = $productId;
        $this->productName = $productName;
        $this->price = $price;
        $this->quantity = $quantity;
    }
}
```





**聚合之间的引用应该通过ID来引用而不是直接的对象引用**

好处是可以避免跨聚合的强耦合，保持业务边界清晰，减少事务复杂度。

举个例子，比如用户在店铺下单，创建订单需要依赖店铺（Store）的一些信息。这里有俩种情况：

1. 依赖的Store信息仅是Order的静态记录（如店铺名称、地址），则可以将必要信息以值对象形式嵌入Order聚合，而非直接引用Store聚合。

```php
class Order {
	private $orderId;
    private $orderItems;
    //店铺信息
    private $storeInfo
}
class StoreInfo {
    private $name;
    private $address;
}
```

2. 如果订单的创建、修改需要强依赖店铺的实时状态（例如库存扣减、店铺促销规则），则Store需要作为聚合根，与Order分开管理。整体由Application Service来协调

```php
// 应用服务层：协调订单创建
class OrderApplicationService {
    public function createOrder($orderId, $storeId, $productIds, Address $address) {
        // 1. 获取 Store 的实时数据（如库存）
        $store = $this->storeRepository->findById($storeId);
        $products = $this->productRepository->findByIds($productIds);

        // 2. 执行业务校验（如库存检查）
        foreach ($products as $product) {
            if (!$store->hasEnoughStock($product)) {
                throw new OutOfStockException();
            }
        }

        // 3. 创建订单（仅关联 Store ID，保存店铺名称快照）
        $order = new Order(
            $orderId,
            $store->getId(),
            new StoreInfo($store->getName(), $store->getShippingPolicy()),
            $products,
            $address
        );

        // 4. 扣减库存（通过领域事件异步处理）
        $this->orderRepository->save($order);
        $this->eventDispatcher->dispatch(new OrderCreatedEvent($order));
    }
}

// 领域事件处理（库存扣减）
class OrderCreatedListener {
    public function handle(OrderCreatedEvent $event) {
        $store = $this->storeRepository->findById($event->getStoreId());
        foreach ($event->getProducts() as $product) {
            $store->decreaseStock($product);
        }
        $this->storeRepository->save($store);
    }
}
```







### 仓储（Repository）

管理聚合的持久化与检索。



### 领域服务（Domain Service）

定位：实现领域内的业务逻辑，不依赖外部系统。

职责：

- 实现领域内的业务逻辑：如校验、计算、状态转换等。
- 操作聚合根或实体

```php
// 示例1：订单价格计算
// 场景：订单总价需要根据促销规则、用户等级和商品组合动态计算。
class OrderPricingService {
    public function calculateTotalPrice(Order $order, User $user, Promotion $promotion): Money {
        $basePrice = $order->getBasePrice(); // 订单基础价格
        $discount = $promotion->calculateDiscount($user, $order); // 促销折扣
        $vipDiscount = $user->isVip() ? $basePrice->multiply(0.1) : Money::zero(); // VIP折扣
        return $basePrice->subtract($discount)->subtract($vipDiscount);
    }
}

// 库存预留
// 场景：下单时需要预留商品库存，防止超卖。
class InventoryReservationService {
    public function reserveProducts(Order $order): void {
        foreach ($order->getItems() as $item) {
            $product = $item->getProduct();
            $product->reserveStock($item->getQuantity()); // 预留库存
        }
    }
}

// 示例3：用户权限校验
// 场景：校验用户是否有权限执行特定操作（如删除订单）。
class OrderPermissionService {
    public function checkDeletePermission(User $user, Order $order): void {
        if ($user->isAdmin()) {
            return; // 管理员有权限
        }
        if ($order->getUserId() !== $user->getId()) {
            throw new PermissionDeniedException();
        }
    }
}
```



**领域服务是否需要操作仓储?**

不需要。领域服务应**只处理业务逻辑**，仓储的操作应由应用服务负责。例如，应用服务调用仓储获取数据后，将领域对象传递给领域服务处理：

```php
// 应用服务：协调仓储和领域服务
class OrderApplicationService {
    public function createOrder($userId, $productId) {
        // 1. 调用仓储获取数据
        $user = $this->userRepository->find($userId);
        $product = $this->productRepository->find($productId);

        // 2. 调用领域服务校验库存
        $this->inventoryService->validateStock($product);

        // 3. 创建订单并保存
        $order = new Order($user->getId(), $product->getId());
        $this->orderRepository->save($order);
    }
}

// 领域服务：仅处理业务逻辑
class InventoryService {
    public function validateStock(Product $product): void {
        if ($product->getStock() <= 0) {
            throw new OutOfStockException();
        }
    }
}
```





**领域服务能够接收其他领域的实体？**

核心原则：领域服务应避免依赖外部聚合根，因为这会破坏领域边界，导致耦合。比如：

```php
class OrderPricingService {
    // 直接依赖 User 和 Promotion 实体（聚合根）
    public function calculateTotalPrice(User $user, Promotion $promotion, Order $order) {
        // ...
    }
}
```

- 若 `User` 或 `Promotion` 的模型变更，`OrderPricingService` 可能被迫修改。
- 隐含强依赖关系，导致代码难以维护。



正确设计：将需要的数据抽象为值对象或原始类型，而非直接传递实体。

```php
// example1: 
class OrderPricingService {
    // 接受值对象或原始数据作为参数
    public function calculateTotalPrice(
        UserLevel $userLevel,  // 值对象：用户等级
        PromotionRule $promotionRule,  // 值对象：促销规则
        Money $basePrice  // 原始类型或值对象
    ): Money {
        // 计算逻辑...
    }
}

// 调用方的职责：
// 应用服务负责从其他领域获取数据，并转换为领域服务所需的格式：
class OrderApplicationService {
    public function calculateOrderPrice($userId, $promotionId) {
        // 1. 获取外部数据
        $user = $this->userRepository->find($userId);
        $promotion = $this->promotionRepository->find($promotionId);

        // 2. 转换为值对象
        $userLevel = new UserLevel($user->getLevel());
        $promotionRule = new PromotionRule($promotion->getDiscount());

        // 3. 调用领域服务
        return $this->pricingService->calculateTotalPrice(
            $userLevel,
            $promotionRule,
            $basePrice
        );
    }
}

// example2:
class OrderService {
    // 正确：接受与订单领域相关的参数（如用户状态、商品库存）
    public function validateOrder(UserStatus $userStatus, ProductStock $productStock) {
        if ($userStatus->isBlocked()) {
            throw new UserBlockedException();
        }
        if ($productStock->isOutOfStock()) {
            throw new OutOfStockException();
        }
    }
}

// 应用服务中调用领域服务
class OrderApplicationService {
    public function createOrder($userId, $productId) {
        $user = $this->userRepository->find($userId);
        $product = $this->productRepository->find($productId);

        // 将外部数据转换为领域服务所需的参数
        $userStatus = new UserStatus($user->isBlocked(), $user->getLevel());
        $productStock = new ProductStock($product->getStock());

        // 调用领域服务（仅传递必要数据）
        $this->orderService->validateOrder($userStatus, $productStock);

        // 创建订单...
    }
}
```





**什么情况允许传递其他领域的实体？**

条件：

- 这些实体属于同一界限上下文，（例如电商核心领域中的用户和促销），且业务高度耦合，可以传递实体。

- 需要确保需要其他领域的实体不影响其数据（例如订单中的促销规则是快照）。

注：谨慎考虑

```php
// 假设 User 和 Promotion 属于同一限界上下文
class OrderPricingService {
    // 允许接受同一限界上下文内的实体
    public function calculateTotalPrice(Order $order, Promotion $promotion) {
        // 直接使用 Promotion 的规则（需确保促销是订单的不可变快照）
        $discount = $promotion->calculateDiscount($order);
        return $order->getBasePrice()->subtract($discount);
    }
}
```





**实体VS值对象**

- 实体（entity）

  有唯一标识和声明周期，直接引用会导致领域耦合

- 值对象（Value Object）

  无唯一标识，可安全传递。如UserLevel、PromotionRule



**领域服务VS应用服务**

```
领域服务：
处理领域内的业务逻辑（如计算、校验、规则）。
不依赖外部系统（如数据库、HTTP 客户端）。
接收领域对象（实体、值对象）作为参数。
通常由应用服务调用。
示例：订单计价、库存预留、权限校验。

应用服务：
协调跨领域的协作（如调用仓储、发布事件）。
直接操作外部系统（如调用仓储、调用其他微服务）。
接收原始数据（如 DTO）并转换为领域对象。
作为用户界面或 API 层的入口。
示例：创建订单时调用仓储、发布事件、记录日志。
```









### 应用服务（Application Service）

定位：协调领域模型和外部系统（如数据库、HTTP请求）

职责：

- 调用仓储获取数据
- 调用领域服务（Domain Service）执行业务逻辑
- 发布领域事件（Domain Event

示例：跨聚合的协调方式

```php
class OrderApplicationService {
    public function createOrder($orderData) {
        // 1. 获取外部聚合的实时数据
        $product = $this->productRepository->find($orderData['productId']);
        
        // 2. 校验库存（依赖领域服务或应用服务）
        if ($product->getStock() < $orderData['quantity']) {
            throw new OutOfStockException();
        }

        // 3. 创建订单（保存快照）
        $order = new Order($orderData['id']);
        $order->addItem(
            $product->getId(),
            $product->getName(),
            $product->getPrice(),
            $orderData['quantity']
        );

        // 4. 保存订单
        $this->orderRepository->save($order);

        // 5. 发布领域事件（异步扣减库存）
        $this->eventDispatcher->dispatch(new OrderCreatedEvent(
            $product->getId(),
            $orderData['quantity']
        ));
    }
}

// 领域事件监听器（库存扣减）
class OrderCreatedListener {
    public function handle(OrderCreatedEvent $event) {
        $product = $this->productRepository->find($event->getProductId());
        $product->decreaseStock($event->getQuantity());
        $this->productRepository->save($product);
    }
}
```

总结：应用服务负责协调，而领域服务专注于业务逻辑，如果把应用服务合并到领域服务Service中可能导致领域服务承担过多职责，破坏分成架构。



### 领域（Domain）

软件要解决的核心业务问题。





### 界限上下文（Bounded Context）

定义明确的边界，每个上下文都有自己的领域模型和术语。