假设在项目中划分出一个代理模块，它的目录结构是这样的：

```
-- Agent
 |-- Repository
 |-- Biz
 |-- Validator
 |-- Provider
 |-- Event
 |-- Qunue
 |-- Agent.php
```

原有的MVC模式不是能够很好的对代码进行划分和抽象，建议将代码划分成不同的模块来进行管理。单独的模块下，可以根据代码的功能性来进行划分。

这样处理方式比起单独的MVC模式要好得多。我们是在原有的MVC模式下进行扩展，而不是取代它。

- 控制器
  
  控制器属于传输层，负责从网络中读取数据。传输层相关的代码是不能放到业务逻辑层的，比如传输层的Request、Response等对象就不能出现在业务逻辑层。

- Biz
  
  Biz目录是业务逻辑层，与业务逻辑有关的代码都放置在这里。

- Agent
  
  模块根目录，在Agent目录下可以直接防止该模块相关的实体对象。

我们来模拟下业务层的AgentQuery，它是一个代理查询器，负责处理代理搜索页面的请求。

```php
class AgentQuery
{
    public function __construct(AgentRepository $agentRepository)
    {
    }

// Biz层的参数必须携带类型提示，明确的参数类型能够防止非法输入数据。
// 弱类型的痛苦：
// 不同类型的参数在表达式中返回的结构是不确定的。
// 比如float与int进行比较，string与数字类型进行比较，对象或数组与数字类型比较。


    public function exec(
        User $operator,  string $account, int $pid,
        Datetime $start, Datetime $end, int page, int pageSize)
    {
        // 对于代理，它只能搜索自己的后裔代理。对于管理员，可以搜索所有代理
        $offset = ($page - 1) * $pageSize;

        // 管理员：直接调用queryAgent方法
        if ($operator->isAdministrator() {
            return $this->userRepository->findAgents($account, $pid, 
                $start, $end, $offset, $pageSize);
        }

        // 代理
        // 1. 先搜索出代理的后裔代理id列表
        // 2. 根据关键字进行模糊搜索
        // 3. 根据俩者做一个交集处理，再分页处理
    }
}

// 一般的后台设计，一个查询页面除了展示列表，还需要展示列表的总行数。也就是说我们的接口
// 除了返回列表数据，还是需要查询对应的总行数。甚至还有跟该查询相对应的统计数据。
// 在以前我是在业务层定义一个查询列表方法，一个统计查询行数方法，以前这种设计是为了考虑
// 代码的重用性。我觉得业务层就是对外提供服务的地方，它对外提供的服务是灵活的，
// 可被客户端（控制器）随意组合调用的。
// 但是实践中这种设计不好，为什么不好?
// 一是定义的这俩个方法都是调用Repository的查询列表和查询行数接口，传递大部分相同的参数
// 来返回结果。
// 另外俩个方法的处理逻辑都是相同的，分成俩个方法你会发现这些逻辑散落在不同方法中，当
// 业务逻辑发现变化时，你要做俩次改动，极其容易造成bug。

// 设计有个理念是职责单一化，一个类处理一件事情。那么方法也是同样的道理，业务层中
// 代理查询这个query方法，它就是处理代理的查询逻辑，所以我们把共同的逻辑写在exec方法
// 中，调用Repository提供的方法来组装出请求需要的数据即可。

// 思考下，假设现在代理查询界面除了显示代理列表和总记录数，还需要计算总代理的充值金额，
// 代码应该放到哪里处理比较好呢?
// 按照我以前的想法，肯定是在AgentQuery业务对象中再增加一个查询充值金额的方法，毕竟
// 为了重用查询充值服务嘛。
// 这样设计就错了，查询代理的充值金额，它的查询逻辑跟上面query方法的逻辑强相关，所以
// 也应该放到exec方法中去处理。

// 到了这里你会发现，大部分情况下重用的是Repository的方法，Biz层中反而重写服务的机会少的多，
// 如果你发现某个Biz的服务有大部分你可以重用的代码，应该从这个Biz中提取出来定义成
// 另外一个方法，共用相同的代码去对外提供服务（保留原服务方法不变，重新创建一个方法）。


// 按照现在这个项目的web_users表来设计接口
class AgentRepository
{
    // 获取一个代理的后裔代理id列表
    public function findDescendantIds(int $id);

    // 查询代理
    public function findAgents(string $account, int $pid, int $start,
        int $end, int $offset, int $limit);
}
// query: 查询，一般用于统计Repository。比如查询代理充值金额。
// find: 寻找、发现
// get: 获取
// create()
// update($id, $data)
// delete($id)
// logXX: 记录某类日志
```

创建代理示例：

```php
class Creator
{
    public function __construct(AgenteRepository $agentRepository)
    {

    }

    // 代理创建下级代理
    // $account, $password, $single_recharge_limit, $single_redeem_limit
    public function exec(User $operator, $account, $password,
            $singleRechargeLimit, $singleRedeemLimit, $attr)
    {
        // 计算代理等级
        $level = 1;
        if ($operator->isAgent()) {
            $level = $operator + 1;
        }
        // 创建代理
        $agent = $this->agentFactory->create($account, $password,
            $singleRechargeLimit, $singleRedeemLimit, $operator->id,
            $level, $attr);
        $agent = $this->agentRepository->create($agent);
        // 设置代理角色
        // ...

        // 将新代理挂载到当前代理的节点下
        $this->agentRepository->mount($agent->id);

        // 调用游戏服务器
    }
}

class AgentRepository
{
    // 将新的代理id添加到其所有祖先代理下面。所有的祖先代理指父级代理、父级代理的父级代理，一直到根代理
    public function mount(int $parentId, $newAgentId);

    // 插入一行代理记录, 同时在redis中创建创建一个sortedset，用于存储它的后裔代理。
    public function create(Agent $agent)
    {
        // insert into ....
        // 好蠢，根本不用去初始化sortedset。在挂载时，祖先代理直接使用sortedset来
        // 添加后裔代理即可。
    }
}
```

再来考虑之前项目的查询玩家接口，这个接口比较复杂。不仅根据使用人员查询出不同的数据，同时有个在线状态的选项，在线与不在线的查询逻辑也不同。

```php
class PlayerQuery
{

}
```
