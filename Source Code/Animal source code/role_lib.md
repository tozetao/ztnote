## 协议





## 方法

### 扣除玩家资产

```erlang
%% 扣除指定资产
%% 金币
do_cost_coin(Role = #role{coin = Value}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = coin, num = Value - Cost}]}),
    {ok, Role#role{coin = Value - Cost}};
do_cost_coin(_, _) ->
    {false, ?error_coin}.

%% 钻石
do_cost_gold(Role = #role{gold = Value}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = gold, num = Value - Cost}]}),
    {ok, Role#role{gold = Value - Cost}};
do_cost_gold(_, _) ->
    {false, ?error_gold}.

%% 棒棒糖
do_cost_lollipop(Role = #role{item = Item = #role_item{lollipop = Value}}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = lollipop, num = Value - Cost}]}),
    {ok, Role#role{item = Item#role_item{lollipop = Value - Cost}}};
do_cost_lollipop(_, _) ->
    {false, ?error_item_num}.
```




