## 随机进程

sys_rand.erl是随机数进程的实现。



### 进程状态

```erlang
-record(state, {
    seed
}).
```

sys_rand进程状态。



### 初始化

sys_rand进程初始化并没有任何处理。



### get_seed

返回一个随机数组合作为其他进程的随机种子。

```erlang
handle_call(get_seed, _From, State) ->
    %% 使用State的seed来初始化随机种子
    case State#state.seed of
        undefined ->
            rand:seed(exs1024, erlang:timestamp());
        S ->
            rand:seed(exs1024, S)
	end,
    %% 产生一个随机数组合
    Seed = {rand:uniform(99999), rand:uniform(999999), rand:uniform(999999)},
    {reply, Seed, State#state{seed = Seed}}.
```

进程的消息是有序的，使用上一次生成的随机种子来作为本次的随机种子，可以保证每一次生成的随机种子是不同的。





## 方法

### rand/2

生成一个Min到Max之间的随机整数

```erlang
%% 生成一个Min到Max之间的随机整数
rand(Min, Min) ->
    Min;
rand(Min, Max) ->
    case get('#rand_seed') of
        undefined ->
            RandSeed = get_seed(),
            rand:seed(exs1024, RandSeed),
            put('#rand_seed', RandSeed);
        _ ->
            skip
	end,
    %% N = Max - Min + 1，计算Min到Max一共有多少个数
    %% R = rand:uniform(N)，生成1-N之间的随机整数
    %% 为了得到Min到Max之间的随机整数，因此要将R加上Min - 1
	M = Min - 1,
    rand:uniform(abs(Max - M)) + M.
```



### rand/1

生成一个1到Max之间的随机整数

```erlang
%% 生成一个1到Max之间的随机整数
rand(Max) ->
    rand(1, Max).
```





### rand_list/2

从List中，根据各项的权重值来随机取出一项。

```erlang
rand_list([], _Pos) -> undefined;
rand_list([I], _Pos) -> I;
rand_list(List, Pos) ->
    %% 计算出List的总权重
    Sum = lists:sum([element(Pos, I) || I <- List]),
    
    %% 随机出1-Sum之间的值
    RandVal = rand(Sum),
    
    %% 将List所有项的权重当成一个个段连起来，RandVal所对应的段就是我们随机出来的项
    get_rand_tuple(List, Pos, RandVal).

get_rand_tuple([H | T], Pos, RandVal) ->
    Rand = element(Pos, H),
    case RandVal =< Rand of
        true -> H;
        false -> get_rand_tuple(T, Pos, RandVal - Rand)
    end.
```



```erlang
element(N, Tuple) -> term()
```

返回元组里面第N个元素，编号从1开始。