### ������������

- retry_after

  ��ѡ��ָ�����������ִ�ж������ͱ�����ʧ�����ԣ���Ӧ������Ϊ���ʱ����������Ӧ��ʱ�䡣



- queue

  Ĭ��ֵ��default��������ڷַ�һ������û��ָ��Ҫ����Ķ��У���ô�ᱻ�ŵ�����������queue����������Ķ��С�





### ��������

- sleep

  ����û��������ʱ��˯��ʱ�䣬������̴���������Ǻܽ�����������ô����ʱ�䲻Ҫ̫����

  ��λ�롣

- tries

  ����ʧ�ܵ�������Դ�����

  �����������ʱ�׳��쳣���������ͻ��Զ��ͷŻض��У����������������������ˡ�tries���Ƕ����������еĴ�����

  ���棺���û�����ø�ֵ������ͻ��������ԡ�

- delay

  һ������ʧ�ܺ��ӳٶ೤ʱ�����ԡ���λ���롣

- timeout

  ���̴���һ��������ʱ�䣬������ʱ�䣬���̾ͻᱻɱ����

- quiet

  ������κ����ݡ�



### timeout��retry_after

timeout�ǹ�����̵����ִ��ʱ�䣬retry_after�ǹ���������ִ��ʱ�䡣�����������ǲ�һ���ģ���ʱ����ͬʱ���ڡ�

timeoutӦ����Զ��retry_after�̼��룬�����ܹ���֤���н�������������ʧ������ǰ�ͱ�ɱ���ˣ���ֹ�������ظ�ִ�С�


����ϣ������ĳ������Դ����Ƿ���Ԥ�ڵģ�������������쳣����ʱ������������������ǵ�Ԥ�ڣ����Բ��������á�









### ����

ָ�����̵�����

> php artisan queue:work redis

ָ��redis���ӡ�

��Ӧ��������Ҫ����onConnection������



ָ�����̴������������

> php artisan queue:work redis --queue=emails

ָ������emails���͵Ķ��С�

��Ӧ��������Ҫ��������ʵ����onQueue������







### ԭ��

���������ʵ������ʹ��sleep��ʱ˯�ߣ�Ȼ���ȡ�洢��redis�еĶ��������ٴ�������

�����Ͼ�����ѯ��ȥ��ȡredis��ʵ�ֵģ���û��ʲô�ر�֮������Ȼʵ���Ͽ��ܸ���Щ��





���

- �ӳٶ��У�delay queue��

  ��delayed��ʶ���洢�����е���ʱ������ʱ����ָ�������������ӳٶ�������ִ�У���delayѡ��ָ����

- �������У�reserved queue��

  �䵱һ���м����С�

  �������������ȡ��һ������󣬸ı�����ĸ������ݣ����Դ�������ִ��ʱ�䣩���Ὣ�����ٴη���ö����С�

  �������������ִ�н����������ö����е�����

- ������У�default queue��

  �洢���н���Ҫ���������







��һ����

���ӳٶ��У�queue:default:delayed �����򼯺��л�ȡ���Դ����"�ӳ�����"��rpush��������У�queue:default���С�

ִ����䣺

> redis> eval "Lua�ű�" 2 queues:default:delayed queues:default ��ǰʱ���

lua�ű���

```lua
-- ��delay�����л�ȡ���й��ڵ�����
local val = redis.call('zrangebyscore', KEYS[1], '-inf', ARGV[1])

-- �����������ֵ�����ǽ��ӵ�һ��������ɾ����Щֵ��Ȼ��������100Ϊһ������ӵ�Ŀ������У��������Էǳ���ȫ�ؽ������ʵ�����ҵ�ƶ���Ŀ�������
if(next(val) ~= nil) then
    redis.call('zremrangebyrank', KEYS[1], 0, #val - 1)
 
    for i = 1, #val, 100 do
        redis.call('rpush', KEYS[2], unpack(val, i, math.min(i+99, #val)))
    end
end
```



�ڶ�����

��reserved�����л�ȡ�ѹ��ڵ�����rpush����������С�reserved������redis���򼯺ϡ�

> redis> eval "Lua�ű�" 2 queues:default:reserved queues:default ��ǰʱ���

ʹ�õ�lua�ű�ͬ��һ�������ű���ͬ��



��������

��������У�queue:default���л�ȡһ�������������ԣ�attempts���������������񱣴浽reserved�����У��������scoreֵ�ǵ�ǰʱ�� + �����ִ��ʱ�䣨��retry_afterѡ�����ã���

ִ����䣺

> redis> eval ��Lua�ű��� 2 queues:default queues:default:reserved ����ʱʱ���

lua�ű���

```lua
local job = redis.call('lpop', KEYS[1])
local reserved = false
 
if(job ~= false) then
    -- Increment the attempt count and place job on the reserved queue...
    reserved = cjson.decode(job)
    reserved['attempts'] = reserved['attempts'] + 1
    reserved = cjson.encode(reserved)
    redis.call('zadd', KEYS[2], ARGV[1], reserved)
end
 
return {job, reserved}
```



���Ĳ������������ִ�н���в�ͬ�Ĵ���Ч����

����ɹ�ִ�������ȡ�����񣬾ͽ��������reserved������ɾ������������ִ����ϡ�

ִ����䣺

> redis> zrem queue:default:reserved '��������'





�������ִ��ʧ�ܣ���������������

����ʧ�ܴ�����attempts��δ�ﵽ���Դ�����ֵ���Ὣ�������reserved������ɾ������ӵ�delayed�����У�score�Ǹ�������һ��ִ�е�ʱ���������ǰʱ��� + delay������

ִ����䣺

> redis> EVAL "Lua�ű�" 2 queues:default:delayed queues:default:reserved "ʧ�ܵ�����" �����ӳ�ִ�е�ʱ���

Lua�ű���

```lua
-- Remove the job from the current queue...
redis.call('zrem', KEYS[2], ARGV[1])
 
-- Add the job onto the \"delayed\" queue...
redis.call('zadd', KEYS[1], ARGV[2], ARGV[1])
 
return true
```



����ʧ�ܴ�������ָ����������ֵ���ͽ��������queue:default:reserved���Ƴ���

ִ����䣺

> ZREM queue:default:reserved "ʧ�ܵ�����"







�ܽ᣺

reserved���г䵱һ���м�㣬����default������ȡ��һ������ִ��ʱ����������Դ��������ִ��ʱ�䷢���ı䣬��ʱ���ǻ���reserved�����������Ѿ��������ı������

������һ�������ִ�гɹ�����ʧ�ܣ����Ὣ�������reserved������ɾ����











### ������redis����

> rpush key value [value...]

�����key���б�β����������ָ����ֵ�����key�������򴴽�һ���б��ٽ���push���������key����һ���б��᷵��һ������

����ֵ������push��������еĳ��ȡ�



> lpop key

�Ƴ�������key��Ӧ��list�еĵ�һ��Ԫ�ء�

����ֵ�����ص�һ��Ԫ�ص�ֵ�����ߵ�key������ʱ����nil��



> zadd key [NX|XX] [CH] [INCR] Score Number [...]

������Ԫ����ӵ���Ϊkey�������б��С�Ԫ���Ƿ�����Score������Ա��Number���ԣ�����ָ�������

���key�������򴴽�һ���µ������б������Ԫ�أ����key���ڣ���ӵĳ�Ա�����ھ���Ϊһ���µ�Ԫ�ؼ��룬���򽫻����ԭ�г�Ա�ķ�����

- Score�ܹ���ʾ��������Χ

  Redis���򼯺ϵķ���ʹ��˫����64λ���������ܹ���ʾ��������Χ��-2^53��+2^53

- �������

  ���򼯺��е�ÿ��Number����Ψһ�ģ������ǰ��յ�����˳������

- ��ͬ�����ĳ�Ԫ

  ��...

����ֵ�������������Ԫ�ط�������Ԫ�صĸ���������Ǹ��³�Ա�ķ������򷵻ط���ֵ���ַ����ͣ���



> zrem key member [member...]

�Ӽ�Ϊkey�������б���ɾ��ָ����Ԫ�ء�

����ֵ������ɾ���ĳ�Ա������



> zremrangebyran key start stop

�Ӽ�Ϊkey�����򼯺��У��Ƴ�ָ�����������ڵ�����Ԫ�ء�

start��stop���������������䣬�������Ǵ�0��ʼ������start = 0��stop = 1����ʾ�Ƴ���һ�����ڶ���Ԫ�ء�

��Щ����Ҳ�����Ǹ�ֵ����ʾ�����������ʼ��������-1�Ƿ�����ߵ�Ԫ�أ�-2�Ƿ����ڶ��ߵ�Ԫ�ء�

����ֵ������ɾ����Ԫ�ظ�����



> zrangebyscore key min max [withscores] [limit offset count]

��ȡkey��Ӧ�����򼯺���score��min��max֮��ĳ�Ա������min��max�ĳ�Ա����min��max������-inf��+inf�������Ϳ����ڲ�֪�����򼯺�����ͷ�������߷�����ʱ��ʹ��zrangebyscore��

Ĭ�������������Ǳ����䣬Ҳ����ͨ��������ǰ��ӣ� ��������ʾ�����䣨С�ڻ���ڣ���

����ֵ��Ĭ�Ϸ��������еĳ�Ա�����withscores�������ڣ����᷵�س�Ա�ķ�����













### ���´���

- ���뷢���仯

  �����Ĵ��뷢���˱仯����Ҫ�������������´��롣

  ����˵��Ŀ�����ݿ������ʱ������ô��������Ƶ����ݿ����Ҫ���и��£�����Ҫ�������̡�

- �ӽ������÷����仯

  supervisord���ӽ������÷����仯Ҳ��Ҫ������







