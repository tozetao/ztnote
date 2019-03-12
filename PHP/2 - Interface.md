接口是对象与对象之间的约定，一个对象不是依赖另外一个对象的身份，而是依赖另一个对象的功能。

简单的说编写的代码如果依赖于对象，那么代码的功能就被限定了，而如果依赖的是接口，那么代码的功能就是动态的。



example：

假设需求是从不同的源来收集文本，可以是远程URL读取HTML，可以读取流资源或者收集终端命令。

不同的源收集数据的方式会有不同的实现，然后使用者不关心实现细节，只关心收集的数据，因此可以定义收集数据的接口来进行约束。

```php
interface Documentable
{
	public function getId();
	public function getContent();
}
```

然后由不同的源来实现该接口即可。

```php
class HtmlDocument implements Documentable
{
    public function getId();
    public function getContent();
}

// 流数据的处理类
class StreamDocument implements Documentable
{
    public function getId();
    public function getContent();
}
```

使用者只需要注入不同的源的实现，就可以实现读取各种源了

```php
class DocumentStore
{
    public $source;
    
    public function addDocument(Documentable $source) {
        $this->source = $source;
    }
    
    public function getDocument() {
        return $this->source->getContent();
    }
}

$store = new DocumentStore();
$store->addDocument(new StreamDocument());
echo $store->getContent();

$store->addDocument(new HtmlDocument());
echo $store->getContent();
```



