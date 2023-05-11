一些配置可以在kernel类本身（默认位于src/Kernel.php）完成。你可以通过覆盖父级Kernel类的特定方法来实现。



### Configuration

在以前的Symfony版本中，有另一个配置选项来定义 "内核名称"，这在使用具有多个内核的应用程序（[using applications with multiple kernels](https://symfony.com/doc/5.4/configuration/multiple_kernels.html)）时是很重要。如果你需要给你的内核一个唯一的ID，可以使用kernel.container_class参数或Kernel::getContainerClass()方法。



### Charset

type: string, default: UTF-8

这个选项定义了应用程序中使用的字符集。这个值通过kernel.charset配置参数和getCharset()方法公开。

要改变这个值，请覆盖getCharset()方法并返回另一个字符集。

```php
// src/Kernel.php
namespace App;

use Symfony\Component\HttpKernel\Kernel as BaseKernel;
// ...

class Kernel extends BaseKernel
{
    public function getCharset()
    {
        return 'ISO-8859-1';
    }
}
```

