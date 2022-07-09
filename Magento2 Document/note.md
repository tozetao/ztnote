Router

标准Route的Uri是由FrontName + Controler+ Action组成的。



> /etc/frontend/routes.xml

```xml
<?xml version="1.0" ?>
<config xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="urn:magento:framework:App/etc/routes.xsd">
    <router id="standard">
        <route frontName="helloworld" id="helloworld">
            <module name="Mageplaza_HelloWorld"/>
        </route>
    </router>
</config>
```

route的id是route id，它是唯一的，与FrontName相对应。在我目前的理解中，frontName对应一个模块，通过frontName找到对应的Module，再根据Controller、Action将请求分发到对应的Actin去处理。

比如URI：/helloworld/index/test，对应的代码文件为：/Mageplaza/HelloWorld/Controller/Index/Test.php.



