#### CGI

CGI，全称是公共网关接口（Common Gateway Interface），它是HTTP服务器与机器上的程序进行通讯时的一种规范，CGI程序可以用任何一种语言实现，只要有输入、输出和环境变量。

CGI保证了web服务器传递过来的数据是标准格式的，web服务器只是内容的分发者，而不是处理者。

例如Nginx服务器，假设某个请求是/index.php，web服务器根据配置文件知道要将该请求分发给php解析器，在转发给php解析器的时候，要传递什么数据，传递数据的格式，这些都是CIG规定的。

php解析器也就是CIG程序在收到数据后，会解析php.ini文件，初始化环境处理请求，再以CGI规定的格式返回处理的结果，这大体是web服务器与php-cgi解析器的交互过程。



CGI执行流程演示：

```c
//CGI程序，/usr/local/cgi/helo
printf("Content-Type:text/html; charset=UTF-8");
printf("\n\n");
printf("hello world");
```

- 假设web服务器把 http://localhost/hell 的请求转给/usr/local/cgi/helo处理，这时会启用一个进程执行helo程序，helo程序执行完后会输出结果
- web服务器在拿到结果后，会把helo程序的输出结果添加其他http头元素，拼接完整的响应报文，最后把结果发送给浏览器，至此一次CGI的http请求就完成。



CGI性能问题

- PHP-CGI执行时，每次都会解析php.ini文件，初始化执行环境，标准的CGI对每个请求都会执行这些步骤，因此很耗费资源，造成性能消耗




#### fastCGI

fastcig是提高CGI性能的一种实现，避免php-cgi程序处理每次处理请求的初始化过程造成的资源浪费。

fastcgi会创建一个master进程负责php解析器的配置文件加载和解析、初始化环境，同时会创建多个worker子进程，当请求过来时，master进程会传递给worker进程处理，这样就避免了多次初始化所耗费的资源了，master也可以根据配置文件决定启动几个worker等待处理，这便是fastcgi对进程的管理了。

fastCGI是一个CGI进程管理容器，原理如下：

- fastCGI会启动一个master进程，master进程负责PHP-CGI程序的php.ini解析，执行环境的初始化，同时fastCGI启动多个worker进程。
- 当请求过来时，master进程会将请求转发给worker进程处理，这样就避免了CGI程序初始化的资源浪费问题




#### PHP-FPM

PHP-FPM是对fastCGI的一个实现，负责管理⼀个进程池，来处理来⾃Web服务器的请求。

PHP的解释器是php-cgi， 它只是个CGI程序，只能解析请求返回结果，不会进程管理，所以就出现了⼀些能够调度php-cgi进程的程序，而PHP-fpm就是针对于PHP的Fastcgi的⼀种实现。

note：⽬前PHP-FPM是内置于PHP的。



#### Web Server与PHP的交互

- PHP作为模块来使用

  如果是Apache服务器，PHP会被编译为一个模块给Apache使用；

  一般Apache是多进程模式，启动后会fork出多个子进程，每个进程的内存空间独立，且每个子进程都会经过开始阶段和结束阶段，开始阶段只会在fork时执行，结束阶段只会在进程结束时执行。

  在整个进程的生命周期可能会处理多个请求。

- PHP作为服务来使用

  如果是Nginx服务器，PHP是作为服务常驻在内存中，为Nginx提供服务。

  nginx与php-fpm分别是独立运行的进程，它们之间是通过php-cgi协议来进行通讯的。

php-fpm与mod_php的区别

nginx是异构非阻塞模型，每个worker可以同时处理若干请求；apache是一个阻塞模型，通过fork子线程对应处理某一个请求。

php-fpm是一个独立的服务存在的，nginx只需要请求php-fpm即可， php-fpm的运行权限不会受到nginx的影响；而mod_php会被加载到apache的子线程中去，权限是继承自apache的，从这个角度看php-fpm的自由度高。

性能上很难说明倆者的好坏，因为它们都无法改变php的运行方式。