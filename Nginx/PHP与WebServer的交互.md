#### CGI

CGI，全称是公共网关接口（Common Gateway Interface），它是HTTP服务器与机器上的程序进行通讯时的一种规范。

CGI程序是运行在网络服务器上的，CGI可以用任何一种语言实现，只要有输入、输出和环境变量，例如PHP/C/perl等。

CGI执行流程：

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

fastCGI是一个CGI进程管理容器，原理如下：

- fastCGI会启动一个master进程，master进程负责PHP-CGI程序的php.ini解析，执行环境的初始化，同时fastCGI启动多个worker进程。
- 当请求过来时，master进程会将请求转发给worker进程处理，这样就避免了CGI程序初始化的资源重复问题



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

  PHP-FPM会启动多个CGI进程，这是进程可以同时处理多个请求。

php-fpm与mod_php的区别

nginx是异构非阻塞模型，每个worker可以同时处理若干请求；apache是一个阻塞模型，通过fork子线程对应处理某一个请求。

php-fpm是一个独立的服务存在的，nginx只需要请求php-fpm即可， php-fpm的运行权限不会受到nginx的影响；而mod_php会被加载到apache的子线程中去，权限是继承自apache的，从这个角度看php-fpm的自由度高。

性能上很难说明倆者的好坏，因为它们都无法改变php的运行方式。