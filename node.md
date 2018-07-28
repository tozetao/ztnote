官网下载nodejs安装包

Configuration



配置模块的安装目录

> npm config set prefix ...
>
> npm config set cache ...

配置nodejs模块的安装目录与缓存目录，修改目录后需要配置系统环境变量，否则模块命令也找不到可执行的文件。

> NODE_MODULE = 模块安装目录
>
> path = nodejs_root
>
> path - nodejs_module_root



node、npm命令的执行，只需要在path环境变量中，配置nodejs的安装目录即可。

模块安装完毕后，可执行文件都是放在安装的模块目录下的，因此需要在path中继续配置模块的安装目录