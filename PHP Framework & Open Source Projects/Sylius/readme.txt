安装碰到的问题：

- LNMP安装包安装的PHP为了安装，禁用了很多shell相关操作的函数，在php.ini配置文件，要允许执行proc_get_status等系列函数。

- 需要安装加密扩展，参考：
    http://www.manongjc.com/detail/29-jdjwxycutrdzixv.html
    https://paragonie.com/book/pecl-libsodium/read/00-intro.md#installing-libsodium
    注：安装扩展使用c99来进行编译：export CFLAGS='-std=c99'


Sylius需要的扩展：
extension=redis
extension=sodium
extension=redis
