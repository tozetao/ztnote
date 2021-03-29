vim模式

- insert mode

- normal mode



### 基本操作

#### map

> imap

insert模式下的映射

> nmap

normal模式下的映射。



> no recursion

map这种映射可能产生递归映射，因此该命令可以用于禁止递归映射，缩写为：nore。

```ini
inoremap jk <Esc>
inoremap <Esc> <Nop>

inoremap <up> <Nop>
inoremap <Down> <Nop>
inoremap <Left> <Nop>
inoremap <Right> <Nop>

nnoremap <up> <Nop>
nnoremap <Down> <Nop>
nnoremap <Left> <Nop>
nnoremap <Right> <Nop>

nnoremap H ^
nnoremap L $
nnoremap ,h H
```





单词间的快速跳转

> w/W

移动到下一个单词开头，W是以空格作为分隔。

> b/B

回到上一个单词开头

> e/E

移动到下一个单词结尾。



> ,

 重复上次的跳转指令

> .

重复上次的修改指令





#### 页内操作

> / + 单词

通过搜索的方式来跳转。配合n可以搜索下一个单词



#### 行内操作

> d + i + 符号

d是delete，i是inside，加上符号表示在某对符号内删除。



> f + 字符

用于行内搜索，配合；可以搜索下一个字符





### vim配置

vim配置步骤

1. 安装gnu global
2. 安装vim-plug插件管理器，接着执行vimrc的插件安装
3. 安装ctags

4. 调整主题：https://my.oschina.net/u/2401546/blog/895053







### gnu global

一个源代码tag（标记）系统。tag（标记）可以是函数、宏、结构、类。gnu global提供的命令有：

- global：用于查询。

- gtags：生成索引文件。

- gtags-cscope与cscope一样的界面。

后续的使用再慢慢研究。





vim-gutentags插件：用于自动生成tags。

gutentags_plug插件：实现数据库自动切换，该插件目前提供的快捷键：

- <leader>cg - 查看光标下符号的定义
- <leader>cs - 查看光标下符号的引用
- <leader>cc - 查看有哪些函数调用了该函数
- <leader>cf - 查找光标下的文件
- <leader>ci - 查找哪些文件 include 了本文件

上面的键映射的是GscopeFind命令，查询到的结果会显示在quickfix窗口中。



> :GscopeFind {querytype} {name}

执行cscope搜索并会搜索前处理数据库切换。{querytype}对应实际的cscope行号以及默认的nvi命令：

- 0 or s：查找引用，比如某个函数在哪里被调用了。
- 1 or g：查找定义
- 2 or d：查找此函数调用哪些函数
- 3 or c：查找哪些函数调用了该函数
- 4 or t：查找该文本字符串
- 6 or e：以egrep pattern寻找
- 7 or f：查看该文件
- 8 or i：查找哪些文件包含了本文件
- 9 or a：查找该符号被赋值的地方





quickfix相关快捷键：

- copen
- cclose
- cn：下一项
- cp：上一项
- cl：列出查询到的相关项
- ccN：到列表中第N个符号处





example：

- 通过GScopeFind寻找到函数的定义。
- 通过quickfix选择，vim会根据选项显示不同的函数实现。

ctrl+o，返回到调用函数代码处。















### 问题

cscope、gtags-cscope有什么作用？



待解决的问题：

括号自动匹配

怎么跳转到行首

quickfix的使用

编译运行

动态检查

查找到一个tag后，如果在新窗口打开？





### universal-ctags

安装：

```ini
$ sudo apt install autoconf
$ cd /tmp
$ git clone https://github.com/universal-ctags/ctags
$ cd ctags
$ sudo apt install \
    gcc make \
    pkg-config autoconf automake \
    python3-docutils \
    libseccomp-dev \
    libjansson-dev \
    libyaml-dev \
    libxml2-dev
$ ./autogen.sh
$ ./configure --prefix=/opt/software/universal-ctags  # 安装路径可以况调整。
$ make -j8
$ sudo make install


# 创建ctags链接
$ sudo ln -s /opt/software/universal-ctags.ac/bin/ctags /usr/bin/ctags  #注意ctags的安装文件夹名称

# vim设置ctags
set tags=tags
set autochdir
```









### nerdtree

一个目录树插件：https://github.com/preservim/nerdtree

在目录树之间怎么快速切换?



tab切换

> g+t

向右切换tab

> g+T

向左切换tab







### ctrlp

一个查找文件的插件：https://github.com/ctrlpvim/ctrlp.vim

> ctrl + p

模糊搜索当前目录下的所有文件，可绑定快捷键。



搜索框出来后的操作：

> ctrl + j/k

上下选择文件

> ctrl + t

在tab中打开文件。

> ctrl + x

在当前窗口水平分屏打开文件

> ctrl + v

垂直分屏打开文件









### gutentags



编辑情况下怎么进行选中和复制

逗号和引号问题

打开一个工程目录并切换











let与set的区别

set用于设置选项，let用于设置变量。

- b:

  当前缓冲区变量

- w:

  当前窗口变量

- g:

  全局变量

- l:

  本地函数

- 其他略...