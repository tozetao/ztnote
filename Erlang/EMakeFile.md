Emakefile

Emakefile负责编译erl程序，Emakefile的格式为：

```erlang
{Modules, Options}.
...
```

Modules是一个原子或原子列表，它可以是：

- 一个模块名，比如：'file1'
- 在其他目录中的模块名，比如：../foo/file3
- 用通配符指定的一组模块，比如：file\*
- 表示当前目录中所有模块的通配符可以用*
- 可以是上面所有选项的列表，比如：['file*', '../foo/file3', 'File4']

Options是编译器选项的列表。



Emakefile是从上到下读取的，如果一个模块匹配多个条目，则第一个条目有效。

example：

```erlang
{'file1', [debug_info, {i, "../foo"}]}.
{'*', [debug_info]}
```

在上面的配置中，file1模块会使用条目1进行编译，而当前目录中的所有其他文件都应该只使用debug_info标志编译。





一个Emakefile例子：

```erlang
{'src/*', [debug_info, {i, "include"}, {outdir, "ebin"}]}.
```

```shell
erl -make
```

执行添加make选项的erl命令时，将会在当前目录中寻找Emakefile文件，并根据内容进行构建。选项 i 表示相关文件在include文件夹中寻找，选项outdir表示编译好的文件会放置在ebin目录中。

在make时会查看输出目录是否已有编译文件，如果没有则进行编译；如果有对应的编译文件则检查文件时间，比较后决定是否进一步编译。如果源文件最新修改过了则进行编译。



make标准库模块也提供了加载函数。

```erlang
make:all().
make:all([load]).
```

这俩个函数会将编译好的模块重新载入erlang中。













erlang如何加载代码的?

erlang代码的载入是按需进行的。

当系统调用一个尚未加载的模块时，代码载入器会在当前载入路径的所有目录中进行寻找对应模块的编译文件，如果未按照对应的编译文件就会报一个异常；如果寻找对应的编译则会进行加载并使用。


















