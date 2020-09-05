### Make

make模块提供了一组类似于Unix类型的make函数。



```erlang
all() -> up_to_date ! error
all(Options) -> up_to_date ! error
```

- Options = [Option]

  Option = noexec | load | netload | (emake, Emake) | <compiler option>

这个函数决定要使用的编译选项和要编译的模块，首先寻找emake的make选项，如果没有则从一个名为E'makefile的文件中读取配置；如果没有找到这样的文件，编译的模块集默认为当前工作目录下的所有模块。

遍历模块集，然后重新编译的模块至少要符合以下条件：

- 没有对象文件。

- 或者源文件自上次编译后已被修改。

- 或者自上次编译源文件以来包含文件已被修改。

函数在执行的过程中，会打印它试图编译的每个模块的名称。如果某个模块的编译失败，make过程将停止并返回错误。

Option是make和编译选项的列表，存在以下make选项：

- noexec

  不执行模式，只需打印需要编译的每个模块的名称。

- load

  加载模式，会加载所有编译模块。

- netload

  节点加载模式，在所以已知节点上加载所有编译模块。

- {emake, Emake}

  显示指定配置，而不是读取Emakefile文件。

Options 中所有不属于 make 选项的项目都被认为是编译器选项，并按原样传递给 compile:file/2。Options的默认值是[]。





问题：make选项和编译选项有什么不同？







### Emakefile

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
















