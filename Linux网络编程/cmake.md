### CMake常用变量

- CMAKE_SOURCE_DIR

  指向工程根目录。

- PROJECT_SOURCE_DIR

  指向工程根目录。



- CMAKE_BINARY_DIR

  指向cmake命令的跟目录，所有二进制文件在这个文件夹内产生。

  如果是内部构建（in source），指的是工程根目录；如果是外部构建（out-of-source），指的是工程编译时发生的目录，一般是build目录。

- PROJECT_BINARY_DIR

  同上。



- CMAKE_CURRENT_SOURCE_DIR

  当前处理CMakeLists.txt所在的路径

- CMAKE_CURRENT_BINARY_DIR

  当前所在build目录。

- CMAKE_RUNTIME_OUTPUT_DIRECTORY 

  如果是out-of-source编译，它指的是target编译目录。





- CMAKE_RUNTIME_OUTPUT_DIRECTORY 

  可以用于指定可执行文件（bin）的执行目录。

  比如：set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin))

- CMAKE_LIBRARY_OUTPUT_DIRECTORY 

- CMAKE_ARCHIVE_OUTPUT_DIRECTORY 



这些都是cmake自动生成的变量。比如PROJECT_NAME这个变量，PROJECT_NAME是变量名，${PROJECT_NAME}是变量值。

如果想输出这些变量值可以使用message命令。



### 外部构建

外部构建指的是可以在文件系统上的任意位置创建一个构建的文件夹。所有临时构建和目标文件都在这个目录中，以保持源代码树的整洁。

文件树：

```
├── build
├── CMakeLists.txt
├── main.cpp
```

build文件夹下生成了许多二进制文件，如果要从头开始重新创建cmake环境，只需删除构建目录build，然后重新运行cmake即可。



### 包含目录

当你有需要包含的文件夹（比如包含头文件）时，可以使用target_include_directories命令让编译器知道它们。

使用该命令后在编译此目标时将会使用 -l 标志将这些目录添加到编译器中。



### 库文件

库文件一般对外提供了可以使用的接口。库文件可以分为静态库与动态库。

静态库的创建步骤：

- 创建库：使用add_library命令将源文件打包为静态库文件。
- 添加头文件所在目录：为静态库文件添加头文件所在目录（头文件定义接口供外部使用）。





链接库：如果目标（库或者可执行文件）要使用某个库，那么目标必须链接这个库文件。

add_executable()是链接源文件，而target_link_libraries()是链接库文件。



### cmake的构建级别

构建级别有：

- Release - 程序发行版本，不可以调式，占用体积小。

  在编译器中使用命令：-O3 -DNDEBUG可选择此版本。

- Debug - 调式版本，体积大。

  在编译器中使用命令：-g可选择此版本。

- MinSizeRel - 最小体积版本。

  在编译器中使用命令：-Os -DNDEBUG

- RelWithDebInfo - 既优化又能调式。

  在编译器中使用命令：-O2 -g -DNDEBUG可选择此版本



在命令行运行cmake时，可以使用cmake -D选项来设置构建级别。





### 编译标志

编译标志也叫做编译选项。可执行文件的生成离不开编译和链接。那么如何编译，比如编译时使用哪个C++标准？

这些编译设置都在CMAKE_CXX_FLAGS变量肿，C语言的编译选项时CMAKE_C_FLAGS。



```
target_compile_definitions(target scope items)
```

该命令会针对某个目标（target）设置编译标志。比如：

```
target_compile_definitions(compile_flags PRIVATE EX3)
```

这个示例的目标如果是一个库，编译器在编译目标时会添加定义-DEX3。如果scope是PUBLIC或INTERFACE，该定义-DEX3也将会包含在链接此目标的所有可执行文件肿。

在这里使用了PRIVATE，所以编译选项不会传递。













## 命令

### include_directories

```cmake
include_directories([AFTER|BEFORE] [SYSTEM] dir1 [dir2...])
```

添加编译器用于查找头文件的目录，如果文件路径是相对路径，则认为该路径是基于当前目录。

默认情况下路径是被追加到文件路径列表中，AFTER是追加，BEFORE是插入。







### add_executable

```cmake
add_executable(var1 var2)
```

将代码文件编译为可执行文件。var2可以是多个代码文件，var1是可执行文件名。



### target_include_directories

```cmake
target_include_directories(target 
	PRIVATE
	path)
```

当目标文件（target）需要包含目录时就可以使用该命令。

比如某个可执行文件需要包含头文件所在的目录时，target_include_directories命令允许在编译目标时，使用-l标志将指定目录添加到编译器中（比如-l /paths）。

- target

  taget可以是库文件、可执行文件。



scope是指库是否包含这个路径，以及链接了这个库的其他目标是否包含这个目录的3种可能。它们分别是：

- PRIVATE

  目录被添加到target（库）的包含路径中。

- PUBLIC

  目录既被添加到target的包含路径中，也被添加到链接了这个库的其他目标（库或者可执行文件）的包含路径中。

- INTERFACE

  目录没有被添加到目标的包含路径中，而是被添加到链接到这个库的其他目标的包含路径中。







### add_library

```cmake
add_library()
```

该指令用于从某些源文件创建一个库，默认生成在构建文件夹。

注：所谓的库文件就是提供了模块接口的代码文件。一般的可以将其链接到可执行文件使用。

example：

```cmake
add_library(hello_library STATIC
           src/Hello.cpp)
```



### target_link_libraries

```cmake
target_link_libraries(target scope lib)
```

将库文件链接到目标文件上。add_executable是链接源文件，而target_link_libraries是链接库文件。

该命令也有scope，关键字为：private、public、interface，与target_include_libraries()相同，但是意义不同。该命令的scope是指lib被链接时的范围。

如果scope是public，当工程目标（target）被link了，那么target_link_libraries指定的lib也会被链接。scope是private，那么指定的libs就不会被link，避免暴露出去。

比如我们的工程B是个dll，public链接了C、D。这个时候a.exe要链接B，那么它也会链接C、D。如果B是private链接了C和D，那么A链接B的时候是不会链接到C和D的。



这个scope主要是针对其他工程链接自己时，libs对外开放的一个设置。













### configure_file

```cmake
configure_file(<input> <output>
	...)
```

将<input>文件中的内容复制到<output>文件中。



该指令一般跟config.h文件有关系，回头再看。









### include

```cmake
include(<file|module> [OPTIONAL] [RESULT_VARIABLE])
```

从文件或模块中加载并运行cmake代码。

example：

```cmake
# Check C++11 or C++0x support
include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)
if(COMPILER_SUPPORTS_CXX11)
   set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
   add_definitions(-DCOMPILEDWITHC11)
   message(STATUS "Using flag -std=c++11.")
elseif(COMPILER_SUPPORTS_CXX0X)
   set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
   add_definitions(-DCOMPILEDWITHC0X)
   message(STATUS "Using flag -std=c++0x.")
else()
   message(FATAL_ERROR "The compiler ${CMAKE_CXX_COMPILER} has no C++11 support. Please use a different C++ compiler.")
endif()
```



### message

```cmake
message(STATUS var1 var2)
```

var1是前置文件，var2是变量值。

message可以输出变量值。



## set





## example

example：测试库文件包含头文件目录的scope为PRIVATE时，链接对象是否能够包含头文件。

CMakeLists.txt文件的配置：

```txt
cmake_minimum_required(VERSION 3.5)
project(network)

add_library(acceptor STATIC src/acceptor.c)
target_include_directories(acceptor PRIVATE ${PROJECT_SOURCE_DIR}/include)

add_executable(server src/server.c)
target_link_libraries(server PRIVATE acceptor)
```

目录树：

```c
├── CMakeLists.txt
├── include
│   └── acceptor.h
└── src
    ├── acceptor.c
    └── server.c
```

acceptor.h

```c
struct acceptor{
    int listen_port;
    int listen_fd;
};

struct acceptor* acceptor_init(int);
```

acceptor.c

```c
#include <acceptor.h>

struct acceptor *acceptor_init(int port) {
    struct acceptor *acceptor1 = malloc(sizeof(struct acceptor));
    acceptor1->listen_fd = 1000;
    acceptor1->listen_port = 80;
    return acceptor1;
}

```

server.c

```c
#include <acceptor.h>

int main(int c, char **v) {
    struct acceptor *a = acceptor_init(80);
    printf("%d %d\n", a->listen_fd, a->listen_port);
}
```

当库文件包含的头文件目录设为PRIVATE时，可执行文件server是无法包含include目录的头文件的。只有将scope改为PUBLIC才能编译通过。









a.h：引用b的对象。

b.h：引用a的对象。

server.c载入a