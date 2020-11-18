### CMake常用变量

- CMAKE_BINARY_DIR
- PROJECT_BINARY_DIR
- <project_name>_BINARY_DIR

这三个变量的值是一致的。

如果是in source编译，指的是工程根目录；如果是out-of-source编译，指的是工程编译时发生的目录。PROJECT_BINARY_DIR跟其他指令有点区别，但是可以认为它们是一致的。



- CMAKE_SOURCE_DIR
- PROJECT_SOURCE_DIR

这俩个变量指工程根目录。



- CMAKE_CURRENT_SOURCE_DIR

  指向正在处理的源目录（CMakeLists.txt所在目录）。这是一个指向源目录的完全路径，当前正被cmake处理。

- CMAKE_RUNTIME_OUTPUT_DIRECTORY 

  如果是out-of-source编译，它指的是target编译目录。





- CMAKE_RUNTIME_OUTPUT_DIRECTORY 

  可以用于指定可执行文件（bin）的执行目录。

  比如：set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin))

- CMAKE_LIBRARY_OUTPUT_DIRECTORY 

- CMAKE_ARCHIVE_OUTPUT_DIRECTORY 









 重点关键工程根目录与cmake运行目录。

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

当有需要包含文件夹时，比包含头文件的文件夹时，target_include_directories命令允许在编译目标时，使用-l标志将指定目录添加到编译器中（比如-l /paths）。

- target

  taget可以是库文件、可执行文件。



关于scope：

scope是针对其他工程链接自己的设置，有以下几种：

- PRIVATE

  目录被添加到目标（库）的包含路径中。

- PUBLIC

  目录既被添加到目标的包含路径中，也被添加到链接了这个库的其他目标的包含路径中。

- INTERFACE

  目录没有被添加到目标的包含路径中，而是被添加到链接到这个库的其他目标的包含路径中。

根据上面的三种情况，就可以知道cmake是根据库是否包含这个路径，以及调用了这个库的其他目标是否包含这个路径来划分scope的。



example：

```cmake
# app/include
# app/src
# app/src/service.c
# app/src/

# 添加一个库
add_library(hello_library src/service.c)

# 针对库以及链接到这个库的目标添加头文件目录
target_include_directories(hello_library
	PUBLIC
	${PROJECT_SOURCE_DIR}/include)

add_executable(hello_bin
	src/main.c)
# hello_library的scope是PUBLIC，因此hello_bin链接时也会添加头文件目录
target_link_libraries(hello_bin
	PRIVATE
	hello_library)
```







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
target_link_libraries()
```

要创建使用若干个库文件的可执行文件时，必须告知编译器需要用到这个库。该指令就可以为可执行文件链接库文件。

总结：add_executable是链接源文件，target_link_libraries是链接库文件。











### configure_file

```cmake
configure_file(<input> <output>
	...)
```

将<input>文件中的内容复制到<output>文件中。



该指令一般跟config.h文件有关系，回头再看。





### set

```cmake
set(<variable> <value> [[CACHE <type> <docstring> <FORCE>] | PARENT_SCOPE])
```

将一个CMake、缓存或环境变量设为给定值。



CMake的变量类型。

俩种类型的变量可以同时存在，变量名相同但是值不同。

当使用${FOO}时，CMake会在作用域中寻找一个普通变量，如果该变量存在就返回它。如果不存在才会将其作为缓存变量处理。

比如说：

set(Foo "X")，代码设置了一个变量Foo，虽然它不触发缓存，但是会隐藏任何现有的缓存变量Foo。

set(Foo "x" CACHE...)，代码设置了一个缓存变量，它会忽略任何同名的普通变量。如果Foo在缓存中，那么普通变量和缓存变量都不会发生任何变化。如果Foo不在缓存中，它会被添加到缓存中。

重点要知道每当一个缓存边栏被命令添加或修改时，CMake也会从当前作用域中删除同名的普通变量，这样子就会保留出缓存变量的值。





一个项目包含子项目。子项目有自己的CMakeLists.txt，它是父项目中的CMakeLists.txt中通过add_subdirectory()包含的。后续翻译略...



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