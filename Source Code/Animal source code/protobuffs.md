protobuffs目录说明

- proto目录

  包含所有proto文件列表。

- ebin目录

  包含编译过的proto文件和protobuffs.app文件。

  编译后会出现all_pb.beam和all_pb.hrl文件。

- Output目录

- all.proto

  该文件会导入所有proto文件



编译过程

- cd ./protobuffs/ebin

  进入protobuffs的ebin目录

- erl -eval "protobuffs_compile:scan_file(\"../all.proto\"),erlang:halt()"

  调用protobuffs模块来进行编译。

  在本项目中，所有proto文件会被导入到all_pb.proto文件中，因此完成编译后会产生一个.hrl头文件和beam文件。

- 将all_pb.hrl移动到项目根目录下的include目录

- 将all_pb.beam移动到根目录下的ebin目录下