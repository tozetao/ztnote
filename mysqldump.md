mysqldump -h {host} -u {user} -p{password} --compact --complete-insert --no-create-info --net_buffer_length=4096 {database} {table} 

--net_buffer_length=4096

--extended-insert

--skip-extended-insert





https://www.cnblogs.com/chenmh/p/5300370.html



方式1
- 更改原有表的名字
- 创建新的表
- 再删除原有表


保留一个月的数据
- 查询最近一个月的第一条记录的id
- 导出这部分数据
- 再导入这部分数据

