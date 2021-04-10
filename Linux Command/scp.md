script -q /dev/stdout -c 'scp remotehost:/path/to/file /local/file' | tee scp.log
将scp回显结果输出到日志中


script scp_transfer.txt
scp remotehost:/path/to/file /local/file
exit

cat scp_transfer.txt