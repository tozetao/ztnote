### Network host setting

默认情况下，Elasticsearch只绑定环回地址，如127.0.0.1和[::1]。这足以在一台服务器上运行一个或多个节点的集群，用于开发和测试，但一个有弹性的生产集群必须涉及其他服务器上的节点。虽然有许多关于网络的设置，但通常你需要配置的是network.host。

```yaml
network.host: 192.168.1.10
```

注：当你为network.host提供一个值时，Elasticsearch假定你正在从开发模式转向生产模式，并将一些系统启动检查从警告升级为异常。参见开发模式和生产模式的区别。



### Discovery and cluster formation setting

Discovery与集群组建设置。

在投入生产之前需要配置discovery和cluster formation俩个重要的选项，以便集群中的节点能够相互发现并选出一个master节点。



#### discovery.seed_hosts

开箱即用，无需任何网络配置。es会绑定到回环网卡，并扫描本地9300至9305端口，以连接在同一台服务器上运行的其他节点。这种行为提供了一种自动集群的体验，而不需要做任何配置。

当你想要与其他主机上的节点形成集群时，使用静态的discovery.seed_hosts设置。该设置提供了一个集群中其他节点的列表，这些节点是master-eligible（候选主节点），可能是存活的可通讯的，以作为发现过程的种子。

这个设置可以接受集群中所有符合候选主节点，该设置的值可以是地址数组或YAML序列。每个地址可以是IP地址，也可以是一个通过DNS解析到一个或多个IP地址的主机名。

```yaml
discovery.seed_hosts:
   - 192.168.1.10:9300
   - 192.168.1.11 
   - seeds.mydomain.com 
   - [0:0:0:0:0:ffff:c0a8:10c]:9301
```

- 端口是可选的，默认为9300，但可以被覆盖。
- 如果一个主机名解析到多个IP地址，该节点将试图发现已解出析出阿里的所有地址的其他节点。
- IPv6地址必须用方括号括起来。

If your master-eligible nodes do not have fixed names or addresses, use an alternative hosts provider to find their addresses dynamically.



#### cluster.initial_master_nodes

当你第一次启动ES集群时，集群的引导步骤确定了候选主节点集合，这些节点的投票在第一次选举中被计算在内。在未配置discovery配置的开发模式下，此步骤由节点自身自动执行。

由于自动引导（auto-bootstrapping）本质上是不安全的，所以在生成模式下启动集群时，必须明确列出符合候选主节点，这些节点的投票应该在第一次选举时被计算在内。你可以使用cluster.initial_master_nodes设置该列表。

在集群首次成功组建后，从每个节点的配置中删除cluster.initial_master_nodes设置。在重启集群或向现有集群添加新节点时，不要使用此设置。

```yaml
discovery.seed_hosts:
   - 192.168.1.10:9300
   - 192.168.1.11
   - seeds.mydomain.com
   - [0:0:0:0:0:ffff:c0a8:10c]:9301
cluster.initial_master_nodes: 
   - master-node-a
   - master-node-b
   - master-node-c
```

注：通过node.name表示master节点，默认为其主机名。确保 cluster.initial_master_nodes 中的值与 node.name 完全匹配。If you use a fully-qualified domain name (FQDN) such as master-node-a.example.com for your node names, then you must use the FQDN in this list. Conversely, if node.name is a bare hostname without any trailing qualifiers, you must also omit the trailing qualifiers in cluster.initial_master_nodes.



transport.port

http.port

network.host





reference

https://www.elastic.co/guide/en/elasticsearch/reference/7.17/modules-discovery-settings.html

https://www.elastic.co/guide/en/elasticsearch/reference/7.17/important-settings.html#unicast.hosts

https://blog.csdn.net/laoyang360/article/details/111148362