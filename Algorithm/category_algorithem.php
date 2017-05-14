<?php 
/*

1. 邻表模型
	1.1 表结构
		CREATE TABLE category(
			c_id int,
			name varchar(20),
			p_id int default 0
		);

	1.2 领表模型缺陷
		1. 无法检索整棵树或者某个节点下的所有叶子节点，因为你不知道这个节点下嵌套的节点层次是多少。
		2. 无法检索一个节点的所有父级节点

		这俩个缺点可以通过查询表的所有记录，通过程序来检索树，适用于数据量不大的业务。

		3. 删除一个节点可能会孤立一颗子树。
			这种情况，你需要将该删除节点的下一层级的子节点指向一个新的父节点，防止子树的孤立。

		最大的优点估计就是使用方便，理解简单了。

2. 嵌套集合模型
	在这个模型中，我们将以一种新的方式来看待分层数据，不再是点和线，而是嵌套容器。
	2.1 表结构
		create table category(
			c_id int,
			name char(20),
			lft int,
			rgt int
		);

		insert into category 
			values(1,'ELECTRONICS',1,20),
				(2,'TELEVISIONS',2,9),
				(3,'TUBE',3,4),
				(4,'LCD',5,6),
				(5,'PLASMA',7,8),
				(6,'PORTABLE ELECTRONICS',10,19),
				(7,'MP3 PLAYERS',11,14),
				(8,'FLASH',12,13),
				(9,'CD PLAYERS',15,16),
				(10,'2 WAY RADIOS',17,18);
							
							嵌套树模型
							1 element 18
					2 tel 9					10 for 17
		3 trle 4	5 lcd 6 	7 pus 8		11 fuash 16
											12 haha 15
											13 oooo 14
	
		当我们为树状的结构编号时，我们从左到右，一次一层，为节点赋右值前，先从左到右遍历其子节点给其子节点赋左右值，这种方法叫做先序遍历算法。
	
		规律：子节点的lft值总是在父节点的lft值和rgt值之间。
	
<<<<<<< HEAD
	2.2 检索整课树或某个节点的子树
		根据节点id查询出节点的数据，再根据该节点的左右值进行范围查询。
		select * from category where id=node_id
		select * from category where lft >= a and rgt <=b

		子节点的lft值总是在父节点的lft和rgt之间，所以可以通过父节点连接到子节点上来检索整棵树
		SELECT node.name FROM category AS node, category AS parent
			WHERE node.lft BETWEEN parent.lft AND parent.rgt AND parent.name = 'ELECTRONICS'
			ORDER BY node.lft;
=======
	2.2 检索整树
		因为子节点的lft值总是在父节点的lft值和rgt值之间，所以只要查询出在父节点的lft值和rgt值之间的子节点即可。
		example：
			select * from category where id = 1;
			select * from category where lft between 1 and 20;

		另外一种方式是以迪尔卡积查询每个节点树的集合。
		example：
			select * from category node, category parent
				where node.lft between parent.lft and parent.rgt

		通过增加条件 parent.name = nodeName(节点名)，可以查询出一个节点树。
		example：
			select * from category node, category parent
				where node.lft between parent.lft and parent.rgt
					and parent.name = 'element'
			--查询element节点的节点树

	2.3 检索节点的单一路径
		检索节点的单一路径，即哪些节点的lft值和rgt值包含了要查询节点的lft值。
		简单的说，就是查询节点的lft值同时被哪些父节点间接或直接的拥有，sql语句如下。

		example：
			select * from category node, category parent
				where node.lft between parent.lft and parent.rgt
				and node.name = ''

		example：
			select lft from category where node.name = 'LCD';	// 要查找的节点的lft值，lft比如是5
			select * from category where 5 between lft and rgt; //
	
	2.4 查询所有叶子节点
		每个最底层的叶子节点的rgt = lft + 1，因为根据这个条件可以查询出所有叶子节点。
>>>>>>> 99ddbbc02823164b03dea1ec99d94c00a4db5246

	2.5 查询所有节点的深度
		查询节点的深度就是查询该节点处于整棵树所在的层次。
		只要知道节点同时被哪些父节点间接或直接的包含，就能知道该节点在树中所处的层次。

<<<<<<< HEAD
		select parent.name, (count(parent.category_id) - 1) as depth from category as parent, category as child
			where child.lft between parent.lft and parent.rgt
			group by parent.name
			order by depth

		如果想要统计某个节点的深度，只需要查询该加点lft值和rgt值之间的节点数量，这种方式更快，并且无需自关联。


	2.5 查询一个节点路径（父节点深度）
		统计一个节点路径，就是统计有多少个父节点拥有该节点。
		a. 迪而卡集交叉查询每个节点树的集合，统计这个节点被哪些父节点拥有。
		b. 查询这个节点的lft值，再查找这个lft值位于哪些节点的lft值和rgt值之间。
	

	2.6 检索一个子树的深度信息
		计算一个子树每个节点的层次。
		将这个子树的根节点定义为sub_person，根节点定义为person

		然后计算

	SELECT node.name, (COUNT(parent.name) - (sub_tree.depth + 1)) AS depth
FROM nested_category AS node,
        nested_category AS parent,
        nested_category AS sub_parent,
        (
                SELECT node.name, (COUNT(parent.name) - 1) AS depth
                FROM nested_category AS node,
                nested_category AS parent
                WHERE node.lft BETWEEN parent.lft AND parent.rgt
                AND node.name = 'PORTABLE ELECTRONICS'
                GROUP BY node.name
                ORDER BY node.lft
        )AS sub_tree
WHERE node.lft BETWEEN parent.lft AND parent.rgt
        AND node.lft BETWEEN sub_parent.lft AND sub_parent.rgt
        AND sub_parent.name = sub_tree.name
GROUP BY node.name
ORDER BY node.lft;


语句分析
	WHERE node.lft BETWEEN parent.lft AND parent.rgt
        AND node.lft BETWEEN sub_parent.lft AND sub_parent.rgt
        这个查询条件是自关联3次，由于条件相同，node表和parent表产生N条记录，相同条件下再自身关联N * N条相同的记录。

   	from a,b,c；这种查询本来就是迪而卡集的查询。
=======
		example：查询单一节点的层次
			select node.name, (count(*) - 1) from category node, category parent
				where node.lft between parent.lft and parent.rgt
					and node.name = 'nodeName'
		
		example：检索所有节点的深度
			select node.name (count(*) - 1) depth 
				from category node, category parent
				where node.lft between parent.lft and parent.rgt
				group node.name
				order by node.lft

	2.6 检索子树的深度
		1. 检索该节点树
		2. 再关联自身，检索该节点树每个节点的深度
		3. 查询该节点在整棵树中的深度，检索出来的值设定为x表
		4. 通过节点树每个节点的深度 - 该节点的深度，就能计算出子树的深度

		select 
		 	node.name, (count(node.`name`) - (sub_tree.depth + 1)) depth
			from category node
				, category parent
		 		, category sub_parent
 				,
	  			(
	  				select node.name, (count(*)-1) as depth
  						from category node, category parent
  						where node.lft between parent.lft and parent.rgt and node.`name`='PORTABLE ELECTRONICS'
 						group by node.`name` order by node.lft
	  			) as sub_tree

			where 
				node.lft between parent.lft and parent.rgt and parent.name='PORTABLE ELECTRONICS'
				and node.lft between sub_parent.lft and sub_parent.rgt

			group by node.name
			order by node.lft

		子查询：查询该节点相对于整棵树的深度

		第一个条件判断：
			查询该节点树
		第二个条件判断：
			用于查询该节点树每个节点相对于整棵树的深度，通过node.name(子节点数的每个节点名)分组计算。

		最后通过节点树每个节点的深度 - 该节点的深度，就能计算出子树的深度

	缺点：
		我觉得这种方式的最大缺点是查询一个节点的下一级所有节点的时候，效率是最低的。

		因为你必须加载这个节点树，同时查询出该节点树的深度，即节点层级，才能找出下一集的节点。

		在数据库中，如果你的树非常的大，那么每次获取查询的时候，都需要遍历俩次表，这是很浪费效率的。
>>>>>>> 99ddbbc02823164b03dea1ec99d94c00a4db5246
