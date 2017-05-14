
11. traits
	一种代码复用机制，为了减少单继承语言的限制，能在不用层次结构内独立的类中复用的方法集。
	
	11.1 优先级
		traits定义的方法是可以被覆盖的，优先顺序是当前类的成员覆盖trait的方法，而trait的方法会覆盖基类的方法。
	
	11.2 多个trait
		trait first_trait{
			public function first_method()
			{
				echo 'first method';
			}
		}
		trait second_trait{
			public function second_method(){
				echo 'second method';
			}
		}

		class MyClass{
			use first_trait, second_trait;
		}

		$obj = new MyClass();
		$obj->first_method();
		$obj->second_method();
	
	11.3 冲突
		多个trait可能会插入同名的方法，
		其他的特性看手册