

```php
bool flock(resource $handle, int $operation [, int &$wouldblock])
```

PHP支持一种轻便的锁定文件的方法，但是所有访问程序必须使用同一方式锁定，否则它不会正常工作。默认这个函数会阻塞直到获取锁。

operation有以下值：

- LOCK_SH

  取得共享锁定（读取的程序）

- LOCK_EX

  取得独占锁定（写u人的程序）。

- LOCK_UN

  释放锁定的锁，无论是共享还是独占。

解释一下，所谓的共享锁定，指的是当前程序获取共享锁成功后，其他程序也能够获取该指针的共享锁，但是获取独占锁时就会被阻塞。

而独占锁定是当前程序获取独占锁成功后，其他程序无法获取该指针的共享锁或独占锁。

也就是常说的读读不互斥，读写、写写互斥（阻塞）。



注：文档说必须使用统一方式锁定，否则程序不正常工作。我理解的是你可以直接读取或写入已经有加锁的文件。



example：读读不互斥

```php
// demo1.php
$filename = './test.log';
touch($filename);

$handler = fopen($filename, 'rb');
if (flock($handler, LOCK_SH)) {
    echo 'it is demo1.php, sleep 5 seconds';
    sleep(5);
    flock($handler, LOCK_UN);
} else {
    fclose($handler);
}
```

demo1文件。

```php
// demo2.php
$filename = './test.log';
touch($filename);

$handler = fopen($filename, 'rb');
if (flock($handler, LOCK_SH)) {
    echo 'it is demo2.php';
    flock($handler, LOCK_UN);
} else {
    fclose($handler);
}
```

这俩个文件都是获取共享锁，如果demo1文件先执行demo2后执行，可以看到即使demo1获取加锁成功后，也不会阻塞到demo2文件。









example：读写、写写互斥

```php
// demo1.php
$filename = './test.log';
touch($filename);

$handler = fopen($filename, 'rb');
if (flock($handler, LOCK_EX)) {
    echo 'it is demo1.php, sleep 5 seconds';
    sleep(5);
    flock($handler, LOCK_UN);
} else {
    fclose($handler);
}	
```

demo1文件。

```php
// demo2.php
$filename = './test.log';
touch($filename);

$handler = fopen($filename, 'rb');
if (flock($handler, LOCK_SH)) {
    echo 'it is demo2.php';
    flock($handler, LOCK_UN);
} else {
    fclose($handler);
}
```

demo1文件先执行，demo2文件再执行，可以交换文件双方锁的类型，可以发现会一直相互阻塞的。