# wp-config.php

One of the most important files in your WordPress installation is the `wp-config.php` file. This file is located in the root of your WordPress file directory and contains your website’s base configuration details, such as database connection information.



## [Configure Database Settings](https://developer.wordpress.org/apis/wp-config-php/#configure-database-settings)

**Important:** *Never* use a word processor like Microsoft Word for editing WordPress files!

Locate the file `wp-config-sample.php` in the base directory of your WordPress directory and open in a [text editor](https://wordpress.org/support/article/editing-files/#text-editors).

[Top ↑](https://developer.wordpress.org/apis/wp-config-php/#top)

### [Default wp-config-sample.php](https://developer.wordpress.org/apis/wp-config-php/#default-wp-config-sample-php)

Note: This is an example of a default [wp-config-sample.php](https://core.trac.wordpress.org/browser/trunk/wp-config-sample.php). The values here are examples to show you what to do.

Copy

```php
// ** MySQL settings - You can get this info from your web host ** //
/** The name of the database for WordPress */
define( 'DB_NAME', 'database_name_here' );
/** MySQL database username */
define( 'DB_USER', 'username_here' );
/** MySQL database password */
define( 'DB_PASSWORD', 'password_here' );
/** MySQL hostname */
define( 'DB_HOST', 'localhost' );
```

**Note:** Text inside /* */ are *[comments](http://www.php.net/manual/en/language.basic-syntax.comments.php)*, for information purposes only.

[Top ↑](https://developer.wordpress.org/apis/wp-config-php/#top)

#### [Set Database Name](https://developer.wordpress.org/apis/wp-config-php/#set-database-name)

Replace ‘database_name_here’, with the name of your database, e.g. *MyDatabaseName*.

Copy

```php
define( 'DB_NAME', 'MyDatabaseName' ); // Example MySQL database name
```

[Top ↑](https://developer.wordpress.org/apis/wp-config-php/#top)

#### [Set Database User](https://developer.wordpress.org/apis/wp-config-php/#set-database-user)

Replace ‘username_here’, with the name of your username e.g. *MyUserName*.

Copy

```php
define( 'DB_USER', 'MyUserName' ); // Example MySQL username
```

[Top ↑](https://developer.wordpress.org/apis/wp-config-php/#top)

#### [Set Database Password](https://developer.wordpress.org/apis/wp-config-php/#set-database-password)

Replace ‘password_here’, with the your password, e.g. *MyPassWord*.

Copy

```php
define( 'DB_PASSWORD', 'MyPassWord' ); // Example MySQL password
```

[Top ↑](https://developer.wordpress.org/apis/wp-config-php/#top)

#### [Set Database Host](https://developer.wordpress.org/apis/wp-config-php/#set-database-host)

Replace ‘localhost’, with the name of your database host, e.g. *MyDatabaseHost*. A port number or Unix socket file path may be needed as well.

Copy

```php
define( 'DB_HOST', 'MyDatabaseHost' ); // Example MySQL Database host
```

**Note:** There is a good chance you will **NOT** have to change it. If you are unsure, try installing with the default value of ‘localhost’ and see if it works. If the install fails, contact your web hosting provider.

##### MySQL Alternate Port

If your host uses an alternate port number for your database you’ll need to change the **DB_HOST** value in the `wp-config.php` file to reflect the alternate port provided by your host.

For localhost:

Copy

```php
define( 'DB_HOST', '127.0.0.1:<strong>3307' );
or
define( 'DB_HOST', 'localhost:<strong>3307' );
```

For specified server:

Copy

```php
define( 'DB_HOST', 'mysql.example.com:<strong>3307' );
```



Replace **3307** with whatever port number your host gives you.



##### MySQL Sockets or Pipes

If your host uses Unix sockets or pipes, adjust the **DB_HOST** value in the `wp-config.php` file accordingly.

Copy

```php
define( 'DB_HOST', '127.0.0.1:<strong>/var/run/mysqld/mysqld.sock' );
// or define( 'DB_HOST', 'localhost:<strong>/var/run/mysqld/mysqld.sock' );
// or define( 'DB_HOST', 'example.tld:<strong>/var/run/mysqld/mysqld.sock' );
```

Replace` */var/run/mysqld/mysqld.sock*` with the socket or pipe information provided by your host.

[Top ↑](https://developer.wordpress.org/apis/wp-config-php/#top)

### [Database character set](https://developer.wordpress.org/apis/wp-config-php/#database-character-set)

**DB_CHARSET** was made available to allow designation of the database [character set](https://codex.wordpress.org/Glossary#Character_Set) (e.g. tis620 for TIS620 Thai) to be used when defining the MySQL database tables.

The default value of **utf8** ([Unicode](http://en.wikipedia.org/wiki/Unicode) [UTF-8](http://en.wikipedia.org/wiki/UTF-8)) is almost always the best option. UTF-8 supports any language, so you typically want to leave DB_CHARSET at **utf8** and use the [DB_COLLATE](https://codex.wordpress.org/Editing_wp-config.php#Database_collation) value for your language instead.

This example shows utf8 which is considered the WordPress default value:

Copy

```php
define( 'DB_CHARSET', 'utf8' );
```

There usually should be no reason to change the default value of DB_CHARSET. If your blog needs a different character set, please read [Character Sets and Collations MySQL Supports](http://dev.mysql.com/doc/refman/5.6/en/charset-charsets.html) for valid DB_CHARSET values. **WARNING:** Those performing upgrades.

If DB_CHARSET and DB_COLLATE do not exist in your `wp-config.php` file, **DO NOT** add either definition to your `wp-config.php` file unless you read and understand [Converting Database Character Sets](https://codex.wordpress.org/Converting_Database_Character_Sets). Adding DB_CHARSET and DB_COLLATE to the `wp-config.php` file, for an existing blog, can cause major problems.

[Top ↑](https://developer.wordpress.org/apis/wp-config-php/#top)

### [Database collation](https://developer.wordpress.org/apis/wp-config-php/#database-collation)

**DB_COLLATE** was made available to allow designation of the database [collation](https://codex.wordpress.org/Glossary#Collation) (i.e. the sort order of the character set). In most cases, this value should be left blank (null) so the database collation will be automatically assigned by MySQL based on the database character set specified by DB_CHARSET. An example of when you may need to set ”’DB_COLLATE”’ to one of the UTF-8 values defined in [UTF-8 character sets](http://dev.mysql.com/doc/refman/5.6/en/charset-unicode-sets.html) for most Western European languages would be when a different language in which the characters that you entered are not the same as what is being displayed. (See also [Unicode Character Sets](https://dev.mysql.com/doc/refman/8.0/en/charset-unicode-sets.html#charset-unicode-sets-general-versus-unicode) in SQL Manual)

The WordPress default DB_COLLATE value:

```php
define( 'DB_COLLATE', '' );
```

UTF-8 Unicode General collation

```php
define( 'DB_COLLATE', 'utf8_general_ci' );
```

UTF-8 Unicode Turkish collation

```php
define( 'DB_COLLATE', 'utf8_turkish_ci' );
```

There usually should be no reason to change the default value of DB_COLLATE. Leaving the value blank (null) will insure the collation is automatically assigned by MySQL when the database tables are created. **WARNING:** Those performing upgrades

If DB_COLLATE and DB_CHARSET do not exist in your `wp-config.php` file, **DO NOT** add either definition to your `wp-config.php` file unless you read and understand [Converting Database Character Sets](https://codex.wordpress.org/Converting_Database_Character_Sets). And you may be in need of a WordPress upgrade.