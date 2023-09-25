### QueryVars

QueryVars参数可以是字符串或者数组。以下是QueryVars是数组时key的说明，变量名即是key的名字。

```
$attachment_id
@type int
Attachment post ID. Used for 'attachment' post_type.
附件帖子ID，用于附件帖子类型。


$author
@type int|string
Author ID, or comma-separated list of IDs.
作者ID，或者以逗号分割的ID列表。


$author_name
@type string
User 'user_nicename'.
用户昵称。


$author__in
@type int[]
An array of author IDs to query from.
要查询的作者ID数组。


$author__not_in
@type int[]
An array of author IDs not to query from. 不查询的作者ID数组。


$cache_results
@type bool
Whether to cache post information. Default true. 是否缓存帖子信息。默认true。


$cat
@type int|string
Category ID or comma-separated list of IDs (this or any children).  
分类ID或者以逗号分割的ID列表（本分类或者子分类）。


$category__and
@type int[]
An array of category IDs (AND in). 分类ID数组（And in查询）。


$category__in
@type int[]
An array of category IDs (OR in, no children). 分类ID数组（or in，无子分类）。


$category__not_in
@type int[]
An array of category IDs (NOT in). 分类ID数组（not in查询）。


$category_name
@type string
Use category slug (not name, this or any children). 使用类别标题（不是名称、此或任何子类别）。


$comment_count
@type array|int
Filter results by comment count. Provide an integer to match comment count exactly. Provide an array with integer 'value' and 'compare' operator ('=', '!=', '>', '>=', '<', '<=' ) to compare against comment_count in a specific way.
按评论数过滤结果。提供一个整数，以精确匹配评论数。提供一个包含整数 "值 "和 "比较 "操作符（'='、'!='、'>'、'>='、'<'、'<='）的数组，会使用特定的比较操作符与 comment_count 进行比较。


$comment_status
@type string
Comment status.
评论状态。


$comments_per_page
@type int
The number of comments to return per page. Default 'comments_per_page' option. 
每页返回的评论数量。默认coments_per_page选项。


$date_query
@type array
An associative array of WP_Date_Query arguments.See WP_Date_Query::__construct().
WP_Date_Query参数的关联数组，详见 WP_Date_Query::__construct()


$day
@type int
Day of the month. Default empty. Accepts numbers 1-31.
月份的天数，默认空，接收1-31数字。


$exact
@type bool
Whether to search by exact keyword. Default false. 
是否通过exact关键字进行搜索，默认false。


$fields
@type string
Post fields to query for. Accepts:
- '' Returns an array of complete post objects (`WP_Post[]`).
- 'ids' Returns an array of post IDs (`int[]`).
- 'id=>parent' Returns an associative array of parent post IDs, keyed by post ID (`int[]`). Default ''.
要查询的帖子字段，接受以下值：
- '' 会返回一个元素是完整post对象的数组（WP_Post[]）。
- 'ids'将返回post ID数组（int[]）。
- 'id=>parent'将返回一个父级帖子id的关联数组。
默认''


$hour
@type int
Hour of the day. Default empty. Accepts numbers 0-23. 一天内的小时数，默认空，接受0-23


$ignore_sticky_posts
@type int|bool
Whether to ignore sticky posts or not. Setting this to false excludes stickies from 'post__in'. Accepts 1|true, 0|false.
Default false.
是否忽略粘性帖子（sticky posts）。设置为false将会从post__in中忽略粘性帖子。默认false。


$m
@type int
Combination YearMonth. Accepts any four-digit year and month numbers 01-12. Default empty.
组合年月。接受任何4位数年份和01-12的月数字。默认空。


$meta_key
@type string|string[]
Meta key or keys to filter by.


$meta_value
@type string|string[]
Meta value or values to filter by.


$meta_compare
@type string
MySQL operator used for comparing the meta value. See WP_Meta_Query::__construct() for accepted values and default value.
用于比较元值（meta value）的MySQL操作符。
有关可接受值和默认值，参数WP_Meta_Query::__construct()


$meta_compare_key
@type string
MySQL operator used for comparing the meta key.
See WP_Meta_Query::__construct() for accepted values and default value.


$meta_type
@type string
MySQL data type that the meta_value column will be CAST to for comparisons.
See WP_Meta_Query::__construct() for accepted values and default value.


$meta_type_key
@type string
MySQL data type that the meta_key column will be CAST to for comparisons.See WP_Meta_Query::__construct() for accepted values and default value.


$meta_query
@type array
An associative array of WP_Meta_Query arguments.See WP_Meta_Query::__construct() for accepted values.


$menu_order
@type int
The menu order of the posts.
帖子的菜单顺序。


$minute
@type int
Minute of the hour. Default empty. Accepts numbers 0-59.


$monthnum
@type int
The two-digit month. Default empty. Accepts numbers 1-12.


$name
@type string
Post slug.


$nopaging
@type bool
Show all posts (true) or paginate (false). Default false. 
显示所有帖子（true）或者分页（false），默认false。


$no_found_rows
@type bool
Whether to skip counting the total rows found. Enabling can improve performance. Default false.
是否跳过计算总行数。启用可以提高性能，默认值为false。
     
@type int
$offset
The number of posts to offset before retrieval.
检索前要便宜的贴子数。


$order
@type string
Designates ascending or descending order of posts. Default 'DESC'.
Accepts 'ASC', 'DESC'.
指定帖子的升序或降序。默认'DESC'。接受'ASC', 'DESC'。


$orderby
@type string|array
Sort retrieved posts by parameter. One or more options may be passed.
To use 'meta_value', or 'meta_value_num', 'meta_key=keyname' must be also be defined. To sort by a specific `$meta_query` clause, use that clause's array key. Accepts:
按照参数对检索到的帖子排序。可传递一个或多个选项。
要使用 "meta_value "或 "meta_value_num"，必须同时定义 "meta_key=keyname"。要按特定的 `$meta_query` 子句排序，请使用该子句的数组键。接受：
- 'none'
- 'name'
- 'author'
- 'date'
- 'title'
- 'modified'
- 'menu_order'
- 'parent'
- 'ID'
- 'rand'
- 'relevance'
- 'RAND(x)' (where 'x' is an integer seed value)
- 'comment_count'
- 'meta_value'
- 'meta_value_num'
- 'post__in'
- 'post_name__in'
- 'post_parent__in'
- The array keys of `$meta_query`.
Default is 'date', except when a search is being performed, when the default is 'relevance'.
默认为 "日期"，除非正在进行搜索，此时默认为 "相关性"。


$p
@type int
Post ID.
帖子ID


$page
@type int
Show the number of posts that would show up on page X of a static front page.
显示将可能显示在静态页的第X页上的贴子数。


$paged
@type int
The number of the current page.
当前页数

$page_id
@type int
Page ID.
页面ID


$pagename
@type string
Page slug.
页面短语。


$perm
@type string
Show posts if user has the appropriate capability.
如果用户具有适当的权限（capability），则显示帖子。


$ping_status
@type string
Ping status.

$post__in
@type int[]
An array of post IDs to retrieve, sticky posts will be included.
将包括一组要检索的帖子ID、粘性帖子。

$post__not_in
@type int[]
An array of post IDs not to retrieve. Note: a string of comma-separated IDs will NOT work.
不检索的帖子ID的数组。注意：以逗号分隔的ID字符串不起作用。


$post_mime_type
@type string
The mime type of the post. Used for 'attachment' post_type.
帖子的mime类型，用于附件类型的帖子。


$post_name__in
@type string[]
An array of post slugs that results must match.
要匹配的帖子短语数组。


$post_parent
@type int
Page ID to retrieve child pages for. Use 0 to only retrieve top-level pages.
要检索子页面的页面id。使用0禁用检索顶级页面。

@type int[]
$post_parent__in
An array containing parent page IDs to query child pages from.
包含父页面ID的数组，用于查询子页面。

$post_parent__not_in
@type int[]
An array containing parent page IDs not to query child pages from.
一个数组，包含不从中查询子页面的父页面ID。

$post_type
@type string|string[]
A post type slug (string) or array of post type slugs.
Default 'any' if using 'tax_query'.
帖子类型短语（字符串）或者帖子类型短语数组。
如果使用“tax_query”，则默认为“any”。


$post_status
@type string|string[]
A post status (string) or array of post statuses.
发布状态（字符串）或发布状态的数组。


$posts_per_page
@type int
The number of posts to query for. Use -1 to request all posts.
要查询的帖子数。使用-1请求所有帖子。


$posts_per_archive_page
@type int
The number of posts to query for by archive page. Overrides 'posts_per_page' when is_archive(), or is_search() are true.
按存档页面查询的帖子数。当is_archive（）或is_search（）为true时，覆盖“posts_per_page”。

$s
@type string
Search keyword(s). Prepending a term with a hyphen will exclude posts matching that term. Eg, 'pillow -sofa' will return posts containing 'pillow' but not 'sofa'. The character used for exclusion can be modified using the the 'wp_query_search_exclusion_prefix' filter.
搜索关键词。在一个term前加上连字符将排除与该term匹配的帖子。例如，"pillow -sofa "将返回包含 "pillow "但不包含 "sofa "的帖子。可使用 "wp_query_search_exclusion_prefix "过滤器修改用于排除的字符。



$search_columns
@type string[]
Array of column names to be searched. Accepts 'post_title', 'post_excerpt' and 'post_content'. Default empty array.
要搜索的列名数组。接受 "post_title"、"post_excerpt "和 "post_content"。默认为空数组。


$second
@type int
Second of the minute. Default empty. Accepts numbers 0-59.
秒数。默认为空。接受数字 0-59。


$sentence
@type bool
Whether to search by phrase. Default false.
是否按短语搜索。默认为 false。


$suppress_filters
@type bool
Whether to suppress filters. Default false.
是否支持过滤器，默认false。

$tag
@type string
Tag slug. Comma-separated (either), Plus-separated (all).
标签符号。逗号分隔（任一），正号分隔（全部）。

$tag__and
@type int[]
An array of tag IDs (AND in).

$tag__in
@type int[]
An array of tag IDs (OR in).

$tag__not_in
@type int[]
An array of tag IDs (NOT in).

$tag_id
@type int
Tag id or comma-separated list of IDs.
标签 ID 或以逗号分隔的 ID 列表。

$tag_slug__and
@type string[]
An array of tag slugs (AND in).
标签短语数组（and in查询）。

$tag_slug__in
@type string[]
An array of tag slugs (OR in). unless 'ignore_sticky_posts' is true. Note: a string of comma-separated IDs will NOT work.
标签短语数组（OR in查询）。
除非 "ignore_sticky_posts "为 true。注意：逗号分隔的 ID 字符串不起作用。


$tax_query
@type array
An associative array of WP_Tax_Query arguments. See WP_Tax_Query::__construct().
WP_Tax_Query参数的关联数组，详见WP_Tax_Query::__construct().

$title
@type string
Post title.
帖子标题。

$update_post_meta_cache
@type bool
Whether to update the post meta cache. Default true.

$update_post_term_cache
@type bool
Whether to update the post term cache. Default true.
是否更新post term缓存，默认true。

$update_menu_item_cache
@type bool
Whether to update the menu item cache. Default false.
是否更新菜单项缓存（menu item），默认false。

$lazy_load_term_meta
@type bool
Whether to lazy-load term meta. Setting to false will disable cache priming for term meta, so that each get_term_meta() call will hit the database. 
是否懒加载term meta。设置为 false 将禁用term meta的缓存启动，这样每次调用 get_term_meta() 时都会访问数据库。
Defaults to the value of `$update_post_term_cache`.
默认值为 `$update_post_term_cache`。

$w
@type int
The week number of the year. Default empty. Accepts numbers 0-53.
年的周数。默认为空。接受数字 0-53。

$year
@type int
The four-digit year. Default empty. Accepts any four-digit year.
四位数的年份。默认为空。接受任何四位数年份。

```







NONCE是number used once的缩写，在WP中它是一个简短的像密码一样的随机字符串，特定于以下内容：
- 一个用户
- 一个操作（删除、更新、保存等）
- 一个对象（一个帖子、链接、插件设置等）
- One time frame of 24 hours
比如在WP中删除文章#43的链接可以是http://example.com/wp-admin/post.php?post=43&action=trash&_wpnonce=83a08fcbc2，这里的nonce（83a08fcbc2）有效期只有24小时，且只有自己使用时才有效，且只能用于删除#43帖子。
