gulp是以流工作方式来整合前端资源的，所谓的流工作方式就是定义做个要执行的任务，然后依次就去执行这些任务。



任务
​    任务分为私有和共有，公有可以运行，私有无法运行。



异步完成
​	Node以各种方式处理异步。最常见的模式是优先错误回调，你也可能碰到流、promises、事件触发器、子进程或者观察者对象。gulp任务会规范所有的异步类型。

​	当从任务返回一个异步类型时，所执行的结果会影响gulp的执行过程，如果异步类型执行失败，gulp将会停止执行。



任务完成信号

gulp4要求一个任务执行完毕后需要发送一个完成信号，如果不返回完成信号通知gulp，将会报错。所谓的完成信号就是上面说的各种异步类型。

- return errir-first callback

```javascript
//如果没有返回值，调用匿名函数的第一个参数，这个就是文档说的优先错误回调（error-first callback）
function task1(done) {
    console.log('error-first callback.');
    done();
}
```

- return Promise

```javascript
function task() {
    return new Primise((resovle, reject) => {
        //do something
        resovle();
    });
}
```

- return stream

```javascript
const { src, dest } = require('gulp');

function task() {
    return src('app/css/*.css').pipe(dest('app/css'));
}
```







### gulpfile.js

```javascript

 
var gulp = require('gulp');
var rename = require('gulp-rename');//重命名
var uglify=require('gulp-uglify');//js压缩
var watch=require('gulp-watch');//监视
var less=require('gulp-less');//编译less
var minifyCss = require("gulp-minify-css");//压缩CSS
var minifyHtml = require("gulp-minify-html");//压缩html
var jshint = require("gulp-jshint");//js检查
var imagemin = require('gulp-imagemin');
var pngquant = require('imagemin-pngquant'); //png图片压缩插件
var connect=require('gulp-connect');//引入gulp-connect模块 

// gulp.task('min', function () {
//     gulp.src('copyUrl/js/resize.js') // 要压缩的js文件
//     .pipe(uglify()) //使用uglify进行压缩,更多配置请参考：
//     .pipe(rename('resize.min.js'))
//     .pipe(gulp.dest('dist/fot')); //压缩后的路径
// });
 
gulp.task('watchs',function(){
    gulp.watch('cug_vatti_Backpass/*.html',gulp.series('html'));
    gulp.watch('cug_vatti_Backpass/css/*.less',gulp.series('css'));
    gulp.watch('cug_vatti_Backpass/js/*.js',gulp.series('js'));
})
gulp.task('connect',function(){
    connect.server({
        root:'cug_vatti_Backpass',//根目录
        // ip:'192.168.11.62',//默认localhost:8080
        livereload:true,//自动更新
        port:9909//端口
    })
})
 
gulp.task('html',function(){
    return gulp.src('cug_vatti_Backpass/*.html')
    .pipe(gulp.dest('dist/html'))
    .pipe(connect.reload());
})
 
gulp.task('css',function(){
    return gulp.src('cug_vatti_Backpass/css/*.less')
    .pipe(less())//编译less
    .pipe(gulp.dest('cug_vatti_Backpass/css')) //当前对应css文件
    .pipe(connect.reload());//更新
})
 
gulp.task('js',function(){
    return gulp.src('cug_vatti_Backpass/js/jquery-1.8.0.min.js')
    .pipe(jshint())//检查代码
    .pipe(uglify())//压缩js
    .pipe(gulp.dest('dist/js'))
    .pipe(connect.reload());
})
 //gulp.series|4.0 依赖
 //gulp.parallel|4.0 多个依赖嵌套
gulp.task('default',gulp.series(gulp.parallel('connect','watchs','html','css','js')));
```







```javascript
const { 
    src,
    watch,
    dest,
    series
} = require('gulp');

const less        = require('gulp-less');
const browserSync = require('browser-sync').create();

//在任务执行完毕后，由browserSync重载，要注意的时reload是在gulp.dest()后再调用。
const reload      = browserSync.reload;

function lessTask() {
    return src('css/*.less')
        .pipe(less())
        .pipe(dest('css'))
        .pipe(reload({stream: true}));
}

function jsTask() {
    return src('js/*.js').pipe(dest('dist/js'));
}

function reloadJs(done) {
    reload({strem: true});
    done();
}

function serve(done) {
    browserSync.init({
        proxy: "http://127.0.0.1/modern_php"
        // server: {
        //     baseDir: './',
        //     index: 'index.html'
        // }
    });

    watch('php/**/*.php').on('change', reload);
    watch('*.html').on('change', reload);
    watch('css/*.less', lessTask);
    watch('js/*.js', series(jsTask, reloadJs));
    done();
}

exports.default = serve;
```







如何压缩文件? 



gulp如何整合到Laravel中?

​	browsersync是一个同步测试工具，它检测文件变化并刷新或注入浏览器变化的内容。配置gulp，能够实现自动化测试和整合前端资源。

​	让gulp监视PHP/JS/HTML/CSS等文件的变化，然后刷新修改内容。整合到laravel中时，可以让gulp监视视图文件、js、css文件变化来达到自动测试的效果。





我们将插件当作模块，通过requireJS引入插件模块与其对应的css文件来管理所有插件的加载。

再将个人JS代码与CSS代码通过requireJS来管理。







管理插件

css：可以模块化的去编写css代码，需要某个模块的时候再一起加载。
css加载：不同的页面，加载不同的css样式
js加载：同上，不同的页面，加载不同的css


如何管理JS插件。
如何使用gulp整理整个项目。

map
​	在加载其他模块之前，都必须先加载map中的模块。


shim
​	通过require的define()方法定义的模块就是标准的，只有不准备的情况下才会使用shim定义的参数。



require css
​	1. 在自定义模块的时候加载需要的样式文件
​	define(['css!../style/a.css'], function() {});

	2. 在shim中定义模块的依赖模块，间接的引入css样式
	    shim: {
		'index': ['css!css/index.css']}






一个js插件会有对应的css文件，require css可以用于解决这种问题。









https://www.cnblogs.com/HCJJ/p/6611669.html







requirejs参考链接.

http://www.requirejs.cn/

https://www.cnblogs.com/HCJJ/p/6611669.html

https://blog.csdn.net/wrp920227/article/details/78055970

requireJS

map

shim

上面这俩个配置不知道时做什么的

