gulp是以流工作方式来整合前端资源的，所谓的流工作方式就是定义做个要执行的任务，然后依次就去执行这些任务。



### Task
任务分为私有和共有，公有可以运行，私有无法运行。



### 异步完成
​	Node以各种方式处理异步。最常见的模式是优先错误回调，你也可能碰到流、promises、事件触发器、子进程或者观察者对象。gulp任务会规范所有的异步类型。

​	当从任务返回一个异步类型时，所执行的结果会影响gulp的执行过程，如果异步类型执行失败，gulp将会停止执行。



### 任务完成信号

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







### 使用

先编写glup任务脚本。

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



再安装插件

> cnpm install -g glup
>
> cnpm install --save-dev browser-sync
>
> cnpm install --save-dev gulp-less



执行启动命令：glup，glup将会执行默认的任务。





### example



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

