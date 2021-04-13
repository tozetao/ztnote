## net/http

### Handler

```go
//处理程序并响应一个HTTP请求。
type Handler interface {
    ServeHTTP(ResponseWriter, *http.Request)
}
```

Handler接口定义了服务端处理请求的公共接口，实现该接口的对象表示服务端实现了处理请求的逻辑。



```go
type HandlerFunc func(ResponseWriter, *Request)

func (f HandlerFunc) ServeHTTP(ResponseWriter, *http.Request) {
    f(w, r)
}
```

所有处理请求逻辑的函数都需要实现Handler接口，为了避免这种情况定义了HandlerFunc类型，并且该类型实现了Handler接口。

这样就可以把同类型的函数转成HandlerFunc类型，统一的调用ServeHTTP方法来处理请求。例如：

```go
func AdminHandler(w ResponseWriter, r *http.Request) {
    //...
}
handler := HandlerFunc(AdminHandler)
handler.ServeHTTP(w, r)
```



### ServeMux

http包中的默认路由对象，应用一般通过该对象来注册路由规则。当有请求时也是通过该对象来判断分发到哪个处理器（Handler）。

```go
type ServeMux struct {
    mu sync.RWMutex
    m map[string]muxEntry
}

type muxEntry struct {
    explicit bool			//是否精准匹配
    h		 Handler		//处理器
}
```


ServeMux的方法：

```go
//根据path获取Handler
func (mux *ServeMux) match(path string) Handler

//根据Request获取Handler，内部实现调用match
func (mux *ServeMux) handler(r *Request) Handler  

//ServeHttp也实现了Handler接口，它实际上也是一个Handler！内部实现调用handler
func (mux *ServeMux) ServeHTTP(w ResponseWriter, r *Request) 

//注册handler方法
func (mux *ServeMux) Handle(pattern string, handler Handler)

//注册handler方法（直接使用func注册）
func (mux *ServeMux) HandleFunc(pattern string, handler func(ResponseWriter, *Request)) 
```



http包提供注册路由的方法就是通过ServeMux对象来完成的：

```go
func Handle(pattern string, handler Handler) {
    DefaultServeMux.Handle(pattern, handler)
}

func HandleFunc(pattern string, handler func(ResponseWriter, *Request)){
     DefaultServeMux.HandleFunc(pattern, handler)
}
```



### Server

Server对象定义用于运行HTTP服务器的配置，提供启动服务方法。

```go
type Server struct {
    // 监听的地址和端口
    Addr           string        
    
    //所有请求需要调用的Handler，实际上是实现Handler接口的路由对象，如果为空则设置为DefaultServeMux
    Handler        Handler       
    
    // 读的最大Timeout时间
    ReadTimeout    time.Duration 
    
    // 写的最大Timeout时间
    WriteTimeout   time.Duration 
    
    // 请求头的最大长度
    MaxHeaderBytes int           
    
    // 配置TLS
    TLSConfig      *tls.Config   
}
```

Server提供的方法：

```go
//对某个端口进行监听，里面就是调用for进行accept的处理了
func (srv *Server) Serve(l net.Listener) error  

//开启http server服务，内部调用Serve
func (srv *Server) ListenAndServe() error  

//开启https server服务，内部调用Serve
func (srv *Server) ListenAndServeTLS(certFile, keyFile string) error 
```

http包中提供提供服务器的方法，就是通过Server提供的方法来实现的。

```go
//开启Http服务
func ListenAndServe(addr string, handler Handler) error   

//开启HTTPs服务
func ListenAndServeTLS(addr string, certFile string, keyFile string, handler Handler) error 
```









后续：

- 路由注册流程
- Server对象处理一个请求的流程





## httprouter

httprouter的路由包含俩种类型的参数，命名参数和全匹配参数。



命名参数是动态路径段，它可以匹配任何东西，直到下一个"/"或路径结束。

```ini
Path:	/blog/:category/:post
Requests:
	/blog/go/request		//匹配，category=go，post=request
	/blog/go/request/		//不匹配，但是会重定向
	/blog/go				//不匹配
	/blog/go/request-routers/comments	//不匹配
```

同一层级的路由参数是允许同名的，而不同名的路由参数将会发生panic。

```ini
/user/:post/balance		//right
/user/:post/account		//right
/user/:hi/add			//error
```









全匹配参数可以匹配任何内容直到路径结束，包括目录索引（指catch-all之前的"/"）。由于全匹配参数是匹配任何内容直到结束，因此该参数必须是最终路径元素。

```ini
Path: /files/*filepath

Requests:
 /files/                             match: filepath="/"
 /files/LICENSE                      match: filepath="/LICENSE"
 /files/templates/article.html       match: filepath="/templates/article.html"
 /files                              no match, but the router would redirect
```



