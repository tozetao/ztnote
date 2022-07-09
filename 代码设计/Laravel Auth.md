













> Laravel中是如何对用户进行认证的？

在使用Auth的静态方法时，本质上是执行AuthManager的静态方法。而AuthManager在执行静态方法时，是调用guard实例来执行的。

```php
    /**
     * Dynamically call the default driver instance.
     *
     * @param  string  $method
     * @param  array  $parameters
     * @return mixed
     */
    public function __call($method, $parameters)
    {
        // guard是获取当前的守卫对象
        return $this->guard()->{$method}(...$parameters);
    }
```



首先AuthManager定义了如何创建guard和provider，我们在使用Auth时调用的静态方法，都是由AuthManager来执行的。而AuthManager会调用guard对象来执行所调用的方法。







### Auth

在Laravel中，Auth是一个门面对象。它就是一个代理对象，Auth静态方法的调用本质上是获取容器中注册的AuthManager对象来调用的。





### AuthManager

认证管理类，实现了Factory接口。

```php
// Illuminate\Contracts\Auth\Factory
// Factory接口定义了guard的setter和getter接口。
interface Factory
{
    // 通过$name获取一个grard示例
    public function guard($name = null);
    
    // 设置Factory应该提供的默认守卫（guard）
    public function shouldUse($name);
}
```

AuthManager关联guard、provider对象，简单的说，AuthManager定义了如何创建guard和provider。

- guard：守卫，提供统一的认证接口。
- provider：守卫的用户提供者，表示如何从存储系统中提取用户数据。



```php
    /**
     * Resolve the given guard.
     *
     * @param  string  $name	守卫名
     * @return \Illuminate\Contracts\Auth\Guard|\Illuminate\Contracts\Auth\StatefulGuard
     *
     * @throws \InvalidArgumentException
     */
    protected function resolve($name)
    {
        // 获取守卫的配置，定义在auth.php配置文件中。
        // 主要获取守卫的驱动（driver）和用户提供者（povider），这里的驱动其实就是guard的实现。
        $config = $this->getConfig($name);

        if (is_null($config)) {
            throw new InvalidArgumentException("Auth guard [{$name}] is not defined.");
        }

        // 自定义驱动的处理
        if (isset($this->customCreators[$config['driver']])) {
            return $this->callCustomCreator($name, $config);
        }
        
        // 默认驱动的处理，有session、token俩种默认驱动。
        $driverMethod = 'create'.ucfirst($config['driver']).'Driver';

        if (method_exists($this, $driverMethod)) {
            return $this->{$driverMethod}($name, $config);
        }

        throw new InvalidArgumentException(
            "Auth driver [{$config['driver']}] for guard [{$name}] is not defined."
        );
    }
```

解析给予的守卫。默认有session和token俩种守卫。



```php
/**
     * Create the user provider implementation for the driver.
     * 为驱动创建用户提供者
     *
     * @param  string|null  $provider
     * @return \Illuminate\Contracts\Auth\UserProvider|null
     *
     * @throws \InvalidArgumentException
     */
public function createUserProvider($provider = null)
{
    if (is_null($config = $this->getProviderConfiguration($provider))) {
        return;
    }

    if (isset($this->customProviderCreators[$driver = ($config['driver'] ?? null)])) {
        return call_user_func(
            $this->customProviderCreators[$driver], $this->app, $config
        );
    }

    switch ($driver) {
        case 'database':
            return $this->createDatabaseProvider($config);
        case 'eloquent':
            return $this->createEloquentProvider($config);
        default:
            throw new InvalidArgumentException(
                "Authentication user provider [{$driver}] is not defined."
            );
    }
}
```

为驱动创建用户提供者。默认有eloquent、database俩种默认的用户提供者。







### Guard

Guard是提供用户认证的统一接口。根据应用的不同，可以有多种不同的Guard实现。Laravel默认提供了基于Session和Token的Guard。

```php
// 定义一个守卫的基础接口
interface Guard
{
    // 确认当前用户是否已认证过的
    public function check();

    // 确认一个用户是否一个游客
    public function guest();

    // 返回当前认证过的用户
    public function user();

    // 返回当前认证过的用户的id
    public function id();

    // 验证一个用户的证书
    public function validate(array $credentials = []);

    // Set the current user.
    public function setUser(Authenticatable $user);
}
```



StatefulGuard：有状态的守卫，SessionGuard会实现该接口。

```php

// 定义有状态的守卫的接口
interface StatefulGuard extends Guard
{
    // 使用基于的证书来认证一个用户
    public function attempt(array $credentials = [], $remember = false);

    // 将用户登陆到一个没有session或cookie的应用中
	// PS：在单次请求中将用户登录到应用中，这样做将不会使用session或cookie
    public function once(array $credentials = []);

	// 将用户登陆到一个应用中。
    public function login(Authenticatable $user, $remember = false);

    /**
     * Log the given user ID into the application.
     * 通过ID将用户登录到应用。
     * @param  mixed  $id
     * @param  bool   $remember
     * @return \Illuminate\Contracts\Auth\Authenticatable
     */
    public function loginUsingId($id, $remember = false);

    /**
     * Log the given user ID into the application without sessions or cookies.
     *
     * @param  mixed  $id
     * @return bool
     */
    public function onceUsingId($id);

    /**
     * Determine if the user was authenticated via "remember me" cookie.
     * 确定用户是否通过remember me cookie进行认证
     * @return bool
     */
    public function viaRemember();

    /**
     * Log the user out of the application.
     * 将用户从一个应用中退出。
     * @return void
     */
    public function logout();
}
```



SessionGuard（Illuminate\Auth\SessionGuard）的接口实现：

```php
    /**
     * Log a user into the application.
     *
     * @param  \Illuminate\Contracts\Auth\Authenticatable  $user
     * @param  bool  $remember
     * @return void
     */
    public function login(AuthenticatableContract $user, $remember = false)
    {
        // 根据标识，将用户存储到session中。
        $this->updateSession($user->getAuthIdentifier());

        // If the user should be permanently "remembered" by the application we will
        // queue a permanent cookie that contains the encrypted copy of the user
        // identifier. We will then decrypt this later to retrieve the users.
        if ($remember) {
            $this->ensureRememberTokenIsSet($user);

            $this->queueRecallerCookie($user);
        }

        // If we have an event dispatcher instance set we will fire an event so that
        // any listeners will hook into the authentication events and run actions
        // based on the login and logout events fired from the guard instances.
        $this->fireLoginEvent($user, $remember);

        // 简单的对user属性赋值
        $this->setUser($user);
    }
```







### Provider

Provider是用户提供者，只负责根据凭证从存储系统中检索用户。

所有的Provider都要实现Illuminate\Contracts\Auth\UserProvider接口，它定义了检索如何从存储源去检索接口。而检索到的用户实例必须是实现了Illuminate\Contracts\Auth\Authenticatable接口的实例。





```php
// Illuminate\Contracts\Auth\UserProvider
interface UserProvider
{
    /**
     * Retrieve a user by their unique identifier.
     *
     * @param  mixed  $identifier
     * @return \Illuminate\Contracts\Auth\Authenticatable|null
     */
    public function retrieveById($identifier);

    /**
     * Retrieve a user by their unique identifier and "remember me" token.
     *
     * @param  mixed   $identifier
     * @param  string  $token
     * @return \Illuminate\Contracts\Auth\Authenticatable|null
     */
    public function retrieveByToken($identifier, $token);

    /**
     * Update the "remember me" token for the given user in storage.
     *
     * @param  \Illuminate\Contracts\Auth\Authenticatable  $user
     * @param  string  $token
     * @return void
     */
    public function updateRememberToken(Authenticatable $user, $token);

    /**
     * Retrieve a user by the given credentials.
     *
     * @param  array  $credentials
     * @return \Illuminate\Contracts\Auth\Authenticatable|null
     */
    public function retrieveByCredentials(array $credentials);

    /**
     * Validate a user against the given credentials.
     *
     * @param  \Illuminate\Contracts\Auth\Authenticatable  $user
     * @param  array  $credentials
     * @return bool
     */
    public function validateCredentials(Authenticatable $user, array $credentials);
}
```

retrieveByCredentials方法接收传递给Auth:attempt方法的凭证，然后该方法将根据凭证查询存储系统中的用户数据。这个方法需要返回Authenticatable实现的实例，且这个方法不能实现任何密码认证或校验。



密码认证工作交给validateCredentials()接口。





### Authenticatable

定义一个已认证过的用户数据的一系列接口，这个接口实现的用户实例是一个已经通过认证后的用户实例。



简单的说用户通过认证后，从存储系统中取出用户信息，我们需要定义统一的对外提供用户信息的接口。这个接口就是Illuminate\Contracts\Auth\Authenticatable。

```php
<?php

namespace Illuminate\Contracts\Auth;

interface Authenticatable
{
    /**
     * Get the name of the unique identifier for the user.
     * 返回主键字段的名字。
     *
     * @return string
     */
    public function getAuthIdentifierName();

    /**
     * Get the unique identifier for the user.
     * 返回用户主键值。
     *
     * @return mixed
     */
    public function getAuthIdentifier();

    /**
     * Get the password for the user.
     * 返回用户的散列密码
     *
     * @return string
     */
    public function getAuthPassword();

    /**
     * Get the token value for the "remember me" session.
     *
     * @return string
     */
    public function getRememberToken();

    /**
     * Set the token value for the "remember me" session.
     *
     * @param  string  $value
     * @return void
     */
    public function setRememberToken($value);

    /**
     * Get the column name for the "remember me" token.
     *
     * @return string
     */
    public function getRememberTokenName();
}
```





Guard::attempt()

使用UserProvider通过证书来检索用户。

调用UserProvider对比提供的证书与检索出来的用户的证书是否一致。

返回对比结果。







### Dingo

Dingo\Api\Auth\Auth:class    
	Auth类，对外提供认证接口。



Provider
    实现了Dingo\Api\Contract\Auth\Provider接口的类。
    它只有一个authenticate()接口，用于实现验证请求的逻辑。



api.auth是Dingo自己实现的中间件，它会使用Dingo\Api\Auth\Auth:class来进行验证。

['middleware' => 'api.auth', 'providers' => ['', '']]
providers是提供者列表，用于验证请求的。

我们这里不使用Dingo提供的验证方式来实现。





### 限流分析

```php
class RateLimter 
{
    // 是否有太多次的尝试
    public function tooManyAttempts(Request $request) {}
    
    // 记录一次尝试
    public function hits(Request $request) {}
}
```

