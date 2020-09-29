

自定义Guard、UserProvider。







### Auth

在Laravel中，Auth是一个门面对象。它就是一个代理对象，Auth静态方法的调用本质上是获取容器中注册的AuthManager对象来调用的。





### AuthManager

认证管理类。提供创建看守器、提供器对象的接口，也提供注册自定义看守器和提供器的接口。

```php
interface Factory
{
    public function guard($name = null);
    
    public function shouldUse($name);
}
```

AuthManager所实现的接口。







### Guard

Guard是提供用户认证的统一接口。

根据应用的不同，可以有多种不同的Guard实现。Laravel默认提供了基于Session和Token的Guard。

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

SessionGuard

```php

// 定义有状态的守卫的接口
interface StatefulGuard
{
    // 使用基于的证书来认证一个用户
    public function attempt(array $credentials = [], $remember = false);

    // 将用户登陆到一个没有session或cookie的应用中
    public function once(array $credentials = []);

	// 将用户登陆到一个应用中。
    public function login(Authenticatable $user, $remember = false);

    /**
     * Log the given user ID into the application.
     *
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
     *
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





### UserProvider

UserProvider是Illuminate\Contracts\Auth\UserProvider接口。

UserProvider是用户提供者，只负责根据用户凭证从存储系统中检索用户。所检索到的用户实例必须实现了Illuminate\Contracts\Auth\Authenticatable接口。



注意：这里的Provider不要跟Laravel的服务提供者搞混了，服务提供者只是在系统启动时初始化对应的服务对象而已。这里的UserProvider是一个用户检索对象。

```php
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

retrieveByCredentials方法接收传递给Auth:attempt方法的凭证数组。然后该方法将根据凭证查询存储系统中的用户数据。这个方法需要返回Authenticatable实现的实例，且这个方法不能实现任何密码认证或校验。



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

