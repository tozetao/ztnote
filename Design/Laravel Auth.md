laravel用户认证的核心是由Guard（看守器）和Provider（提供器）组成。



看守器决定了如何认证每个请求中的用户，例如SessionGuard会使用Session存储和Cookie来维持状态。

提供器定义了如何从存储数据中检索用户。



SessionGuard如何验证用户？

在创建SessionGuard的时候，会注入UserProvider对象。Guard是使用提供器对象来验证的。



看看用户提供器是怎么创建的，



以及如何进行检索和验证用户。



SessionGuard怎么保持用户状态？





如果要自己实现Guard和UserProvider，那么需要通过AuthManager来进行扩展。









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





UserProvider

UserProvider实现负责从持久化存储系统中获取Authenticatable 接口的实现。它提供了如何检索用户的一系列接口。

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





Authenticatable

意为可认证的，定义了获取一个已认证过的用户数据的一系列接口，一般模型需要实现这个接口。

```php
<?php

namespace Illuminate\Contracts\Auth;

interface Authenticatable
{
    /**
     * Get the name of the unique identifier for the user.
     *
     * @return string
     */
    public function getAuthIdentifierName();

    /**
     * Get the unique identifier for the user.
     *
     * @return mixed
     */
    public function getAuthIdentifier();

    /**
     * Get the password for the user.
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

