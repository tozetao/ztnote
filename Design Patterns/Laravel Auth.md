### Auth分析

AuthManager

认证管理类，负责创建Guard和UserProvider对象。
认证一个用户的过程：

- 通过Auth获取配置的守卫
- 守卫根据基于的证书验证用户，通过UserProvider提供的服务来检索用户的证书，完成认证的过程。



Guard

由于登陆的方式是多种多样的，因此抽象认证用户的基础接口，以此实现不同登陆方式的认证。

在Laravel中是通过配置来决定要生成哪种类型的Guard，而用户认证依赖于UserProvider接口定义的实现。

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

