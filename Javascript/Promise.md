Promise是一套专门用于处理异步场景的规范，它能够有效避免回调地狱的产生，使异步代码更加清晰简洁。

这套规范最早诞生于前端社区，成为Promise A+，规范规定：

1. 所有的异步场景，都可以看作是一个异步任务，每个异步任务，在JS中应该表现为一个对象，该对象称为Promise对象，也叫做任务对象。

   比如远程登录、发送表白短信、延时弹窗，这些异步任务都可以用Promise表示。

2. 每个任务对象，都应该有2个阶段，3个状态。

   2个阶段指的是未决阶段（unsettled）、已决阶段（settled）。在未决阶段中Promise处于挂起（pending）状态；在已决阶段，Promise可以是完成状态（fulfilled），或者是失败状态（rejected）。

   同时规范规定了：

   - 任务总是从未决阶段变为已决阶段，无法逆行。
   - 任务总是从挂起状态变为完成或者是失败状态，无法逆行。
   - 任务一旦完成或者失败，状态就固定下来，是不会再次发生变化了。

3. 从挂起到完成状态（fulfilled），成为resolve；从挂起到失败状态，称为reject。任务完成时，可能有一个相关数据。任务失败时，可能有一个失败原因。
4. 可以针对任务进行后续处理，针对完成状态的后续处理称之为onFulfilled。针对失败的后续处理称之为onRejected。



ES6提供了一套API，实现了Promise A+规范。基本使用如下：

```js
const pro = new Promise((resolve, reject) => {
    // 任务的具体执行过程，该函数会立即执行
    
    // 调用resolve(data)，可以将任务变为fulfillied状态，data为传递的数据。
    // 调用reject(error)，可以将任务变为rejected状态，error时要传递的错误。
});

pro.then(
    (data) => {
        // onFulfilled
    },
    (reason) => {
        // onRejcted
    }
)
```

example:

```js
const task = new Promise((resolve, reject) => {
	console.log('开始执行任务')
    const duration = Math.floor(Math.random() * 5000)
    setTimeout(() => {
        if (Math.random() >= 0.5) {
            resolve(duration)
        } else {
            reject('这里填写失败原因')
        }
    }, duration)
})

task.then(
    // 任务执行成功时的回调
    (duration) => {
        console.log('成功: ', duration)
    },
    // 任务执行失败时的回调
    (reason) => {
        console.log('失败：', reason)
    }
)


/**
  * 延迟一段时间。
  * @param {Number} duration 等待的时间
  * @returns {Promise} 返回一个任务，该任务在指定的时间后完成
  */
function delay(duration) {
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            resolve()
        }, duration)
    })
}
```



### then

```js
function then(onFulfilled, onRejected)
```

then()方法会执行onFulfilled，onRejected回调函数，用于执行任务成功或失败时的回调。then()方法一定会返回一个Promise，可以理解为后续也是一个任务。

返回的任务状态取决于后续处理：

1. 若没有相关的后续处理，新任务的状态和前任务一致，数据为前任务的数据。

2. 若有后续处理但未执行，新任务挂起。

3. 若后续处理了，则根据后续任务处理的情况确定新任务的状态。

   3.1 后续任务处理无错，新任务的状态为完成，数据为后续处理的返回值

   3.2 后续处理执行有错，新任务的状态为失败，数据为异常对象。

   3.3 后续执行后返回的是一个任务对象，新任务的状态和数据与该任务对象一致

以下是各种后续处理的示例：

```js
// 示例1
// 学习 考试 出成绩 填志愿 入学
const pro1 = new Promise((resolve, reject) => {
	console.log('学习')
    console.log('但是中彩票了')
    reject(new Error('中奖了'))
})

const pro2 = pro1.then(() => {
    console.log('考试')
})

// 由于没有上一个任务（pro1）的状态处理函数（onReject）,因此pro2任务的状态是reject，数据也是pro1失败的数据。
setTimeout(() => {
    console.log(pro2)
});
```

example: 示例2

```js
// 示例2
const pro1 = new Promise((resolve, reject) => {
	console.log('学习')
    setTimeout(() => {
        resolve()
    }, 2000)
})

// 前面的任务未处理完毕，处于pengding状态，那么后续的任务也都会处理pengding状态
const pro2 = pro1.then(() => {
    console.log('考试')
})
setTimeout(() => {
    console.log(pro2)
}, 1000)
```



```js
// 示例3
const pro1 = new Promise((resolve, reject) => {
	console.log('学习')
    resolve('1')
})

// 3.1 有处理pro1任务的状态，同时也没有报错，因此pro2任务的状态是成功的（fulfilled），由于没有返回数据，pro2的数据是undefined
const pro2 = pro1.then(() => {
    console.log('考试')
    // 3.2 主动抛出异常，会发现pro2任务变成失败了（rejected），错误原因即时异常原因。
	// throw new Error('考试睡着了')
})
setTimeout(() => {
    console.log(pro2)
}, 1000)
```



```js
// 示例3.3
const pro1 = new Promise((resolve, reject) => {
	console.log('学习')
    resolve('1')
})

// pro2等同于返回的Promise()，在这里，返回的Promise是pending状态，因此pro2也是pending状态，数据是undefined
const pro2 = pro1.then(() => {
    return new Promise((resolve, reject) => {
    })
})
setTimeout(() => {
    console.log(pro2)
}, 1000)
```







**链式调用的原理分析**

```js
const Pending = 'Pending'
const Rejected = 'Rejected'
const Fulfilled = 'Fulfilled'

class MyPromise {
    state = Pending
    value = undefined
    
	handlers = []
    
    _resolve(data) {}
    _reject(reason) {}
    
    construct(callback) {
        try {
            callback(this._resolve.bind(this), this._reject.bind(this))
        } catch(error) {
            this._reject(error)
        }
    }
    
    then(onFulfilled, onRejected) {
        // 关键点：
        // 这里是设置当前任务状态改变后要执行的回调函数。
        // this指向当前对象，resolve, reject是由实例化的对象传递进来的，指向实例化的对象，以此形成了一个任务链。
        return new MyPromise((resolve, reject) => {
            this._pushHandler({
                onFulfilled, resolve, reject
            })
            this._pushHandler({
                onRejected, resolve, reject
            })
            // ...
        })
    }
}

Promise1
  handlers
  	handler [onFulfilled, onRejected, resolve, reject]

resolve => Promise2
reject  => Promise2

通过js的bind函数，构建了一个链表，以此来实现任务的链式处理。
```







### then()使用示例

有了then()方法，就可以进行链式调用，以此来消除回调地狱。

```js
// 常见的任务处理
const pro = new Promise();

// 任务成功后，执行处理1，失败后执行处理2
pro.then(onFulfilled).catch(onRejected)

// 任务成功后，执行onFulfilled1，onFulfilled1执行成功后，再执行onFulfilled2
pro.then(onFulfilled1).then(onFulfilled2)

// 任务成功后，依次执行onFulfilled1、onFulfilled2。如果其中某个出错，就执行onRejected错误处理。
pro.then(onFulfilled1)
    .then(onFulfilled2)
    .catch(onRejected)
```



```js
function sendMessage(name) {
    console.log(`开始给 ${name} 发送消息`);
    // 模拟请求阻塞
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            if (Math.random() > 0.5) {
                resolve('success')
            } else {
                reject('failed')
            }
        }, 1000)
    })
}

// 失败将会重试3次
const name = 'Lee'
const reply = (reply) => {
    // 重试1次
    console.log('reply')
    return sendMessage(name)
}
sendMessage(name)
	.catch(reply)
	.catch(reply)
	.catch(reply)
    .then(response => {
    	console.log('success: ', response)
	})
```



### async

async用于修饰函数。一个函数如果被async修饰过，该函数将会返回一个Promise。

```js
// 示例1
async function foo() {
    return 1
}
// Promise{<fulfilled>: 1}
console.log(foo())


// 示例2：如果async修饰的函数执行过程中出错，返回的Promise状态是reject。
async function foo() {
	throw 'error 1'
}
// Promise{<reject>: 'error 1'}
console.log(foo())
```

如果被async修饰过的函数返回值是一个Promise，那么JS会进行特殊处理，函数仍然会返回一个Promise，该Promise的状态和数据最终会和函数内返回的Promise的状态和数据相同。

```js
async function foo() {
    // return Promise.resolve(1)
    return new Promise((resolve) => {
        return resolve(1)
    })
}

const pro = foo();

// Promise{<pending>}
console.log(pro)

setTimeout(() => {
    // Promise{<fulfilled>: 1}，最终状态是fulfilled，数据是1
    console.log(pro)  
}, 1000)
```



### await

await表示等待某个Promise完成，它必须用于async函数中。

```js
function sendMessage(message) {
    return new Promise((resolve) => {
        return resolve(100)
    })
}

async function foo() {
    const result = await sendMessage('hi!')
    console.log(result)
}
```

由于await是等待任务的完成，如果要处理失败的任务，需要使用try catch：

```js
function sendMessage(message) {
  return new Promise((resolve, reject) => {
    return reject('empty data')
  })
}

async function foo() {
  try {
    const result = await sendMessage('hi!')
    console.log('result: ', result)
  } catch (err) {
    console.log(err)
  }
}

foo()
```



async和await示例代码：

```js
function sendMessage(name, content) {
    return Promise((resolve, reject) => {
        console.log(`开始向${name}发送消息`)
        setTimeout(() => {
            if (Math.random() > 0.5) {
                resolve('success')
            } else {
                reject('failed')
            }
        }, 2000)
    })
}

const array = [
    {name: 'a', 'content': 'a1'},
    {name: 'b', 'content': 'a1'},
    {name: 'c', 'content': 'a1'},
    {name: 'd', 'content': 'a1'},
    // ...
];

// 使用数组中的元素发送消息，失败重发，如果某个成功了则通知发送消息。
(async () => {
    for (const item of array) {
        try {
            const result = await sendMessage(item.name, item.content)
            console.log('success: ', result)
            break;
        } catch (err) {
            console.log('error: ', err)
        }
    }
})()
```




