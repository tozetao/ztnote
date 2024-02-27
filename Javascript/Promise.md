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

示例1

```js
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

示例3

```js
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

示例3.3

```js
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





测试案例：

```js
const pro1 = new Promise((resolve, reject) => {
    setTimeout(() => {
        resolve(1)
    }, 1000)
})

const pro2 = pro1.then((data) => {
    console.log(data)
    return data + 1
})

const pro3 = pro2.then((data) => {
    console.log(data)
})

console.log(pro1, pro2, pro3)

setTimeout(() => {
    console.log(pro1, pro2, pro3)
}, 2000)

问题：then()会立即执行，并把执行函数放到微队列中去，那么前一个任务的状态和数据是如何影响到后一个任务的呢?
```







source code

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

