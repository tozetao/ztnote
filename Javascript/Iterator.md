迭代指按照某种逻辑，依次取出下一个数据进行处理。迭代类似于遍历。



#### 迭代器

JS语言规定，如果一个对象具有next方法，并且next方法满足一定约束，那么它就是一个迭代器。

next方法的约束：

- next方法必须返回一个包含done和value属性的对象。
- done属性表示迭代是否完成。

- value表示当前迭代的数据，当迭代完成，value的值一般未undefined。

通过迭代器的next方法，可以依次取出数据，并且可以根据done属性来判断是否迭代结束。

```js
// 一个简单的迭代器
var iterator = {
    index: 1,
    prev: 1,
    current: 1,
    next() {
        if (this.index === 1 || this.index === 2) {
            this.index++;
            return {
                value: 1,
                done: false
            }
        }
        this.index++;

        const temp = this.prev + this.current
        this.prev = this.current
        this.current = temp

        return {
            value: temp,
            done: false
        }
    }
}

console.log(iterator.next());
console.log(iterator.next());

// 创建一个数组迭代器
function createIterator(array) {
    return {
        index: 0,
        next() {
            return {
                value: array[this.index++],
                done: this.index <= array.length
            }
        }
    }
}

var it = createIterator([1,2,3,4])
console.log(it.next())
```



#### 可迭代协议

ES6的for-of用于迭代对象，它要求对象是可迭代的。可迭代协议要求：对象必须要有一个知名符号（Symbol.iterator）属性，同时该属性必须是一个无参的迭代器创建函数。

```js
const iterator = {
    [Symbol.iterator]: function() {
        return {
            array: [1,2,3,4,5,6,6,7,87,8],
            index: 0,
            next() {
                const obj = {
                    value: this.array[this.index++],
                    done: this.index > this.array.length
                }
                return obj
            }
        }
    }
}
for (const item of iterator) {
    console.log(item)
}
```

for-of的原理就是使用Symbol.iterator符号属性获取一个迭代器对象，不断地调用迭代器对象的next()方法获取数据，直到结束。





#### Generator

生成器（Generator）既是一个迭代器（有next方法），同时也是满足迭代协议的对象（可以通过for-of进行迭代）。

> 生成器被设计为，通过生成器可以控制生成器函数内的代码执行时机。yield关键字配合生成器的next()、throw()、return()等方法来控制代码的执行时机。



ES6使用了一个新的语法来提供生成器。

```js
// 返回一个新的生成器
function *createGenerator() {}

var generator = createGenerator()

// 在浏览器进行验证
generator.next === generator[Symbol.iterator]().next
```





生成器函数yield关键字使用，它的特点如下：

- 调用生成器函数时，函数内部的代码是不会执行的。代码的执行由返回所返回的生成器控制。
- 每当生成器的next()方法执行时，函数内的代码会由上一个yield的位置运行到下一个yield的位置。
- yield表示暂停，并且yield后面的值会作为返回值。

```js
  // 返回一个新的生成器
function *createGenerator() {
    console.log('start')
    yield;
    console.log(1);
    yield;
    console.log(2);
    yield;
    console.log(3);
    yield;
    console.log(4);
}

var generator = createGenerator()
// generator.next()
/*
generator(生成器)每次调用next()时，都会从停留在上一次的yield的代码出向下执行，直到碰到下一个yield停止。
生成器依次调用next()，直到函数内的代码执行完毕。

yield每次会返回{ value: '', done: boolean }的对象，当函数内的代码执行完毕时，返回{ done: true, value: undefined }的对象。
*/
```



**yield与return**

```js
// 注：在生成器函数中，return关键字可以提供结束生成器的执行。
function *createGenerator() {
    console.log('start')
    yield;
    console.log(1);
    yield;
    console.log(2);
    yield;
    
    // 当generator代码执行到这里时，会返回undefined，后面的代码不会执行
    return;
    
    console.log(3);
    yield;
    console.log(4);
}
```



**next方法的参数**

```js
// next传递的值会作为上一次停留的yield的返回值。
function *createGenerator() {
    console.log('start')

    const result1 = yield 1;
    console.log('receive result1: ', result1)       // B

    const result2 = yield 2;
    console.log('receive result1: ', result2)       // C
}

const g = createGenerator()
console.log(g.next('A'))
console.log(g.next('B'))
console.log(g.next('C'))
console.log(g.next())
```



**thorw方法**

生成器提供了throw方法，一旦在生成器执行过程中调用该方法，可以中断生成器的执行。

```js
// 生成器的throw方法 可以中断 
function *createGenerator() {
    try {
        console.log('start')
        yield;
        console.log(1);
        yield;
        console.log(2);
        yield;    
        console.log(3);
        yield;
        console.log(4);
    } catch(err) {
        console.log(err)
    }
}

const g = createGenerator()

console.log(g.next())
g.throw(new Error('not found.'))
console.log(g.next())
```



**return方法**







示例：使用生成器来遍历数组。

```js
function *createArrayIterator(array) {
    // 当generator没有调用next()时，这个循环体是不会执行的。
    for (let item of array) {
        yield item;
    }
}

const iterator = createArrayIterator([1,2,3,4,5,6,7,8,9,10])
for (const item of iterator) {
    console.log(item)        
}
```



示例：类似async、awit效果

```js
function fetchUsers() {
    return new Promise(resolve => {
        setTimeout(() => {
            resolve([
                { id: 1, name: 'Test Name', rand: Math.random() }
            ])
        }, 2000)
    })
}

// 实现类似于async、awit的效果
function *createTask() {
    console.log('start')
    const result = yield fetchUsers()
    console.log('I got a result, ', result)

    const result1 = yield 10
    console.log('result1: ', result1)

    const result2 = yield fetchUsers()
    console.log('result2: ', result2)
}

function run(generatorFn) {
    const generator = generatorFn()

    function next(nextValue) {
        const result = generator.next(nextValue)
        if (result.done) {
            return;
        }

        if ((!!result.value) && typeof result.value.then === 'function') {
            result.value.then(response => next(response))
        } else {
            next(result.value)
        }
    }

    next()
}


run(createTask);
```











