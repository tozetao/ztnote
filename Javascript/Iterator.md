迭代指按照某种逻辑，依次取出下一个数据进行处理。迭代类似于遍历。



迭代器

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
console.log(iterator.next());
console.log(iterator.next());
console.log(iterator.next());
console.log(iterator.next());
```

