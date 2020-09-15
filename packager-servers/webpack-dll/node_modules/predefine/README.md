# predefine

[![Build Status](https://travis-ci.org/bigpipe/predefine.png)](https://travis-ci.org/bigpipe/predefine)

When creating objects or prototypes using `Object.defineProperties` or
`Object.defineProperty` it make your code look really verbose by all the
property descriptions that it needs. Most of the time, they are the same. They
either make your properties writable, readable or prevents them from being
enumerable. So basically:

Predefine makes `Object.defineProperties` your human readable and manageable.

## Installation

```
npm install --save predefine
```

## Getting started

Let's start with a basic example of predefine usage:

```
var predefine = require('predefine');

function Base() {
  var readable = predefine(this, { configurable: false, enumerable: false })
    , writable = predefine(this, predefine.WRITABLE);

  readable('prop', 'value');
  writable('data', []);
}

Base.writable = predefine(Base.prototype, predefine.WRITABLE);

Base.writable('foo', 'bar');
```

As you can see from the snippet above, it's really easy to see which properties
are made readable and which one's are writable.

### Predefine.extend

This allows you to add `Backbone` inspired `.extend` functionality to your
constructors. This makes inheriting a lot easier and readable. See the
`extendible` module in npm for information.

```js
function Foo() {}
Foo.extend = predefine.extend;

var Bar = Foo.extend({
  method: function () {}
});
```

### Predefine.descriptor

Test if a given object is a valid `Object` description to it can be used with
`Object.defineProperty`, `Object.defineProperties` and `Object.create`.

```js
predefine.descriptor({ foo: 'bar' });                       // false
predefine.descriptor({ value: 'bar' });                     // true
predefine.descriptor({ value: 'bar', enumerable: false });  // true
predefine.descriptor({ value: 'bar', foo: 'bar' });         // false
```

### Predefine.create

This is a simple helper function to create descriptions that can be used within
`Object.create` and `Object.defineProperties`.

```js
var data = Object.create(null, predefine.create('foo', { 
  value: 'bar' 
}));

var data = Object.create(null, predefine.create('foo', {
  value: 'bar'
}, predefine.READABLE));

var data = Object.create(null, predefine.create('foo', 'bar', predefine.READABLE));
```

### Predefine.remove

Removes all enumerable properties from a given object, with the ability to keep
white listed properties.

```js
var data = { foo: 'bar', bar: 'foo' };

predefine.remove(data);           // The data variable is now an empty object.
predefine.remove(data, ['foo']);  // The foo property is kept.

var readable(data);
readable('baz', 'baz');

predefine.remove(data);           // Only `baz` is left.
```

### Predefine.merge

Merge two objects in to one single object. This supports deep merging.

```js
var result = predefine.merge({ foo: 'bar' }, { bar: 'foo' });
```

### Predefine.mixin

Mixin two Objects, which also transfers properties that are set using
Object.defineProperty.

```js
var result = predefine.mixin({ foo: 'bar' }, { bar: 'foo' });
```

## License

MIT
