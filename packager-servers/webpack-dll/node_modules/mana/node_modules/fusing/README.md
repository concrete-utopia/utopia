# fusing

[![From bigpipe.io][from]](http://bigpipe.io)[![Version npm][version]](http://browsenpm.org/package/fusing)[![Build Status][build]](https://travis-ci.org/bigpipe/fusing)[![Dependencies][david]](https://david-dm.org/bigpipe/fusing)[![Coverage Status][cover]](https://coveralls.io/r/bigpipe/fusing?branch=master)

[from]: https://img.shields.io/badge/from-bigpipe.io-9d8dff.svg?style=flat-square
[version]: http://img.shields.io/npm/v/fusing.svg?style=flat-square
[build]: http://img.shields.io/travis/bigpipe/fusing/master.svg?style=flat-square
[david]: https://img.shields.io/david/bigpipe/fusing.svg?style=flat-square
[cover]: http://img.shields.io/coveralls/bigpipe/fusing/master.svg?style=flat-square

Fusing is a small library that creates the base class that is used in all of
[bigpipe]'s components. It takes care of:

- Prototypical inheritance.
- An API for adding `readable` and `writable` properties to these classes.
- Adding default methods which are commonly used.
- A `Backbone.extend` based extending of the prototypes.

## Installation

The stable versions of this module are released in the npm registry and can be
installed using:

```
npm install --save fusing
```

The `--save` tells `npm` to automatically save this dependency in your
`package.json`.

## Getting Started

The module is required just like any other module you use. It exposes a single
function that takes care of all the merging.

```js
'use strict';

var fuse = require('fusing');
```

And that is all we need to start with inheritance. When you want to have a class
inherit from the `EventEmitter` you only need to pass in the class references:

```js
function Example() {

}

fuse(Example, require('events').EventEmitter);
```

This will tell `fuse` to use the `.prototype` of the `EventEmitter` for your
`Example` class. In addition to that it has added a couple of function to your
class which makes it easier to setup the prototypes and extend Example again.

### Example.readable

One of the functions that are added to your class is `readable` this allows you
to easily specify which properties or methods on the `Example.prototype` are
`read-only` and should never be overridden by other code. This is ideal for
protecting your private methods.

```js
Example.readable('config', { foo: 'bar' });
```

The example above added the property `config` to the prototype with the foo/bar
object as value. If you wonder how this magic works, take a look a our
[predefine] project for more details.

**Please note that this function is added on the `Example` function not on the
`Example.prototype`.**

### Example.writable

This is the writable equivalent of the function above. This allows you to
specify properties on the prototype that are writable. The added benefit of this
function is that your methods will not be enumerable (which is also true for all
properties/methods added using the `readable` function).

```js
Example.writable('property', 'foo');
```

**Please note that this function is added on the `Example` function not on the
`Example.prototype`.**

### Example.get

Add a getter to the prototype.

```js
var foo = 'bar'
Example.get('property', function () {
  return foo;
});
```

**Please note that this function is added on the `Example` function not on the
`Example.prototype`.**

### Example.set

Add a getter AND a setter to the prototype.

```js
var foo = 'bar'
Example.set('property', function () {
  return foo;
}, function (value) {
  return foo = value;
});
```

**Please note that this function is added on the `Example` function not on the
`Example.prototype`.**

### Example.extend

This allows you to use the same `extend` functionality that you might be
accustomed to with `Backbone` in your own classes:

```js
var MyExample = Example.extend({
  method: function method() {
    console.log('my custom method');
  },

  prop: 132
});
```

**Please note that this function is added on the `Example` function not on the
`Example.prototype`.**

### Example.predefine

As it's sometimes useful to also create readable and writable properties when
your class is constructed, we decided to expose the `predefine` module on your
class. Which allows you use the same readable pattern again:

```js
function Example() {
  var writable = Example.predefine(this, Example.predefine.WRITABLE)
    , readable = Example.predefine(this);

  readable('private', 134);
  readable('evn', process.ENV.NODE_ENV || 'development');
  writable('value', 100);
}

fuse(Example, require('eventemitter3'));
```

**Please note that this function is added on the `Example` function not on the
`Example.prototype`.**

## License

MIT

[bigpipe]: https://github.com/bigpipe/bigpipe
[predefine]: https://github.com/bigpipe/predefine
