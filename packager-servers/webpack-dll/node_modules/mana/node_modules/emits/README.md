# emits

[![Version npm][version]](http://browsenpm.org/package/emits)[![Build Status][build]](https://travis-ci.org/primus/emits)[![Dependencies][david]](https://david-dm.org/primus/emits)[![Coverage Status][cover]](https://coveralls.io/r/primus/emits?branch=master)[![IRC channel][irc]](https://webchat.freenode.net/?channels=primus)

[version]: https://img.shields.io/npm/v/emits.svg?style=flat-square
[build]: https://img.shields.io/travis/primus/emits/master.svg?style=flat-square
[david]: https://img.shields.io/david/primus/emits.svg?style=flat-square
[cover]: https://img.shields.io/coveralls/primus/emits/master.svg?style=flat-square
[irc]: https://img.shields.io/badge/IRC-irc.freenode.net%23primus-00a8ff.svg?style=flat-square

## Installation

This module is compatible with browserify and node.js and is therefore released
through npm:

```
npm install --save emits
```

## Usage

In all examples we assume that you've assigned the `emits` function to the
prototype of your class. This class should inherit from an `EventEmitter` class
which uses the `emit` function to emit events. For example:

```js
'use strict';

var EventEmitter = require('events').EventEmitter
  , emits = require('emits');

function Example() {
  EventEmitter.call(this);
}

require('util').inherits(Example, EventEmitter);

//
// You can directly assign the function to the prototype if you wish or store it
// in a variable and then assign it to the prototype. What pleases you more.
//
Example.prototype.emits = emits; // require('emits');

//
// Also initialize the example so we can use the assigned method.
//
var example = new Example();
```

Now that we've set up our example code we can finally demonstrate the beauty of
this functionality. To create a function that emits `data` we can simply do:

```js
var data = example.emits('data');
```

Every time you invoke the `data()` function it will emit the `data` event with
all the arguments you supplied. If you want to "curry" some extra arguments you
can add those after the event name:

```js
var data = example.emits('data', 'foo');
```

Now when you call `data()` the `data` event will receive `foo` as first argument
and the rest of the arguments would be the ones that you've supplied to the
`data()` function.

If you supply a function as the last argument we assume that this is an async
argument parser. This allows you to modify the arguments, prevent the event from
being fired or just clear all supplied arguments (except for the ones that are
curried in). The first argument of the function is always the callback function,
all other arguments after that are the ones emitted with the event. The callback
function follows the usual error first pattern. When the callback is invoked
with an error it will emit an `error` event on the `EventEmitter` instance. In
our case the `example` instance:

```js
var data = example.emits('data', function parser(next, arg) {
  try { arg = JSON.parse(arg); }
  catch (e) { return next(e); }

  next(undefined, arg);
});
```

To modify the data you need to supply the change as second argument:

```js
var data = example.emits('data', function parser(next, arg) {
  next(undefined, 'bar');
});
```

In the example above we've transformed the incoming argument to `bar`. So when
you call `data()` it will emit a `data` event with `bar` as the second argument.
If you call the callback with `undefined` as second argument we assume that no
modifications have been made and we emit all received arguments. If you want to
clear all received arguments, call the callback with `null`:

```js
var data = example.emits('data', function parser(next, arg) {
  next(undefined, null);
});
```

### Patterns

In Primus the most common pattern for this module is to proxy events from one
instance to another:

```js
eventemitter.on('data', example.emits('data'));
```

It is also very useful to re-format data. For example, in the case of WebSockets,
if we don't want to reference `evt.data` every time we need to access the data,
we can parse the argument as following:

```js
var ws = new WebSocket('wss://example.org/path');
ws.onmessage = example.emits('data', function parser(next, evt) {
  next(undefined, evt.data);
});
```

In the example above we will now emit the `data` event with a direct reference
to `evt.data`. The following final example shows how you can prevent events
from being emitted.

```js
var ws = new WebSocket('wss://example.org/path');
ws.onmessage = example.emits('data', function parser(next, evt) {
  var data;

  try { data = JSON.parse(evt.data); }
  catch (e) { return next(e); }

  if ('object' !== typeof data || Array.isArray(data)) return;

  next(undefined, data);
});
```

By not calling the callback we make sure that the event is not emitted. So the
`data` event will only be fired if we've received a valid JSON document from the
server and it's an object.

## License

MIT
