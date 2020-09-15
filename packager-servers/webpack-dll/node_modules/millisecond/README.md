# millisecond

[![Made by unshift][made-by]](http://unshift.io)[![Version npm][version]](http://browsenpm.org/package/millisecond)[![Build Status][build]](https://travis-ci.org/unshiftio/millisecond)[![Dependencies][david]](https://david-dm.org/unshiftio/millisecond)[![Coverage Status][cover]](https://coveralls.io/r/unshiftio/millisecond?branch=master)[![IRC channel][irc]](https://webchat.freenode.net/?channels=unshift)

[made-by]: https://img.shields.io/badge/made%20by-unshift-00ffcc.svg?style=flat-square
[version]: https://img.shields.io/npm/v/millisecond.svg?style=flat-square
[build]: https://img.shields.io/travis/unshiftio/millisecond/master.svg?style=flat-square
[david]: https://img.shields.io/david/unshiftio/millisecond.svg?style=flat-square
[cover]: https://img.shields.io/coveralls/unshiftio/millisecond/master.svg?style=flat-square
[irc]: https://img.shields.io/badge/IRC-irc.freenode.net%23unshift-00a8ff.svg?style=flat-square

Parse strings that indicate a time to their millisecond equivalents.

## Installation

This module is written with Node.js and Browserify in mind and can therefor be
installed using the node package manager:

```
npm install --save millisecond
```

## Usage

The module exposes one single function interface, so you simply require it
using:

```js
'use strict';

var ms = require('millisecond');
```

And to parse a string simply supply it as first argument and it will return
a number. If we're unable to parse it, we will automatically return `0`.

```js
ms('1 second'); // returns 1000
ms('1 ms');     // returns 1
ms('10 cows');  // returns 0
```

It understands the following strings:

- `x milliseconds`
- `x millisecond`
- `x msecs`
- `x msec`
- `x ms`
- `x seconds`
- `x second`
- `x secs`
- `x sec`
- `x s`
- `x minutes`
- `x minute`
- `x mins`
- `x min`
- `x m`
- `x hours`
- `x hour`
- `x hrs`
- `x hr`
- `x h`
- `x days`
- `x day`
- `x d`
- `x weeks`
- `x week`
- `x wks`
- `x wk`
- `x w`
- `x years`
- `x year`
- `x yrs`
- `x yr`
- `x y`

The space after the number is optional so you can also write `1ms` instead of `1
ms`. In addition to that it also accepts numbers and strings which only includes
numbers and we assume that these are always in milliseconds.

## License

MIT

This module is heavily inspired by the `ms` module which is also licensed under
MIT. If you also need to transform numbers back in to strings I suggest you look
at that library.
