# glob-promise [![version][npm-version]][npm-url] [![License][license-image]][license-url]

[`Promise`][Promise] version of [`glob`][glob]

> Match files using the patterns the shell uses, like stars and stuff.

[![Build Status][travis-image]][travis-url]
[![Downloads][npm-downloads]][npm-url]
[![Code Climate][codeclimate-quality]][codeclimate-url]
[![Coverage Status][codeclimate-coverage]][codeclimate-url]
[![Dependency Status][dependencyci-image]][dependencyci-url]
[![Dependencies][david-image]][david-url]

## Install

```bash
npm install --only=production --save glob-promise
```

## API

### `glob(pattern [, options])`

Alias for `glob.promise`

### `glob.promise(pattern [, options])`

_pattern_: `String` (glob pattern)
_options_: `Object` or `String`
Return: `Object` ([Promise])

When it finishes, it will be [_fulfilled_](http://promisesaplus.com/#point-26) with an `Array` of filenames as its first argument.

When it fails to read the files, it will be [_rejected_](http://promisesaplus.com/#point-30) with an error as its first argument.

```js
glob('**/*')
  .then(function(contents) {
    contents; //=> ['lorem', 'ipsum', 'dolor']
  });

glob('{foo,bar.baz}.txt', { nobrace: true })
  .then(function(contents) {
    contents; //=> []
  });
```

### `glob.glob(pattern [, options], cb)`

> see [`glob`](https://github.com/isaacs/node-glob#globpattern-options-cb)

### `glob.sync(pattern [, options])`

> see [`glob.sync()`](https://github.com/isaacs/node-glob#globsyncpattern-options)

### `glob.hasMagic(pattern, [options])`

> see [`glob.hasMagic()`](https://github.com/isaacs/node-glob#globhasmagicpattern-options)

### `Class: glob.Glob`

> see [`Glob`](https://github.com/isaacs/node-glob#class-globglob)


#### options

The option object will be directly passed to [glob](https://github.com/isaacs/node-glob#options).

---
> :copyright: [ahmadnassri.com](https://www.ahmadnassri.com/)  · 
> License: [ISC][license-url]  · 
> Github: [@ahmadnassri](https://github.com/ahmadnassri)  · 
> Twitter: [@ahmadnassri](https://twitter.com/ahmadnassri)

[license-url]: http://choosealicense.com/licenses/isc/
[license-image]: https://img.shields.io/github/license/ahmadnassri/glob-promise.svg?style=flat-square

[travis-url]: https://travis-ci.org/ahmadnassri/glob-promise
[travis-image]: https://img.shields.io/travis/ahmadnassri/glob-promise.svg?style=flat-square

[npm-url]: https://www.npmjs.com/package/glob-promise
[npm-version]: https://img.shields.io/npm/v/glob-promise.svg?style=flat-square
[npm-downloads]: https://img.shields.io/npm/dm/glob-promise.svg?style=flat-square

[codeclimate-url]: https://codeclimate.com/github/ahmadnassri/glob-promise
[codeclimate-quality]: https://img.shields.io/codeclimate/github/ahmadnassri/glob-promise.svg?style=flat-square
[codeclimate-coverage]: https://img.shields.io/codeclimate/coverage/github/ahmadnassri/glob-promise.svg?style=flat-square

[david-url]: https://david-dm.org/ahmadnassri/glob-promise
[david-image]: https://img.shields.io/david/ahmadnassri/glob-promise.svg?style=flat-square

[dependencyci-url]: https://dependencyci.com/github/ahmadnassri/glob-promise
[dependencyci-image]: https://dependencyci.com/github/ahmadnassri/glob-promise/badge?style=flat-square

[glob]: https://github.com/isaacs/node-glob
[Promise]: http://promisesaplus.com/
