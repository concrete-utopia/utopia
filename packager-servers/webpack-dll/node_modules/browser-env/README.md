# browser-env

> Simulates a global browser environment using [`jsdom`](https://github.com/tmpvar/jsdom).

[![Build Status](https://travis-ci.org/lukechilds/browser-env.svg?branch=master)](https://travis-ci.org/lukechilds/browser-env) [![Coverage Status](https://coveralls.io/repos/github/lukechilds/browser-env/badge.svg?branch=master)](https://coveralls.io/github/lukechilds/browser-env?branch=master) [![npm](https://img.shields.io/npm/dm/browser-env.svg)](https://www.npmjs.com/package/browser-env)

This allows you to run browser modules in Node.js 4 or newer with minimal or no effort. Can also be used to test browser modules with any Node.js test framework. Please note, only the DOM is simulated, if you want to run a module that requires more advanced browser features (like `localStorage`), you'll need to polyfill that seperately.

> ❗️**Important note**
>
> This module adds properties from the `jsdom` window namespace to the Node.js global namespace. This is explicitly [recommended against](https://github.com/tmpvar/jsdom/wiki/Don't-stuff-jsdom-globals-onto-the-Node-global) by `jsdom`. There may be scenarios where this is ok for your use case but please read through the linked wiki page and make sure you understand the caveats. If you don't need the browser environment enabled globally, [`window`](https://github.com/lukechilds/window) may be a better solution.

## Install

```shell
npm install --save browser-env
```

Or if you're just using for testing you'll probably want:

```shell
npm install --save-dev browser-env
```

## Usage

```js
// Init
require('browser-env')();

// Now you have access to a browser like environment in Node.js:

typeof window;
// 'object'

typeof document;
// 'object'

var div = document.createElement('div');
// HTMLDivElement

div instanceof HTMLElement
// true
```

By default everything in the `jsdom` window namespace is tacked on to the Node.js global namespace (excluding existing Node.js properties e.g `console`, `setTimout`). If you want to trim this down you can pass an array of required properties:

```js
// Init
require('browser-env')(['window']);

typeof window;
// 'object'

typeof document;
// 'undefined'
```

You can also pass a config object straight through to `jsdom`. This can be done with or without specifying required properties.

```js
require('browser-env')(['window'], { userAgent: 'My User Agent' });

// or

require('browser-env')({ userAgent: 'My User Agent' });
```

You can of course also assign to a function:

```js
var browserEnv = require('browser-env');
browserEnv();

// or

import browserEnv from 'browser-env';
browserEnv();
```

## Related

- [`window`](https://github.com/lukechilds/window) - Exports a jsdom window object

## License

MIT © Luke Childs
