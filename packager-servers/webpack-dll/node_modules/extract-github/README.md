# extract-github

[![Build Status](https://travis-ci.org/3rd-Eden/extract-github.png)](https://travis-ci.org/3rd-Eden/extract-github)

Extract a valid github URL from a given object. We make the assumption that the
given object follows a `package.json` format as used by npm.

## Installation

This module is released in the npm registry as `extract-github` and can therefor
be installed using:

```
npm install --save extract-github
```

## Usage

This module was written to only do specific task and do that task well, and that
is extracting github information. We therefor export as a single function:

```js
'use strict';

var extract = require('extract-github');
```

Once you've required the module you can feed it the contents of a package.json
to extract to github information.

```js
var github = extract(require('./package.json'));
```

The `github` variable should now contain an object with a `user` and `repo`
property which can be re-used to create a new github URL. In addition to parsing
package.json's it also supports extracting github information out of `README` it
can do this by extracting Travis-CI badges or just plain ol matching github URL
from the content.

```js
var github = extract(require('fs').readFileSync(__dirname +'/README.md'));
```

## License

MIT
