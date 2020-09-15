# rmdir

Remove all files in the given path recursively.

## Description

Or just use `require( 'child_process' ).exec` and call `rm -r`

    var exec = require('child_process').exec;
    var path = '/path/to/the/dir';

    exec('rm -r ' + path, function (err, stdout, stderr) {
      // your callback goes here
    });

## Requires

    node >= 0.4.x

## Installation

    npm install rmdir

## Usage

> Require the module before using

    var rmdir = require('rmdir');

### rmdir(path, [options], [callback]);

You can optionally pass in an alternate fs implementation by passing in `options.fs`.Your implementation should have `fs.lstat(path, callback)`, `fs.unlink(path, callback)`, `fs.readdir(path, callback)`, `fs.rmdir(path, callback)`, and `fs.exists(path, callback)` implemented.

#### Arguments

> path

    type: String
    desc: The path to be clear.

> options

    type: Object
    desc: Options to be used when removing all files.

> callback

    type: Function
    desc: The callback to be called after all files are removed.
    arguments:
      err:
      type: Error
    dirs:
      type: Array
    desc: The removed dirs.
    files:
      type: Array
    desc: The removed files.

#### Example

    var rmdir = require('rmdir');
    var path = '/path/to/the/dir';

    rmdir(path + '/assets', function (err, dirs, files) {
      console.log(dirs);
      console.log(files);
      console.log('all files are removed');
    });

## Creadit

[Aaron Larner](https: //github.com/alarner)
[Glen R.Goodwin](https: //github.com/arei)
[David Pate](https: //github.com/DavidTPate)
[Radare](https://github.com/radare)

## License

(The MIT License)

Copyright(c) 2011 dreamerslab & lt; ben@ dreamerslab.com & gt;

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files(the 'Software'), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and / or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
