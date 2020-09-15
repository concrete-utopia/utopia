/* This plugin based on https://gist.github.com/Morhaus/333579c2a5b4db644bd5

 Original license:
 --------
 The MIT License (MIT)
 Copyright (c) 2015 Alexandre Kirszenberg
 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 --------

 And it's NPM-ified version: https://github.com/dcousineau/force-case-sensitivity-webpack-plugin
 Author Daniel Cousineau indicated MIT license as well but did not include it

 The originals did not properly case-sensitize the entire path, however. This plugin resolves that issue.

 This plugin license, also MIT:
 --------
 The MIT License (MIT)
 Copyright (c) 2016 Michael Pratt
 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 --------
 */

var path = require('path');

function CaseSensitivePathsPlugin(options) {
    this.options = options || {};
    this.reset();
}

CaseSensitivePathsPlugin.prototype.reset = function () {
    this.pathCache = {};
    this.fsOperations = 0;
    this.primed = false;
}

CaseSensitivePathsPlugin.prototype.getFilenamesInDir = function (dir, callback) {
    var _that = this
    var fs = this.compiler.inputFileSystem;
    this.fsOperations += 1;

    if (this.pathCache.hasOwnProperty(dir)) {
        return callback(this.pathCache[dir]);
    }
    if (this.options.debug) {
        console.log('[CaseSensitivePathsPlugin] Reading directory', dir);
    }

    fs.readdir(dir, function(err, files) {
        if (err) {
            if (_that.options.debug) {
                console.log('[CaseSensitivePathsPlugin] Failed to read directory', dir, err);
            }

            return callback([]);
        }

        callback(files.map(function(f) {
            return f.normalize ? f.normalize('NFC') : f;
        }));
    })
};

// This function based on code found at http://stackoverflow.com/questions/27367261/check-if-file-exists-case-sensitive
// By Patrick McElhaney (No license indicated - Stack Overflow Answer)
// This version will return with the real name of any incorrectly-cased portion of the path, null otherwise.
CaseSensitivePathsPlugin.prototype.fileExistsWithCase = function (filepath, callback) {
    // Split filepath into current filename (or directory name) and parent directory tree.
    var _this = this;
    var dir = path.dirname(filepath);
    var filename = path.basename(filepath);
    var parsedPath = path.parse(dir);

    // If we are at the root, or have found a path we already know is good, return.
    if (parsedPath.dir === parsedPath.root || dir === '.' || _this.pathCache.hasOwnProperty(filepath)) {
        return callback();
    }

    // Check all filenames in the current dir against current filename to ensure one of them matches.
    // Read from the cache if available, from FS if not.
    _this.getFilenamesInDir(dir, function (filenames) {
          // If the exact match does not exist, attempt to find the correct filename.
        if (filenames.indexOf(filename) === - 1) {
            // Fallback value, just in case.
            var correctFilename = '- File does not exist.';

            for (var i = 0; i < filenames.length; i++) {
                if (filenames[i].toLowerCase() === filename.toLowerCase()) {
                    correctFilename = '`' + filenames[i] + '`.';
                    break;
                }
            }
            return callback(correctFilename);
        }

        // If exact match exists, recurse through directory tree until root.
        _this.fileExistsWithCase(dir, function (recurse) {
          // If found an error elsewhere, return that correct filename
          // Don't bother caching - we're about to error out anyway.
          if (!recurse) {
            _this.pathCache[dir] = filenames;
          }

          callback(recurse)
        });
    });
}

CaseSensitivePathsPlugin.prototype.primeCache = function (callback) {
    if (this.primed) return callback();

    var _this = this;
    // Prime the cache with the current directory. We have to assume the current casing is correct,
    // as in certain circumstances people can switch into an incorrectly-cased directory.
    var currentPath = path.resolve();
    _this.getFilenamesInDir(currentPath, function (files) {
        _this.pathCache[currentPath] = files;
        _this.primed = true;
        callback()
    });
}

CaseSensitivePathsPlugin.prototype.apply = function(compiler) {
    var _this = this;

    this.compiler = compiler;

    compiler.plugin('done', function() {
        if (_this.options.debug) {
            console.log('[CaseSensitivePathsPlugin] Total filesystem reads:', _this.fsOperations);
        }
        _this.reset();
    });

    compiler.plugin('normal-module-factory', function(nmf) {
        nmf.plugin('after-resolve', function(data, done) {
            _this.primeCache(function () {
                // Trim ? off, since some loaders add that to the resource they're attemping to load
                var pathName = data.resource.split('?')[0];
                pathName =  pathName.normalize ? pathName.normalize('NFC') : pathName;

                _this.fileExistsWithCase(pathName, function(realName) {
                  if (realName) {
                      done(new Error('[CaseSensitivePathsPlugin] `' + pathName + '` does not match the corresponding path on disk ' + realName));
                  } else {
                      done(null, data);
                  }
                });
            });
        });
    });
};

module.exports = CaseSensitivePathsPlugin;
