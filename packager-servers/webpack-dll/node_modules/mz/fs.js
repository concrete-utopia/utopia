
var Promise = require('any-promise')
var fs
try {
  fs = require('graceful-fs')
} catch(err) {
  fs = require('fs')
}

var api = [
  'rename',
  'ftruncate',
  'chown',
  'fchown',
  'lchown',
  'chmod',
  'fchmod',
  'stat',
  'lstat',
  'fstat',
  'link',
  'symlink',
  'readlink',
  'realpath',
  'unlink',
  'rmdir',
  'mkdir',
  'readdir',
  'close',
  'open',
  'utimes',
  'futimes',
  'fsync',
  'fdatasync',
  'write',
  'read',
  'readFile',
  'writeFile',
  'appendFile',
  'truncate',
]

typeof fs.access === 'function' && api.push('access')
typeof fs.mkdtemp === 'function' && api.push('mkdtemp')

require('thenify-all').withCallback(fs, exports, api)

exports.exists = function (filename, callback) {
  // callback
  if (typeof callback === 'function') {
    return fs.stat(filename, function (err) {
      callback(null, !err);
    })
  }
  // or promise
  return new Promise(function (resolve) {
    fs.stat(filename, function (err) {
      resolve(!err)
    })
  })
}
