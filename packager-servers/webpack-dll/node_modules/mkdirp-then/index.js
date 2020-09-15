
var _mkdirp = require('mkdirp')
var Promise = require('any-promise')

module.exports = function (dir, mode) {
  return new Promise(function (resolve, reject) {
    _mkdirp(dir, mode, function (err) {
      if (err) reject(err)
      else resolve()
    })
  })
}
