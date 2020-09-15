var Base = require('./base.js')
var util = require('util')
var ownOr = require('own-or')
var domain = require('domain')

util.inherits(Stdin, Base)

module.exports = Stdin

function Stdin (options) {
  options = options || {}
  if (!(this instanceof Stdin))
    return new Stdin(options)

  options.name = ownOr(options, 'name', '/dev/stdin')
  Base.call(this, options)

  // This has to be here for node 0.10's wonky streams
  this.stream = ownOr(options, 'tapStream', process.stdin)
  this.stream.pause()
}

Stdin.prototype.main = function (cb) {
  this.domain.add(this.stream)
  this.setTimeout(this.options.timeout)
  this.stream.pipe(this.parser)
  this.stream.resume()
  this.once('end', cb)
}

Stdin.prototype.threw = function (er, extra, proxy) {
  extra = Base.prototype.threw.call(this, er, extra, proxy)
  this.options = extra
  this.parser.abort(er.message, extra)
  this.parser.end()
}
