var Base = require('./base.js')

var assert = require('assert')
var util = require('util')
util.inherits(Spawn, Base)
var ownOr = require('own-or')
var path = require('path')
var cleanYamlObject = require('./clean-yaml-object.js')

module.exports = Spawn

var cp = require('child_process')
var spawn = cp.spawn

function Spawn (options) {
  options = options || {}
  if (!(this instanceof Spawn))
    return new Spawn(options)

  Base.call(this, options)

  this.command = options.command

  if (!this.command)
    throw new TypeError('no command provided')

  this.args = options.args
  // stdout must be a pipe
  if (options.stdio) {
    if (typeof options.stdio === 'string')
      this.stdio = [ options.stdio, 'pipe', options.stdio ]
    else
      this.stdio = options.stdio.slice(0)
  } else
    this.stdio = [ 0, 'pipe', 2 ]

  this.stdio[1] = 'pipe'
  var env = options.env || process.env
  this.env = Object.keys(env).reduce(function (e, k) {
    e[k] = env[k]
    return e
  }, {})

  this.env.TAP = '1'
  if (this.bail)
    this.env.TAP_BAIL = '1'

  this.cwd = ownOr(options, 'cwd', process.cwd())
  options.cwd = this.cwd
  if (!this.name) {
    if (this.command === process.execPath) {
      this.name = path.basename(process.execPath) + ' ' +
        this.args.map(function (a) {
          if (a.indexOf(this.cwd) === 0) {
            return './' +
              a.substr(this.cwd.length + 1).replace(/\\/g, '/')
          } else {
            return a
          }
        }, this).join(' ')
    } else {
      this.name = this.command + ' ' + this.args.join(' ')
    }
  }

  this.proc = null
}

Spawn.prototype.endAll = function () {
  if (this.proc)
    this.proc.kill('SIGKILL')
  this.parser.abort('test unfinished')
  this.cb()
}

Spawn.prototype.main = function (cb) {
  this.cb = cb
  this.setTimeout(this.options.timeout)
  var options = Object.keys(this.options).reduce(function (o, k) {
    o[k] = this.options[k]
    return o
  }.bind(this), {
    cwd: this.cwd,
    env: this.env,
    stdio: this.stdio
  })
  try {
    var proc = this.proc = spawn(this.command, this.args, options)
    proc.stdout.pipe(this.parser)
    proc.on('close', this.onprocclose.bind(this))
    proc.on('error', this.threw.bind(this))
  } catch (er) {
    this.threw(er)
  }
}

Spawn.prototype.threw = function (er, extra, proxy) {
  extra = Base.prototype.threw.call(this, er, extra, proxy)
  extra = cleanYamlObject(extra)
  // unhook entirely
  this.parser.abort(er.message, extra)
  if (this.proc) {
    this.proc.stdout.removeAllListeners('data')
    this.proc.stdout.removeAllListeners('end')
    this.proc.removeAllListeners('close')
    this.proc.kill('SIGKILL')
  }
  this.cb()
}

Spawn.prototype.onprocclose = function (code, signal) {
  this.debug('SPAWN close %j %s', code, signal)
  this.options.exitCode = code
  if (signal)
    this.options.signal = signal
  this.results = this.results || {}

  // spawn closing with no tests is treated as a skip.
  if (this.results.plan && this.results.plan.skipAll && !code && !signal)
    this.options.skip = this.results.plan.skipReason || true

  if (code || signal) {
    this.results.ok = false
    this.parser.ok = false
  }
  return this.cb()
}

Spawn.prototype.timeout = function (extra) {
  if (this.proc)
    this.proc.kill('SIGTERM')
  var t = setTimeout(function () {
    if (!this.options.signal && this.options.exitCode === undefined) {
      Base.prototype.timeout.call(this, extra)
      this.proc.kill('SIGKILL')
    }
  }.bind(this), 1000)
  if (t.unref)
    t.unref()
}
