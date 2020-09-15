module.exports = Base

var Readable = require('stream').Readable
/* istanbul ignore if */
if (!Readable || process.version.match(/^v0\.10/)) {
  Readable = require('readable-stream').Readable
}

var extraFromError = require('./extra-from-error.js')
var assert = require('assert')
var cleanYamlObject = require('./clean-yaml-object.js')

var domain = require('domain')
var util = require('util')
util.inherits(Base, Readable)

var Parser = require('tap-parser')

var ownOr = require('own-or')
var ownOrEnv = require('own-or-env')

function Base (options) {
  this.start = 0
  this.hrtime = null
  this.time = null
  this.readyToProcess = false
  this.options = options
  this.parent = ownOr(options, 'parent', null)
  this.bail = ownOrEnv(options, 'bail', 'TAP_BAIL', true)
  this.name = ownOr(options, 'name', '')
  if (!this.name)
    this.name = ''
  else
    this.name = this.name.replace(/[\n\r\s\t]/g, ' ')
  this.indent = ownOr(options, 'indent', '')
  this.silent = !!options.silent
  this.buffered = !!options.buffered || !!options.silent
  this.finished = false
  this.strict = ownOrEnv(options, 'strict', 'TAP_STRICT', true)
  this.omitVersion = !!options.omitVersion
  this.preserveWhitespace = ownOr(options, 'preserveWhitespace', true)
  this.jobs = +ownOrEnv(options, 'jobs', 'TAP_JOBS') || 0
  this.skip = ownOr(options, 'skip', false)
  this.todo = ownOr(options, 'todo', false)
  this.setupParser(options)
  this.finished = false
  this.output = ''
  this.results = null
  this.bailedOut = false
  if (this.skip || this.todo)
    this.main = Base.prototype.main

  Readable.apply(this, options)

  domain.create().add(this)
  this.domain.on('error', this.threw.bind(this))

  if (typeof options.debug === 'boolean')
    this.debug = options.debug ? debug : nodebug
}

Base.prototype.passing = function () {
  return this.parser.ok
}

Base.prototype.setTimeout = function (n) {
  if (!this.hrtime)
    this.hrtime = process.hrtime()

  if (!n) {
    clearTimeout(this.timer)
    this.timer = null
  } else {
    this.start = Date.now()
    this.timer = setTimeout(this.timeout.bind(this), n)
    if (this.timer.unref)
      this.timer.unref()
  }
}

Base.prototype.threw = function (er, extra, proxy) {
  if (this.name && !proxy)
    er.test = this.name

  var message = er.message

  if (!extra)
    extra = extraFromError(er, extra, this.options)

  if (this.results || this.ended) {
    this.results.ok = false
    if (this.parent)
      this.parent.threw(er, extra, true)
    else if (!er.stack)
      console.error(er)
    else {
      er.message = message
      delete extra.stack
      delete extra.at
      console.error('%s: %s', er.name || 'Error', message)
      console.error(er.stack.split(/\n/).slice(1).join('\n'))
      console.error(extra)
    }
  } else
    this.parser.ok = false

  return extra
}

Base.prototype.timeout = function (options) {
  this.setTimeout(false)
  var er = new Error('timeout!')
  options = options || {}
  options.expired = options.expired || this.name
  this.threw(new Error('timeout!'), options)
}

Base.prototype.main = function (cb) {
  cb()
}

Base.prototype.online = function (line) {
  this.debug('LINE %j', line)
  return this.push(this.indent + line)
}

Base.prototype.push = function (c, e) {
  assert.equal(typeof c, 'string')
  assert.equal(c.substr(-1), '\n')

  if (this.buffered) {
    this.output += c
    return true
  }

  // We *always* want data coming out immediately.  Test runners have a
  // very special relationship with zalgo. It's more important to ensure
  // that any console.log() lines that appear in the midst of tests are
  // not taken out of context
  if (this._readableState) {
    this._readableState.sync = false
  }

  // this.debug(this._readableState)
  return Readable.prototype.push.call(this, c, e)
}

Base.prototype.onbail = function (reason) {
  this.bailedOut = reason || true
  this.emit('bailout', reason)
}

Base.prototype.oncomplete = function (results) {
  if (this.hrtime) {
    this.hrtime = process.hrtime(this.hrtime)
    this.time = Math.round(this.hrtime[0] * 1e6 + this.hrtime[1] / 1e3) / 1e3
  }

  this.debug('ONCOMPLETE %j %j', this.name, results)

  if (this.results)
    Object.keys(this.results).forEach(function (k) {
      results[k] = this.results[k]
    }, this)

  this.results = results
  this.emit('complete', results)
  var failures = results.failures.filter(function (f) {
    delete f.diag
    delete f.ok
    return f.tapError
  })

  if (failures.length)
    this.options.failures = failures

  this.onbeforeend()
  this.emit('end')
  this.ondone()
}

Base.prototype.onbeforeend = function () {}
Base.prototype.ondone = function () {}

Base.prototype.setupParser = function (options) {
  this.parser = new Parser({
    bail: this.bail,
    strict: this.strict,
    omitVersion: this.omitVersion,
    preserveWhitespace: this.preserveWhitespace
  })
  assert(this.parser.preserveWhitespace)
  this.parser.on('line', this.online.bind(this))
  this.parser.once('bailout', this.onbail.bind(this))
  this.parser.on('complete', this.oncomplete.bind(this))
}

Base.prototype._read = function (n) {
  // this.emit('readable')
  // this.debug('_read %j', this.name, arguments)
}

Base.prototype.inspect = function () {
  return this.constructor.name + ' ' + util.inspect({
    name: this.name,
    jobs: this.jobs,
    buffered: this.buffered,
    occupied: this.occupied,
    pool: this.pool,
    queue: this.queue,
    subtests: this.subtests,
    output: this.output,
    skip: this.skip,
    todo: this.todo,
    results: this.results,
    options: [
      'autoend',
      'command',
      'args',
      'stdio',
      'env',
      'cwd',
      'exitCode',
      'signal',
      'expired',
      'timeout',
      'at',
      'skip',
      'todo'
    ].reduce(function (set, k) {
      if (this.options[k] !== undefined)
        set[k] = this.options[k]
      return set
    }.bind(this), {})
  })
}

Base.prototype.debug = (/\btap\b/i.test(process.env.NODE_DEBUG || ''))
  ? debug : nodebug

function nodebug () {}

/* istanbul ignore next */
function debug () {
  var prefix = 'TAP ' + process.pid + ' ' + this.name + ': '
  var msg = util.format.apply(util, arguments).trim()
  msg = prefix + msg.split('\n').join('\n' + prefix)
  console.error(msg)
}
