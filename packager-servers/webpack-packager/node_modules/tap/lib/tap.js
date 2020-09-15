var Test = require('./test.js')
var Stdin = require('./stdin.js')
var Spawn = require('./spawn.js')
var util = require('util')
var objToYaml = require('./obj-to-yaml.js')
var yaml = require('js-yaml')

util.inherits(TAP, Test)
function TAP (options) {
  Test.call(this, options)
  this.start = Date.now()
}

var didPipe = false
TAP.prototype.pipe = function () {
  didPipe = true
  this.setTimeout(this.options.timeout)
  this.pipe = Test.prototype.pipe
  this.push = Test.prototype.push
  var ret = this.pipe.apply(this, arguments)
  this.process()
  return ret
}

function monkeypatchEpipe () {
  process.stdout.emit = function (emit) {
    return function (ev, er) {
      if (ev === 'error' && er.code === 'EPIPE')
        return this.emit = emit
      return emit.apply(this, arguments)
    }
  }(process.stdout.emit)
}

function monkeypatchExit () {
  // ensure that we always get run, even if a user does
  // process.on('exit', process.exit)
  process.reallyExit = function (original) {
    return function reallyExit (code) {
      code = onExitEvent(code)
      return original.call(this, code)
    }
  }(process.reallyExit)

  process.exit = function (original) {
    return function exit (code) {
      code = onExitEvent(code)
      return original.call(this, code)
    }
  }(process.exit)

  process.on('exit', onExitEvent)
}

var didOnExitEvent = false
function onExitEvent (code) {
  if (didOnExitEvent)
    return process.exitCode || code

  didOnExitEvent = true

  if (!tap.results)
    tap.endAll()

  if (tap.results && !tap.results.ok && code === 0) {
    process.exitCode = 1
    if (process.version.match(/^v0\.(10|[0-9])\./))
      process.exit(code)
  }

  return process.exitCode || code || 0
}

TAP.prototype.push = function push () {
  // this resets push and pipe to standard values
  this.pipe(process.stdout)
  this.patchProcess()
  return this.push.apply(this, arguments)
}

TAP.prototype.patchProcess = function () {
  monkeypatchEpipe()
  monkeypatchExit()
  process.on('uncaughtException', this.threw)
  process.on('unhandledRejection', function (er) {
    this.threw(er)
  }.bind(this))
}

TAP.prototype.onbail = function () {
  Test.prototype.onbail.apply(this, arguments)
  this.endAll()
  process.exit(1)
}

TAP.prototype.onbeforeend = function () {
  if (didPipe && this.time && !this.bailedOut)
    this.emit('data', '# time=' + this.time + 'ms\n')
}

TAP.prototype.ondone = function () {
  try {
    this.emit('teardown')
  } catch (er) {
    this.threw(er)
  }
}

// Root test runner doesn't have the 'teardown' event, because it
// isn't hooked into any parent Test as a harness.
TAP.prototype.teardown = TAP.prototype.tearDown = function (fn) {
  this.autoend()
  return Test.prototype.teardown.apply(this, arguments)
}

var opt = { name: 'TAP' }
if (process.env.TAP_DEBUG === '1' ||
    /\btap\b/.test(process.env.NODE_DEBUG || ''))
  opt.debug = true

var tap = new TAP(opt)
module.exports = tap
tap.mocha = require('./mocha.js')
tap.mochaGlobals = tap.mocha.global

tap.Test = Test
tap.Spawn = Spawn
tap.Stdin = Stdin
tap.synonyms = require('./synonyms.js')

// SIGTERM means being forcibly killed, almost always by timeout
var onExit = require('signal-exit')
var didTimeoutKill = false
onExit(function (code, signal) {
  if (signal !== 'SIGTERM' || !didPipe || didTimeoutKill)
    return

  var handles = process._getActiveHandles().filter(function (h) {
    return h !== process.stdout &&
    h !== process.stdin &&
    h !== process.stderr
  })
  var requests = process._getActiveRequests()

  // Ignore this because it's really hard to test cover in a way
  // that isn't inconsistent and unpredictable.
  /* istanbul ignore next */
  var extra = {
    at: null,
    signal: signal
  }
  if (requests.length) {
    extra.requests = requests.map(function (r) {
      var ret = { type: r.constructor.name }
      if (r.context) {
        ret.context = r.context
      }
      return ret
    })
  }
  if (handles.length) {
    extra.handles = handles.map(function (h) {
      var ret = { type: h.constructor.name }
      if (h.msecs) {
        ret.msecs = h.msecs
      }
      if (h._events) {
        ret.events = Object.keys(h._events)
      }
      if (h._sockname) {
        ret.sockname = h._sockname
      }
      if (h._connectionKey) {
        ret.connectionKey = h._connectionKey
      }
      return ret
    })
  }

  // this is impossible to cover, because it happens after nyc has
  // already done its stuff.
  /* istanbul ignore else */
  if (!tap.results && tap.timeout)
    tap.timeout(extra)
  else {
    console.error('possible timeout: SIGTERM received after tap end')
    if (extra.handles || extra.requests) {
      delete extra.signal
      if (!extra.at) {
        delete extra.at
      }
      var yaml = require('js-yaml')
      console.error(objToYaml(extra))
    }
    didTimeoutKill = true
    process.kill(process.pid, 'SIGTERM')
  }
})
