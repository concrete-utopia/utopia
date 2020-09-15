// We need TWO queues (work and subtest) and one jobs pool
//
// The pool stores buffered subtests being run in parallel.
//
// When new subtests are created, they get put in the work queue and also
// in the subtests queue if they are buffered and jobs>0.  When we put a
// test in the subtest queue, we also process it.
//
// Processing the subtest queue means moving tests into the jobs pool until
// the jobs pool length is at this.jobs
//
// Any output functions get put in the work queue if its length > 0 (ie,
// no cutting the line)
//
// Processing the work queue means walking until we run out of things, or
// encounter an unfinished test.  When we encounter ANY kind of test, we
// block until its output is completed, dumping it all into the parser.

var Base = require('./base.js')
var Spawn = require('./spawn.js')
var Stdin = require('./stdin.js')
var Deferred = require('trivial-deferred')
var Pool = require('yapool')
var TestPoint = require('./point.js')
var parseTestArgs = require('./parse-test-args.js')
var loop = require('function-loop')

var extraFromError = require('./extra-from-error.js')
var stack = require('./stack.js')
var assert = require('assert')
var util = require('util')
util.inherits(Test, Base)
var ownOr = require('own-or')
var ownOrEnv = require('own-or-env')
var tapAsserts = require('./asserts.js')
var Promise = require('bluebird')
var bindObj = require('bind-obj-methods')

// A sigil object for implicit end() calls that should not
// trigger an error if the user then calls t.end()
var IMPLICIT = {}

// Sigil to put in the queue to signal the end of all things
var EOF = { EOF: true }

function hasOwn (obj, key) {
  return Object.prototype.hasOwnProperty.call(obj, key)
}

module.exports = Test

function Test (options) {
  options = options || {}
  if (!(this instanceof Test))
    return new Test(options)

  Base.call(this, options)
  this.pushedEnd = false
  this.jobs = ownOr(options, 'jobs', 1)
  this.subtests = []
  this.pool = new Pool()
  this.queue = ['TAP version 13\n']
  this.noparallel = false
  this.cb = this.domain.bind(options.cb)
  this.occupied = false
  this.currentAssert = null
  this.count = 0
  this.n = 0
  this.ended = false
  this.explicitEnded = false
  this.multiEndThrew = false
  this.currentAssert = null
  this.assertAt = null
  this.assertStack = null
  this.planEnd = -1
  this.onBeforeEach = []
  this.onAfterEach = []
  this.ranAfterEach = false

  // bind all methods to this object, so we can pass t.end as a callback
  // and do `var test = require('tap').test` like people do.
  var bound = Object.create(null)
  bindObj(this, this, bound)
  bindObj(this, Object.getPrototypeOf(this), bound)
  bindObj(this, Test.prototype, bound)
}

Test.prototype.current = function () {
  throw new Error('Test.current() as been removed and is no more')
}

Test.prototype.spawn = function spawn (cmd, args, options, name) {
  if (typeof args === 'string') {
    args = [ args ]
  }

  args = args || []

  if (typeof options === 'string') {
    name = options
    options = {}
  }

  options = options || {}
  options.name = ownOr(options, 'name', name)
  options.command = cmd
  options.args = args

  return this.sub(Spawn, options, spawn)
}

Test.prototype.sub = function (Class, extra, caller) {
  if (extra && (extra.todo || extra.skip)) {
    this.pass(extra.name, extra)
    return Promise.resolve(this)
  }

  extra.indent = '    '
  if (this.jobs > 1 && process.env.TAP_BUFFER === undefined)
    extra.buffered = ownOr(extra, 'buffered', true)
  else
    extra.buffered = ownOrEnv(extra, 'buffered', 'TAP_BUFFER', true)

  extra.bail = ownOr(extra, 'bail', this.bail)
  extra.parent = this
  extra.stack = stack.captureString(80, caller)
  var t = new Class(extra)

  this.queue.push(t)
  this.subtests.push(t)

  var d = new Deferred()
  t.deferred = d
  this.process()
  return d.promise
}

Test.prototype.test = function test (name, extra, cb) {
  extra = parseTestArgs(name, extra, cb)
  return this.sub(Test, extra, test)
}

Test.prototype.stdin = function stdin (name, extra) {
  extra = parseTestArgs(name, extra, function () {}, '/dev/stdin')
  return this.sub(Stdin, extra || {}, stdin)
}

Test.prototype.bailout = function (message) {
  if (this.parent && (this.results || this.ended))
    this.parent.bailout(message)
  else {
    this.process()
    message = message ? ' ' + ('' + message).trim() : ''
    message = message.replace(/[\r\n]/g, ' ')
    this.parser.write('Bail out!' + message + '\n')
  }
  this.end(IMPLICIT)
  this.process()
}

Test.prototype.comment = function () {
  var message = util.format.apply(util, arguments)
  message = '# ' + message.split(/\r?\n/).join('\n# ') + '\n'

  if (this.results)
    this.push(message)
  else
    this.queue.push(message)
  this.process()
}

Test.prototype.timeout = function (options) {
  options = options || {}
  options.expired = options.expired || this.name
  if (this.occupied)
    this.occupied.timeout(options)
  else
    Base.prototype.timeout.call(this, options)
  this.end(IMPLICIT)
}

Test.prototype.main = function (cb) {
  this.setTimeout(this.options.timeout)
  this.debug('MAIN pre', this)

  var self = this
  try {
    var ret = this.cb(this)
  } catch (er) {
    this.threw(er)
  }

  if (ret && ret.then) {
    this.promise = ret
    ret.tapAbortPromise = done
    ret.then(end, done)
  } else
    done()

  function end () {
    self.debug(' > implicit end for promise')
    self.end(IMPLICIT)
    done()
  }

  function done (er) {
    if (er)
      self.threw(er)

    if (self.results || self.bailedOut)
      cb()
    else
      self.ondone = cb
  }

  this.debug('MAIN post', this)
}

Test.prototype.process = function () {
  if (this.processing)
    return this.debug(' < already processing')

  this.debug('\nPROCESSING(%s)', this.name, this.queue.length)
  this.processing = true

  var p

  while (!this.occupied && (p = this.queue.shift())) {
    this.debug('PROCESS(%s)', this.name, p)
    if (p instanceof Base) {
      this.processSubtest(p)
    } else if (p === EOF) {
      this.debug(' > EOF', this.name)
      // I AM BECOME EOF, DESTROYER OF STREAMS
      this.parser.end()
    } else if (p instanceof TestPoint) {
      this.debug(' > TESTPOINT')
      this.parser.write(p.ok + (++this.n) + p.message)
    } else if (typeof p === 'string') {
      this.debug(' > STRING')
      this.parser.write(p)
    } else if (Array.isArray(p)) {
      this.debug(' > METHOD')
      var m = p.shift()
      this[m].apply(this, p)
    } else {
      throw new Error('weird thing got in the queue')
    }
  }

  while (!this.noparallel &&
         this.pool.length < this.jobs &&
         (p = this.subtests.shift())) {
    if (!p.buffered) {
      this.noparallel = true
      break
    }
    this.debug('start subtest', p)
    this.pool.add(p)
    if (this.bailedOut)
      this.onbufferedend(p)
    else
      this.runBeforeEach(p,
        p.main.bind(p,
          this.onbufferedend.bind(this, p)))
  }

  this.debug('done processing', this.queue, this.occupied)
  this.processing = false

  // just in case any tests ended, and we have sync stuff still
  // waiting around in the queue to be processed
  if (!this.occupied && this.queue.length)
    this.process()

  this.maybeAutoend()
}

Test.prototype.processSubtest = function (p) {
  this.debug(' > subtest')
  this.occupied = p
  if (!p.buffered) {
    if (this.bailedOut)
      return this.onindentedend(p)
    this.debug(' > subtest indented')
    p.pipe(this.parser, { end: false })
    this.runBeforeEach(p,
      this.writeSubComment.bind(this, p,
        p.main.bind(p,
          this.onindentedend.bind(this, p))))
  } else if (p.readyToProcess) {
    this.debug(' > subtest buffered, finished')
    // finished!  do the thing!
    this.occupied = null
    if (!p.passing() || !p.silent) {
      this.queue.unshift(['emitSubTeardown', p])
      this.printResult(p.passing(), p.name, p.options, true)
    }
  } else {
    this.occupied = p
    this.debug(' > subtest buffered, unfinished', p)
    // unfinished buffered test.
    // nothing to do yet, just leave it there.
    this.queue.unshift(p)
  }
}

Test.prototype.emitSubTeardown = function (p) {
  try {
    p.emit('teardown')
  } catch (er) {
    delete p.options.time
    p.threw(er)
  }
}

Test.prototype.writeSubComment = function (p, cb) {
  var comment = '# Subtest'
  if (p.name)
    comment += ': ' + p.name
  comment += '\n'
  this.parser.write(comment)
  cb()
}

Test.prototype.onbufferedend = function (p, er) {
  delete p.ondone
  p.results = p.results || {}
  p.readyToProcess = true
  var to = p.options.timeout
  if (to && p.passing())
    var dur = Date.now() - p.start
  if (dur && dur > to)
    p.timeout()
  else
    p.setTimeout(false)
  this.debug('%s.onbufferedend', this.name, p.name, p.results.bailout)
  this.pool.remove(p)
  p.options.tapChildBuffer = p.output || ''
  p.options.stack = ''
  if (p.time)
    p.options.time = p.time
  if (this.occupied === p)
    this.occupied = null
  if (er)
    this.threw(er)
  p.deferred.resolve(this)
  this.process()
}

Test.prototype.onindentedend = function (p, er) {
  delete p.ondone
  this.debug('onindentedend', p)
  this.noparallel = false
  var sti = this.subtests.indexOf(p)
  if (sti !== -1)
    this.subtests.splice(sti, 1)
  p.readyToProcess = true
  p.results = p.results || {}
  if (p.time)
    p.options.time = p.time
  var to = p.options.timeout
  if (to && p.passing())
    var dur = Date.now() - p.start
  if (dur && dur > to)
    p.timeout()
  else
    p.setTimeout(false)
  this.debug('onindentedend %s(%s)', this.name, p.name, er || 'ok')
  assert(this.occupied === p)
  this.occupied = null
  this.debug('OIE(%s) b>shift into queue', this.name, this.queue)
  p.options.stack = ''

  this.queue.unshift(['emitSubTeardown', p])
  this.printResult(p.passing(), p.name, p.options, true)

  this.debug('OIE(%s) shifted into queue', this.name, this.queue)
  if (er)
    this.threw(er)
  p.deferred.resolve(this)
  this.process()
}

Test.prototype.addAssert = function (name, length, fn) {
  if (!name)
    throw new TypeError('name is required for addAssert')

  if (!(typeof length === 'number' && length >= 0))
    throw new TypeError('number of args required')

  if (typeof fn !== 'function')
    throw new TypeError('function required for addAssert')

  if (Test.prototype[name] || this[name])
    throw new TypeError('attempt to re-define `' + name + '` assert')

  this[name] = function ASSERT () {
    if (!this.currentAssert) {
      this.currentAssert = ASSERT
    }
    var args = new Array(length + 2)
    for (var i = 0; i < length; i++) {
      args[i] = arguments[i]
    }
    if (typeof arguments[length] === 'object') {
      args[length] = ''
      args[length + 1] = arguments[length]
    } else {
      args[length] = arguments[length] || ''
      args[length + 1] = arguments[length + 1] || {}
    }

    return fn.apply(this, args)
  }
}

Test.prototype.fail = function fail (message, extra) {
  if (!this.currentAssert) {
    this.currentAssert = fail
  }

  if (message && typeof message === 'object') {
    extra = message
    message = ''
  } else {
    if (!message) {
      message = ''
    }
    if (!extra) {
      extra = {}
    }
  }

  this.printResult(false, message, extra)

  var ret = true
  if (!extra.todo && !extra.skip)
    ret = false

  return ret
}

Test.prototype.pass = function pass (message, extra) {
  if (!this.currentAssert) {
    this.currentAssert = pass
  }
  this.printResult(true, message || '(unnamed test)', extra)
  return true
}

Test.prototype.printResult = function pR (ok, message, extra, front) {
  var n = this.count + 1
  if (this.planEnd !== -1 && n > this.planEnd) {
    if (!this.passing())
      return

    var failMessage = this.explicitEnded
        ? 'test after end() was called'
        : 'test count exceeds plan'

    var er = new Error(failMessage)
    Error.captureStackTrace(er, this.currentAssert || pR)
    er.test = this.name
    er.plan = this.planEnd
    this.threw(er)
    return
  }

  extra = extra || {}

  if (this.assertAt) {
    extra.at = this.assertAt
    this.assertAt = null
  }

  if (this.assertStack) {
    extra.stack = this.assertStack
    this.assertStack = null
  }

  if (hasOwn(extra, 'stack') && !hasOwn(extra, 'at'))
    extra.at = stack.parseLine(extra.stack.split('\n')[0])

  var fn = this.currentAssert || pR
  this.currentAssert = null
  if (!ok && !extra.skip && !hasOwn(extra, 'at')) {
    assert.equal(typeof fn, 'function')
    extra.at = stack.at(fn)
    if (!extra.todo)
      extra.stack = stack.captureString(80, fn)
  }

  var diagnostic
  if (!ok)
    diagnostic = true

  if (extra.skip)
    diagnostic = false

  if (process.env.TAP_DIAG === '0')
    diagnostic = false

  if (typeof extra.diagnostic === 'boolean')
    diagnostic = extra.diagnostic

  if (diagnostic)
    extra.diagnostic = true

  this.count = n
  var res = { ok: ok, message: message, extra: extra }
  var output = new TestPoint(ok, message, extra)
  // when we jump the queue, skip an extra line
  if (front)
    output.message = output.message.trimRight() + '\n\n'

  if (front) {
    this.emit('result', res)
    this.parser.write(output.ok + (++this.n) + output.message)
  } else
    this.queue.push(['emit', 'result', res], output)

  if (this.planEnd === this.count)
    this.end(IMPLICIT)

  this.process()
}

Test.prototype.pragma = function (set) {
  var p = ''
  Object.keys(set).forEach(function (i) {
    p += 'pragma ' + (set[i] ? '+' : '-') + i + '\n'
  })
  this.queue.push(p)
  this.process()
}

Test.prototype.plan = function (n, comment) {
  if (this.bailedOut)
    return

  if (this.planEnd !== -1) {
    throw new Error('Cannot set plan more than once')
  }

  if (typeof n !== 'number' || n < 0) {
    throw new TypeError('plan must be a number')
  }

  // Cannot get any tests after a trailing plan, or a plan of 0
  var ending = false
  if (this.count !== 0 || n === 0) {
    ending = true
  }

  if (n === 0)
    this.skip = comment || true

  this.planEnd = n
  comment = comment ? ' # ' + comment.trim() : ''
  this.queue.push('1..' + n + comment + '\n')

  if (ending)
    this.end(IMPLICIT)
  else
    this.process()
}

Test.prototype.done = Test.prototype.end = function (implicit) {
  this.debug('END implicit=%j', implicit === IMPLICIT)
  if (this.ended && implicit === IMPLICIT)
    return

  // beyond here we have to be actually done with things, or else
  // the semantic checks on counts and such will be off.
  if (!queueEmpty(this) || this.occupied) {
    if (!this.pushedEnd)
      this.queue.push(['end', implicit])
    this.pushedEnd = true
    return this.process()
  }

  if (!this.ranAfterEach && this.parent) {
    this.ranAfterEach = true
    this.parent.runAfterEach(this, end.bind(this, implicit))
  } else
    end.call(this, implicit)
}

function end (implicit) {
  this.ended = true

  if (implicit !== IMPLICIT && !this.multiEndThrew) {
    if (this.explicitEnded) {
      this.multiEndThrew = true
      var er = new Error('test end() method called more than once')
      Error.captureStackTrace(er, this.currentAssert || end)
      er.test = this.name
      this.threw(er)
      return
    }
    this.explicitEnded = true
  }

  if (this.planEnd === -1) {
    this.debug('END(%s) implicit plan', this.name, this.count)
    this.plan(this.count)
  }

  this.queue.push(EOF)
  this.process()
}

Test.prototype.threw = function (er, extra, proxy) {
  this.debug('THREW', er.message, extra, proxy)

  // event emitters 'error' events need to re-throw so that they
  // can jump out of the flow like a normal throw.  They'll just
  // end up back here once that happens, though, unless there's a
  // try/catch somewhere in the call stack.
  if (er.domainEmitter) {
    delete er.domainEmitter
    throw er
  }

  if (this.name && !proxy)
    er.test = this.name
  if (!proxy)
    extra = extraFromError(er, extra, this.options)
  Base.prototype.threw.call(this, er, extra, proxy)

  if (!this.results) {
    this.fail(extra.message || er.message, extra)
    if (!proxy)
      this.end(IMPLICIT)
  }
  this.process()
}

Test.prototype.runBeforeEach = function (who, cb) {
  var self = this
  if (this.parent)
    this.parent.runBeforeEach(who, function () {
      loop(who, self.onBeforeEach, cb, who.threw)
    })
  else
    loop(who, self.onBeforeEach, cb, who.threw)
}

Test.prototype.runAfterEach = function (who, cb) {
  var self = this
  loop(who, self.onAfterEach, function () {
    if (self.parent)
      self.parent.runAfterEach(who, cb)
    else
      cb()
  }, who.threw)
}

Test.prototype.beforeEach = function (fn) {
  this.onBeforeEach.push(fn)
}

Test.prototype.afterEach = function (fn) {
  this.onAfterEach.push(fn)
}

Test.prototype.teardown = Test.prototype.tearDown = function (fn) {
  this.on('teardown', fn)
}

Test.prototype.shouldAutoend = function () {
  var should = (
    this.options.autoend &&
    !this.ended &&
    !this.occupied &&
    queueEmpty(this) &&
    !this.pool.length &&
    !this.subtests.length &&
    this.planEnd === -1
  )
  return should
}

Test.prototype.autoend = function () {
  this.options.autoend = true
  this.maybeAutoend()
}

Test.prototype.maybeAutoend = function () {
  if (this.autoendTimer)
    clearTimeout(this.autoendTimer)

  if (this.shouldAutoend()) {
    var self = this
    self.autoendTimer = setTimeout(function () {
      if (self.shouldAutoend()) {
        self.autoendTimer = setTimeout(function () {
          if (self.shouldAutoend()) {
            self.end(IMPLICIT)
          }
        })
      }
    })
  }
}

function endAllQueue (queue) {
  queue.forEach(function (p, i) {
    if ((p instanceof Base) && !p.readyToProcess)
      queue[i] = new TestPoint(false,
        'child test left in queue ' + p.constructor.name + ': ' +
        p.name, p.options)
  })
  queue.push(['end', IMPLICIT])
}

function queueEmpty (t) {
  return t.queue.length === 0 ||
    t.queue.length === 1 && t.queue[0] === 'TAP version 13\n'
}

Test.prototype.endAll = function (sub) {
  this.processing = true
  if (this.occupied) {
    var p = this.occupied
    if (p.endAll)
      p.endAll(true)
    else {
      p.parser.abort('test unfinished')
    }
  } else if (sub) {
    this.process()
    if (queueEmpty(this)) {
      var options = Object.keys(this.options).reduce(function (o, k) {
        o[k] = this.options[k]
        return o
      }.bind(this), {})
      this.options.at = null
      this.options.stack = ''
      options.test = this.name
      this.fail('test unfinished', options)
    }
  }
  if (this.promise && this.promise.tapAbortPromise)
    this.promise.tapAbortPromise()
  if (this.occupied) {
    this.queue.unshift(this.occupied)
    this.occupied = null
  }
  endAllQueue(this.queue)
  this.processing = false
  this.process()
  this.parser.end()
}

// Add all the asserts
tapAsserts.decorate(Test.prototype)
