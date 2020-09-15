var assert = require('assert')
var bindObj = require('./bind-obj-methods.js')
var obj, m

function makeObj () {
  var obj = {
    method: function () { return this.foo },
    foo: 'bar'
  }

  Object.defineProperty(obj, 'secretMethod', {
    value: function () {
      return 'secret' + this.method()
    },
    enumerable: false,
    configurable: true,
    writable: true
  })

  return obj
}

// pretend we already bound secretMethod
obj = makeObj()
bindObj(obj, obj, [ 'secretMethod', 'method' ])
m = obj.method
assert.equal(m(), undefined)
m = obj.secretMethod
assert.throws(m)

obj = makeObj()
bindObj(obj, obj, { secretMethod: true })
m = obj.method
assert.equal(m(), 'bar')
m = obj.secretMethod
assert.throws(m)

obj = makeObj()
bindObj(obj, obj)
m = obj.method
assert.equal(m(), 'bar')
m = obj.secretMethod
assert.equal(m(), 'secretbar')

obj = makeObj()
bindObj(obj, Object.prototype)
m = obj.hasOwnProperty
assert.equal(m('hasOwnProperty'), true)

console.log('TAP version 13\nok\n1..1')
