function typeOf (arg) {
  var t = typeof arg
  switch (t) {
    case 'object':
      return arg ? 'object' : 'null'
    default:
      return t
  }
}

module.exports = function (name_, extra_, cb_, defaultName) {
  var name
  var extra
  var cb

  // this only works if it's literally the 4th argument.  it's mostly
  // used internally.
  defaultName = defaultName || '(unnamed test)'

  for (var i = 0; i < 3 && i < arguments.length; i++) {
    var arg = arguments[i]
    var type = typeOf(arg)
    if (name === undefined && (type === 'string' || type === 'number'))
      name = '' + arg
    else if (type === 'object') {
      extra = arg
      if (name === undefined)
        name = null
    } else if (type === 'function') {
      if (extra === undefined)
        extra = {}
      if (name === undefined)
        name = null
      cb = arg
    } else if (arg === false) {
      // it's handy while developing to put a ! in front of a
      // function to temporarily make a test TODO
      continue
    } else if (type !== 'undefined')
      throw new TypeError('unknown argument passed to parseTestArgs: ' + type)
  }

  if (!extra)
    extra = {}

  if (!cb)
    extra.todo = true

  if (!name && extra.name)
    name = extra.name

  if (!name && cb && cb.name)
    name = cb.name

  name = name || defaultName
  extra.name = name
  extra.cb = cb || todoCb
  return extra
}

/* istanbul ignore next */
function todoCb () {
  throw new Error('callback called for TODO test')
}
