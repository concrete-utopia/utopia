var stack = require('./stack.js')

module.exports = function (er, extra, options) {
  extra = Object.keys(options || {}).reduce(function (set, k) {
    if (!(k in set) && !/^tapChild/.test(k))
      set[k] = options[k]
    return set
  }, extra || {})

  if (!er || typeof er !== 'object') {
    extra.error = er
    return extra
  }

  var message = er.message
  var addName = true

  if (!message && er.stack) {
    message = er.stack.split('\n')[0]
    addName = false
  }

  er.message = ''
  var st = er.stack
  if (st) {
    st = st.split('\n')
    // parse out the 'at' bit from the first line.
    extra.at = stack.parseLine(st[1])
    extra.stack = stack.clean(st)
  }
  er.message = message

  if (er.name && er.name !== 'Error')
    extra.type = er.name

  Object.keys(er).forEach(function (k) {
    if (k === 'message' ||
        k === 'domainEmitter' ||
        k === 'domainThrown' ||
        k === 'domain' ||
        k === 'domainBound')
      return
    extra[k] = er[k]
  })

  return extra
}
