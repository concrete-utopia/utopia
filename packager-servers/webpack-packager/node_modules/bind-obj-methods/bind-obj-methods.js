module.exports = bindObj

function bindObj (obj, proto, bound) {
  bound = bound || Object.create(null)
  if (Array.isArray(bound))
    bound = bound.reduce(function (set, k) {
      set[k] = true
      return set
    }, Object.create(null))

  // don't try to bind constructors, it's weird
  bound.constructor = true
  proto = proto || obj

  Object.keys(proto).forEach(function (k) {
    if (typeof obj[k] === 'function' && !bound[k]) {
      bound[k] = true
      obj[k] = proto[k].bind(obj)
    }
  })

  Object.getOwnPropertyNames(proto).forEach(function (k) {
    if (typeof obj[k] === 'function' && !bound[k]) {
      bound[k] = true
      Object.defineProperty(obj, k, {
        value: obj[k].bind(obj),
        enumerable: false,
        configurable: true,
        writable: true
      })
    }
  })
}
