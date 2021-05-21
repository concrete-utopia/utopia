import { DEEP_FREEZE_STATE } from '../common/env-vars'

// modifed version of https://github.com/jsdf/deep-freeze

function deepFreezeImpl(o: any) {
  // Utopia modification: an object can ask not to be deep freezed
  if (o?.skipDeepFreeze === true) {
    return o
  }

  // unmodified source code:

  Object.freeze(o)

  var oIsFunction = typeof o === 'function'
  var hasOwnProp = Object.prototype.hasOwnProperty

  Object.getOwnPropertyNames(o).forEach(function (prop) {
    if (
      hasOwnProp.call(o, prop) &&
      (oIsFunction ? prop !== 'caller' && prop !== 'callee' && prop !== 'arguments' : true) &&
      o[prop] !== null &&
      (typeof o[prop] === 'object' || typeof o[prop] === 'function') &&
      !Object.isFrozen(o[prop])
    ) {
      deepFreezeImpl(o[prop])
    }
  })

  return o
}

export function deepFreeze<T>(obj: T): Readonly<T> {
  return deepFreezeImpl(obj)
}

export function optionalDeepFreeze<T>(obj: T): Readonly<T> {
  return DEEP_FREEZE_STATE ? deepFreezeImpl(obj) : obj
}
