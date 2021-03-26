const hasOwn = Object.prototype.hasOwnProperty

export function is(x: any, y: any) {
  if (x === y) {
    return x !== 0 || y !== 0 || 1 / x === 1 / y
  } else {
    return x !== x && y !== y
  }
}

export function abstractEquals(
  objA: any,
  objB: any,
  debugMode: boolean,
  nestedEqualityFn: (a: any, b: any, mode: boolean) => boolean,
): boolean {
  if (is(objA, objB)) return true
  if (typeof objA !== 'object' || objA === null || typeof objB !== 'object' || objB === null) {
    return false
  }

  // code from https://github.com/epoberezkin/fast-deep-equal/blob/a33d49ab5cc659e331ff445109f35dd323230d41/src/index.jst#L25-L39
  if (objA instanceof Map && objB instanceof Map) {
    if (objA.size !== objB.size) return false
    for (const i of objA.entries()) if (!objB.has(i[0])) return false
    for (const i of objA.entries())
      if (!nestedEqualityFn(i[1], objB.get(i[0]), debugMode)) return false
    return true
  }

  if (objA instanceof Set && objB instanceof Set) {
    if (objA.size !== objB.size) return false
    for (const i of objA.entries()) if (!objB.has(i[0])) return false
    return true
  }

  const keysA = Object.keys(objA)
  const keysB = Object.keys(objB)
  if (keysA.length !== keysB.length) return false
  for (let i = 0; i < keysA.length; i++) {
    if (
      !hasOwn.call(objB, keysA[i]) ||
      !nestedEqualityFn(objA[keysA[i]], objB[keysA[i]], debugMode)
    ) {
      if (debugMode) {
        // tslint:disable-next-line:no-console
        console.info(`equality check failed: ${keysA[i]} !== ${keysB[i]}`)
      }
      return false
    }
  }
  return true
}

export function shallowEqual(objA: any, objB: any, debugMode: boolean = false) {
  return abstractEquals(objA, objB, debugMode, is)
}

export function oneLevelNestedEquals(objA: any, objB: any, debugMode: boolean = false) {
  return abstractEquals(objA, objB, debugMode, shallowEqual)
}

export function keepReferenceIfShallowEqual<T>(original: T, maybeNew: T) {
  if (shallowEqual(original, maybeNew)) {
    return original
  } else {
    return maybeNew
  }
}
