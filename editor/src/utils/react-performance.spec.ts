import { keepDeepReferenceEqualityIfPossible } from './react-performance'
import { deepFreeze } from './deep-freeze'
import fastDeepEquals from 'fast-deep-equal'

describe('keepDeepReferenceEqualityIfPossible', () => {
  it('keeps simple value equality', () => {
    const a = 'hello'
    const b = 'hello'
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(a === r).toBeTruthy()
  })

  it('keeps reference equality for arrays if they are referentially equal', () => {
    const a = deepFreeze(['test', 'array'])
    const b = a
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(a === r).toBeTruthy()
  })

  it('keeps reference equality for arrays if only their elements are referentially equal', () => {
    const a = deepFreeze(['test', 'array'])
    const b = deepFreeze(['test', 'array'])
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(a === r).toBeTruthy()
  })

  it('keeps reference equality for arrays if only their elements are value equal', () => {
    const a = deepFreeze([{ a: 5 }, { b: { c: { d: 6 } } }])
    const b = deepFreeze([{ a: 5 }, { b: { c: { d: 6 } } }])
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(a === r).toBeTruthy()
  })

  it('keeps reference equality for matching parts of arrays even if they are different in length', () => {
    const a = deepFreeze([{ a: 5 }, { b: { c: { d: 6 } } }] as any)
    const b = deepFreeze([{ a: 5 }, { b: { c: { d: 6 } } }, { c: 12 }] as any)
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(r).toEqual(b)
    expect(r.length).toBe(3)
    expect(a === r).toBeFalsy()
    expect(a[0] === r[0]).toBeTruthy()
    expect(a[1] === r[1]).toBeTruthy()
  })

  it('keeps reference equality for matching parts of arrays even if they are different in length 2', () => {
    const a = deepFreeze([{ a: 5 }, { b: { c: { d: 6 } } }]) as any[]
    const b = deepFreeze([{ a: 5 }]) as any[]
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(r.length === 1).toBeTruthy()
    expect(a === r).toBeFalsy()
    expect(a[0] === r[0]).toBeTruthy()
  })

  it('if a value is inserted in the middle, we keep reference equality only from one end', () => {
    const a = deepFreeze([{ a: 5 }, { b: 6 }] as any)
    const b = deepFreeze([{ a: 5 }, { c: 12 }, { b: 6 }] as any)
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(r.length === 3).toBeTruthy()
    expect(a === r).toBeFalsy()
    expect(a[0] === r[0]).toBeTruthy()
    expect(r[0].a === 5).toBeTruthy()
  })

  it('keeps reference equality for individual array elements even if some of them are not the same', () => {
    const a = deepFreeze([{ a: 5 }, { b: 6 }])
    const b = deepFreeze([{ a: 5 }, { b: 15 }])
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(a === r).toBeFalsy()
    expect(a[0] === r[0]).toBeTruthy()
    expect(a[1] === r[1]).toBeFalsy()
  })

  it('keeps reference equality for objects that are referentially equal', () => {
    const a = deepFreeze({ a: 5, b: { c: 6 } })
    const b = a
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(a === r).toBeTruthy()
  })

  it('keeps reference equality for objects that are value equal', () => {
    const a = deepFreeze({ a: 5, b: { c: { d: 6 } } })
    const b = deepFreeze({ a: 5, b: { c: { d: 6 } } })
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(a === r).toBeTruthy()
    expect(a.b.c === r.b.c).toBeTruthy()
  })

  it('keeps reference equality for parts of objects that are referentially equal', () => {
    const a = deepFreeze({ a: { some: 5 }, b: { c: { d: 6 } } }) as any
    const b = deepFreeze({ a: { some: 'other value' }, b: { c: a.b.c } })
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(a === r).toBeFalsy()
    expect(a.b.c === r.b.c).toBeTruthy()
  })

  it('keeps reference equality for parts of objects that are value equal', () => {
    const a = deepFreeze({ a: { some: 5 }, b: { c: { d: 6 } } }) as any
    const b = deepFreeze({ a: { some: 'other value' }, b: { c: { d: 6 } } })
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(a === r).toBeFalsy()
    expect(a.b === r.b).toBeTruthy()
  })

  it('keeps reference equality for parts of objects even if they have different keys', () => {
    const a = deepFreeze({
      a: ['this is', 'an array'],
      b: { c: { d: 6 } },
      extraKey: { extraPart: 'hello' },
    })
    const b = deepFreeze({ a: ['this is', 'an array'], b: { c: { d: 6 } } })
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(a === r).toBeFalsy()
    expect(a.a === r.a).toBeTruthy()
    expect(a.b === r.b).toBeTruthy()
  })

  it('keeps reference equality for parts of objects even if they have different keys 2', () => {
    const a = deepFreeze({
      a: ['this is', 'an array'],
      b: { c: { d: 6 } },
    }) as any
    const b = deepFreeze({
      a: ['this is', 'an array'],
      b: { c: { d: 6 } },
      extraKey: { extraPart: 'hello' },
    })
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(fastDeepEquals(b, r)).toBeTruthy()
    expect(a === r).toBeFalsy()
    expect(a.a === r.a).toBeTruthy()
    expect(a.b === r.b).toBeTruthy()
  })

  it('maintains the order for object keys from the new value with primitive values', () => {
    let oldObject: { [key: string]: any } = {}
    oldObject['a'] = 1
    oldObject['b'] = 2
    oldObject['c'] = 3

    let newObject: { [key: string]: any } = {}
    newObject['c'] = 3
    newObject['b'] = 2
    newObject['a'] = 1

    const result = keepDeepReferenceEqualityIfPossible(oldObject, newObject)
    expect(Object.keys(result)).toEqual(['c', 'b', 'a'])
  })

  it('maintains the order for object keys from the new value with complex values', () => {
    let oldObject: { [key: string]: any } = {}
    oldObject['a'] = { a: 1 }
    oldObject['b'] = { b: 2 }
    oldObject['c'] = { c: 3 }

    let newObject: { [key: string]: any } = {}
    newObject['c'] = { c: 3 }
    newObject['b'] = { b: 2 }
    newObject['a'] = { a: 1 }

    const result = keepDeepReferenceEqualityIfPossible(oldObject, newObject)
    expect(Object.keys(result)).toEqual(['c', 'b', 'a'])
  })

  it('ES6 Maps with identical keys and identical values', () => {
    const keyA = { a: 5 }
    const keyB = { b: '5' }
    const keyC = { c: { key: 5 } }

    const valueA = { a: 'value!' }
    const valueB = { b: 2 }
    const valueC = { c: { value: /regex/ } }

    let map1 = new Map()
    map1.set(keyA, valueA)
    map1.set(keyB, valueB)
    map1.set(keyC, valueC)
    deepFreeze(map1)

    let map2 = new Map()
    map2.set(keyA, valueA)
    map2.set(keyB, valueB)
    map2.set(keyC, valueC)
    deepFreeze(map2)

    const resultingMap = keepDeepReferenceEqualityIfPossible(map1, map2)
    expect(resultingMap).toEqual(map2)
    expect(resultingMap).toBe(map1)
  })

  it('ES6 Maps with cloned keys and identical values', () => {
    const keyA1 = { a: 5 }
    const keyB1 = { b: '5' }
    const keyC1 = { c: { key: 5 } }
    const keyA2 = { a: 5 }
    const keyB2 = { b: '5' }
    const keyC2 = { c: { key: 5 } }

    const valueA = { a: 'value!' }
    const valueB = { b: 2 }
    const valueC = { c: { value: /regex/ } }

    let map1 = new Map()
    map1.set(keyA1, valueA)
    map1.set(keyB1, valueB)
    map1.set(keyC1, valueC)
    deepFreeze(map1)

    let map2 = new Map()
    map2.set(keyA2, valueA)
    map2.set(keyB2, valueB)
    map2.set(keyC2, valueC)
    deepFreeze(map2)

    const resultingMap = keepDeepReferenceEqualityIfPossible(map1, map2)
    expect(resultingMap).toEqual(map2)
    expect(resultingMap.get(keyA2)).toBe(valueA)
    expect(resultingMap.get(keyB2)).toBe(valueB)
    expect(resultingMap.get(keyC2)).toBe(valueC)
  })

  it('ES6 Maps with cloned keys and cloned values', () => {
    const keyA1 = { a: 5 }
    const keyB1 = { b: '5' }
    const keyC1 = { c: { key: 5 } }
    const keyA2 = { a: 5 }
    const keyB2 = { b: '5' }
    const keyC2 = { c: { key: 5 } }

    const valueA1 = { a: 'value!' }
    const valueB1 = { b: 2 }
    const valueC1 = { c: { value: /regex/ } }
    const valueA2 = { a: 'value!' }
    const valueB2 = { b: 2 }
    const valueC2 = { c: { value: /regex/ } }

    let map1 = new Map()
    map1.set(keyA1, valueA1)
    map1.set(keyB1, valueB1)
    map1.set(keyC1, valueC1)
    deepFreeze(map1)

    let map2 = new Map()
    map2.set(keyA2, valueA2)
    map2.set(keyB2, valueB2)
    map2.set(keyC2, valueC2)
    deepFreeze(map2)

    const resultingMap = keepDeepReferenceEqualityIfPossible(map1, map2)
    expect(resultingMap).toEqual(map2)
    expect(resultingMap.get(keyA2)).toBe(valueA2)
    expect(resultingMap.get(keyB2)).toBe(valueB2)
    expect(resultingMap.get(keyC2)).toBe(valueC2)
  })

  it('ES6 Maps with identical keys and cloned values', () => {
    const keyA = { a: 5 }
    const keyB = { b: '5' }
    const keyC = { c: { key: 5 } }

    const valueA1 = { a: 'value!' }
    const valueB1 = { b: 2 }
    const valueC1 = { c: { value: /regex/ } }
    const valueA2 = { a: 'value!' }
    const valueB2 = { b: 2 }
    const valueC2 = { c: { value: /regex/ } }

    let map1 = new Map()
    map1.set(keyA, valueA1)
    map1.set(keyB, valueB1)
    map1.set(keyC, valueC1)
    deepFreeze(map1)

    let map2 = new Map()
    map2.set(keyA, valueA2)
    map2.set(keyB, valueB2)
    map2.set(keyC, valueC2)
    deepFreeze(map2)

    const resultingMap = keepDeepReferenceEqualityIfPossible(map1, map2)
    expect(resultingMap).toEqual(map2)
    expect(resultingMap).toBe(map1)
    expect(resultingMap.get(keyA)).toBe(valueA1)
    expect(resultingMap.get(keyB)).toBe(valueB1)
    expect(resultingMap.get(keyC)).toBe(valueC1)
  })

  it('keeps reference equality for parts of ES6 Maps even if they have different keys', () => {
    const keyA = { a: 5 }
    const keyB = { b: '5' }
    const keyC = { c: { key: 5 } }

    const valueA1 = { a: 'value!' }
    const valueB1 = { b: 2 }
    const valueA2 = { a: 'value!' }
    const valueB2 = { b: 2 }
    const valueC2 = { c: { value: /regex/ } }

    let map1 = new Map()
    map1.set(keyA, valueA1)
    map1.set(keyB, valueB1)
    deepFreeze(map1)

    let map2 = new Map()
    map2.set(keyA, valueA2)
    map2.set(keyB, valueB2)
    map2.set(keyC, valueC2)
    deepFreeze(map2)

    const resultingMap = keepDeepReferenceEqualityIfPossible(map1, map2)
    expect(resultingMap).toEqual(map2)
    expect(resultingMap.get(keyA)).toBe(valueA1)
    expect(resultingMap.get(keyB)).toBe(valueB1)
    expect(resultingMap.get(keyC)).toBe(valueC2)
  })

  it('ES6 Sets keep the old reference if the entire set is the same', () => {
    const valueA = { a: 'value!' }
    const valueB = { b: 2 }
    const valueC = { c: { value: /regex/ } }

    const set1 = new Set()
    set1.add(valueA)
    set1.add(valueB)
    set1.add(valueC)
    deepFreeze(set1)

    const set2 = new Set()
    set2.add(valueA)
    set2.add(valueB)
    set2.add(valueC)
    deepFreeze(set2)

    const resultingSet = keepDeepReferenceEqualityIfPossible(set1, set2)
    expect(resultingSet).toEqual(set2)
    expect(resultingSet).toBe(set1)
  })

  it('ES6 Sets cant keep reference equality if one set has more elements', () => {
    const valueA = { a: 'value!' }
    const valueB = { b: 2 }
    const valueC = { c: { value: /regex/ } }

    const set1 = new Set()
    set1.add(valueA)
    set1.add(valueB)
    deepFreeze(set1)

    const set2 = new Set()
    set2.add(valueA)
    set2.add(valueB)
    set2.add(valueC)
    deepFreeze(set2)

    const resultingSet = keepDeepReferenceEqualityIfPossible(set1, set2)
    expect(resultingSet).toEqual(set2)
    expect(resultingSet).not.toBe(set1)
    expect(resultingSet).toBe(set2)
  })

  it('cannot be defeated by a recursive object', () => {
    const sameValue = { a: 'same value' }

    const recursiveObject1 = {
      a: sameValue,
      b: { clonedValue: true },
      c: { more: sameValue, clone: { clone: true }, recursive: null as any },
    }
    const recursiveObject2 = {
      a: sameValue,
      b: { clonedValue: true },
      c: { more: sameValue, clone: { clone: true }, recursive: null as any },
    }
    recursiveObject1.c.recursive = recursiveObject1
    recursiveObject2.c.recursive = recursiveObject2
    deepFreeze(recursiveObject1)
    deepFreeze(recursiveObject2)

    const resultingObject = keepDeepReferenceEqualityIfPossible(recursiveObject1, recursiveObject2)
    expect(resultingObject).toEqual(recursiveObject2)
    expect(resultingObject.a).toBe(recursiveObject1.a)
    expect(resultingObject.b).toBe(recursiveObject1.b)
    expect(resultingObject.c.more).toBe(recursiveObject1.c.more)
    expect(resultingObject.c.clone).toBe(recursiveObject1.c.clone)
    expect(resultingObject.c.clone).not.toBe(recursiveObject2.c.clone)
    expect(resultingObject.c.recursive).toBe(recursiveObject2.c.recursive)
  })

  it('Keeps the Date reference for equal dates', () => {
    const date1 = new Date('December 17, 1995 03:24:00')
    const date2 = new Date('December 17, 1995 03:24:00')

    const result = keepDeepReferenceEqualityIfPossible(date1, date2)
    expect(result).toBe(date1)
  })

  it('Works for not equal dates', () => {
    const date1 = new Date('December 17, 1995 03:24:00')
    const date2 = new Date('December 17, 1995 03:24:10')

    const result = keepDeepReferenceEqualityIfPossible(date1, date2)
    expect(result).toBe(date2)
  })
})
