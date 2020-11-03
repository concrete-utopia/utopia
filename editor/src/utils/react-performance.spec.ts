import * as R from 'ramda'

import { keepDeepReferenceEqualityIfPossible } from './react-performance'
import { deepFreeze } from './deep-freeze'

describe('keepDeepReferenceEqualityIfPossible', () => {
  it('keeps simple value equality', () => {
    const a = 'hello'
    const b = 'hello'
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(R.equals(b, r)).toBeTruthy()
    expect(a === r).toBeTruthy()
  })

  it('keeps reference equality for arrays if they are referentially equal', () => {
    const a = deepFreeze(['test', 'array'])
    const b = a
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(R.equals(b, r)).toBeTruthy()
    expect(a === r).toBeTruthy()
  })

  it('keeps reference equality for arrays if only their elements are referentially equal', () => {
    const a = deepFreeze(['test', 'array'])
    const b = deepFreeze(['test', 'array'])
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(R.equals(b, r)).toBeTruthy()
    expect(a === r).toBeTruthy()
  })

  it('keeps reference equality for arrays if only their elements are value equal', () => {
    const a = deepFreeze([{ a: 5 }, { b: { c: { d: 6 } } }])
    const b = deepFreeze([{ a: 5 }, { b: { c: { d: 6 } } }])
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(R.equals(b, r)).toBeTruthy()
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

  it('keeps reference equality for matching parts of arrays even if they are different in length', () => {
    const a = deepFreeze([{ a: 5 }, { b: { c: { d: 6 } } }]) as any[]
    const b = deepFreeze([{ a: 5 }]) as any[]
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(R.equals(b, r)).toBeTruthy()
    expect(r.length === 1).toBeTruthy
    expect(a === r).toBeFalsy()
    expect(a[0] === r[0]).toBeTruthy()
  })

  it('if a value is inserted in the middle, we keep reference equality only from one end', () => {
    const a = deepFreeze([{ a: 5 }, { b: 6 }] as any)
    const b = deepFreeze([{ a: 5 }, { c: 12 }, { b: 6 }] as any)
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(R.equals(b, r)).toBeTruthy()
    expect(r.length === 3).toBeTruthy()
    expect(a === r).toBeFalsy()
    expect(a[0] === r[0]).toBeTruthy()
    expect(r[0].a === 5).toBeTruthy()
  })

  it('keeps reference equality for individual array elements even if some of them are not the same', () => {
    const a = deepFreeze([{ a: 5 }, { b: 6 }])
    const b = deepFreeze([{ a: 5 }, { b: 15 }])
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(R.equals(b, r)).toBeTruthy()
    expect(a === r).toBeFalsy()
    expect(a[0] === r[0]).toBeTruthy()
    expect(a[1] === r[1]).toBeFalsy()
  })

  it('keeps reference equality for objects that are referentially equal', () => {
    const a = deepFreeze({ a: 5, b: { c: 6 } })
    const b = a
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(R.equals(b, r)).toBeTruthy()
    expect(a === r).toBeTruthy()
  })

  it('keeps reference equality for objects that are value equal', () => {
    const a = deepFreeze({ a: 5, b: { c: { d: 6 } } })
    const b = deepFreeze({ a: 5, b: { c: { d: 6 } } })
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(R.equals(b, r)).toBeTruthy()
    expect(a === r).toBeTruthy()
    expect(a.b.c === r.b.c).toBeTruthy()
  })

  it('keeps reference equality for parts of objects that are referentially equal', () => {
    const a = deepFreeze({ a: { some: 5 }, b: { c: { d: 6 } } }) as any
    const b = deepFreeze({ a: { some: 'other value' }, b: { c: a.b.c } })
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(R.equals(b, r)).toBeTruthy()
    expect(a === r).toBeFalsy()
    expect(a.b.c === r.b.c).toBeTruthy()
  })

  it('keeps reference equality for parts of objects that are value equal', () => {
    const a = deepFreeze({ a: { some: 5 }, b: { c: { d: 6 } } }) as any
    const b = deepFreeze({ a: { some: 'other value' }, b: { c: { d: 6 } } })
    const r = keepDeepReferenceEqualityIfPossible(a, b)
    expect(R.equals(b, r)).toBeTruthy()
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
    expect(R.equals(b, r)).toBeTruthy()
    expect(a === r).toBeFalsy()
    expect(a.a === r.a).toBeTruthy()
    expect(a.b === r.b).toBeTruthy()
  })

  it('keeps reference equality for parts of objects even if they have different keys', () => {
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
    expect(R.equals(b, r)).toBeTruthy()
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
})
