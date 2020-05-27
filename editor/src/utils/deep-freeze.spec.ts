import { deepFreeze } from './deep-freeze'

describe('deep-freeze', () => {
  it('freezes', () => {
    const anObject = { a: 5 }
    deepFreeze(anObject)
    expect(() => {
      anObject.a = 10
    }).toThrow()
  })

  it('deep freezes', () => {
    const anObject = { a: { b: { c: 10 } } }
    deepFreeze(anObject)
    expect(() => {
      anObject.a.b.c = 10
    }).toThrow()
  })

  it('skips deep freezing for objects that ask nicely', () => {
    const anObject: any = { a: { b: { c: 10, skipDeepFreeze: true } } }
    deepFreeze(anObject)
    anObject.a.b.c = 10
    expect(anObject.a.b.c).toEqual(10)
    expect(() => {
      anObject.a.b = { c: 15 }
    }).toThrow()
  })
})
