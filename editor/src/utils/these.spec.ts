import type { These } from './these'
import {
  isThis,
  makeThis,
  makeThat,
  makeThisAndThat,
  isThisAndThat,
  isThat,
  foldThese,
  setThis,
  setThat,
  thisMap,
  thatMap,
  mergeThese,
} from './these'

describe('isThis', () => {
  it('returns true for a this', () => {
    expect(isThis(makeThis(10))).toEqual(true)
  })
  it('returns false for a that', () => {
    expect(isThis(makeThat('test'))).toEqual(false)
  })
  it('returns false for a this and that', () => {
    expect(isThis(makeThisAndThat(10, 'test'))).toEqual(false)
  })
})

describe('isThat', () => {
  it('returns true for a this', () => {
    expect(isThat(makeThis(10))).toEqual(false)
  })
  it('returns false for a that', () => {
    expect(isThat(makeThat('test'))).toEqual(true)
  })
  it('returns false for a this and that', () => {
    expect(isThat(makeThisAndThat(10, 'test'))).toEqual(false)
  })
})

describe('isThisAndThat', () => {
  it('returns true for a this', () => {
    expect(isThisAndThat(makeThis(10))).toEqual(false)
  })
  it('returns false for a that', () => {
    expect(isThisAndThat(makeThat('test'))).toEqual(false)
  })
  it('returns false for a this and that', () => {
    expect(isThisAndThat(makeThisAndThat(10, 'test'))).toEqual(true)
  })
})

function runFoldThese(these: These<number, string>): string {
  return foldThese<number, string, string>(
    (thisValue) => (thisValue * 2).toString(),
    (thatValue) => thatValue + 'hat',
    (thisValue, thatValue) => `${thisValue}-${thatValue}`,
    these,
  )
}

describe('foldThese', () => {
  it('applies the transform as expected to a this', () => {
    expect(runFoldThese(makeThis(10))).toEqual('20')
  })
  it('applies the transform as expected to a that', () => {
    expect(runFoldThese(makeThat('car'))).toEqual('carhat')
  })
  it('applies the transform as expected to a this and that', () => {
    expect(runFoldThese(makeThisAndThat(30, 'spaceship'))).toEqual('30-spaceship')
  })
})

describe('setThis', () => {
  it('updates the this value for a this', () => {
    expect(setThis(10, makeThis(5))).toEqual(makeThis(10))
  })
  it('creates a this and that for a that', () => {
    expect(setThis(10, makeThat('plane'))).toEqual(makeThisAndThat(10, 'plane'))
  })
  it('updates the this value for a this and that', () => {
    expect(setThis(10, makeThisAndThat(5, 'plane'))).toEqual(makeThisAndThat(10, 'plane'))
  })
})

describe('setThat', () => {
  it('creates a this and that for a this', () => {
    expect(setThat('plane', makeThis(5))).toEqual(makeThisAndThat(5, 'plane'))
  })
  it('updates the that value for a that', () => {
    expect(setThat('plane', makeThat('helicopter'))).toEqual(makeThat('plane'))
  })
  it('updates the that value for a this and that', () => {
    expect(setThat('plane', makeThisAndThat(5, 'helicopter'))).toEqual(makeThisAndThat(5, 'plane'))
  })
})

describe('thisMap', () => {
  it('transforms the this part of a this', () => {
    expect(thisMap((n: number) => n * 2, makeThis(5))).toEqual(makeThis(10))
  })
  it('does nothing to a that', () => {
    expect(thisMap((n: number) => n * 2, makeThat('plane'))).toEqual(makeThat('plane'))
  })
  it('transforms the this part of a this and that', () => {
    expect(thisMap((n: number) => n * 2, makeThisAndThat(5, 'plane'))).toEqual(
      makeThisAndThat(10, 'plane'),
    )
  })
})

describe('thatMap', () => {
  it('does nothing to a this', () => {
    expect(thatMap((s: string) => `${s}seat`, makeThis(5))).toEqual(makeThis(5))
  })
  it('transforms the that part of a that', () => {
    expect(thatMap((s: string) => `${s}seat`, makeThat('plane'))).toEqual(makeThat('planeseat'))
  })
  it('transforms the that part of a this and that', () => {
    expect(thatMap((s: string) => `${s}seat`, makeThisAndThat(5, 'plane'))).toEqual(
      makeThisAndThat(5, 'planeseat'),
    )
  })
})

describe('mergeThese', () => {
  it('returns the this value for a this', () => {
    expect(mergeThese((first: number, second: number) => first + second, makeThis(5))).toEqual(5)
  })
  it('returns the that value for a that', () => {
    expect(mergeThese((first: number, second: number) => first + second, makeThat(10))).toEqual(10)
  })
  it('merges the contents of a this and that', () => {
    expect(
      mergeThese((first: number, second: number) => first + second, makeThisAndThat(5, 10)),
    ).toEqual(15)
  })
})
