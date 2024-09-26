import type { HighlightBoundsForUids } from './project-file-types'
import type { UIDMappings } from './uid-utils'
import { generateConsistentUID, nextTestUID, updateHighlightBounds } from './uid-utils'

describe('generateConsistentUID', () => {
  it('if the starting value is unused return it', () => {
    const actualResult = generateConsistentUID('xyz')
    expect(actualResult).toBe('xyz')
  })
  it('if the starting value is used generate another', () => {
    const actualResult = generateConsistentUID('pqr')
    expect(actualResult).toBe('aab')
  })
})

describe('updateHighlightBounds', () => {
  it('caters for mappings which could destroy each other', () => {
    const highlightBounds: HighlightBoundsForUids = {
      ['first']: {
        startLine: 100,
        startCol: 1,
        endLine: 200,
        endCol: 2,
        uid: 'first',
      },
      ['second']: {
        startLine: 300,
        startCol: 3,
        endLine: 400,
        endCol: 4,
        uid: 'second',
      },
      ['third']: {
        startLine: 500,
        startCol: 5,
        endLine: 600,
        endCol: 6,
        uid: 'third',
      },
      ['fourth']: {
        startLine: 700,
        startCol: 7,
        endLine: 800,
        endCol: 8,
        uid: 'fourth',
      },
    }
    const mappings: UIDMappings = [
      { originalUID: 'first', newUID: 'second' },
      { originalUID: 'second', newUID: 'third' },
      { originalUID: 'third', newUID: 'first' },
    ]
    const actualResult = updateHighlightBounds(highlightBounds, mappings)
    const expectedResult: HighlightBoundsForUids = {
      ['second']: {
        startLine: 100,
        startCol: 1,
        endLine: 200,
        endCol: 2,
        uid: 'second',
      },
      ['third']: {
        startLine: 300,
        startCol: 3,
        endLine: 400,
        endCol: 4,
        uid: 'third',
      },
      ['first']: {
        startLine: 500,
        startCol: 5,
        endLine: 600,
        endCol: 6,
        uid: 'first',
      },
      ['fourth']: {
        startLine: 700,
        startCol: 7,
        endLine: 800,
        endCol: 8,
        uid: 'fourth',
      },
    }
    expect(actualResult).toEqual(expectedResult)
  })
})

describe('nextTestUID', () => {
  it("increments a string's last character", async () => {
    expect(nextTestUID('aaa')).toEqual('aab')
    expect(nextTestUID('aab')).toEqual('aac')
    expect(nextTestUID('aba')).toEqual('abb')
    expect(nextTestUID('aaz')).toEqual('aba')
    expect(nextTestUID('azz')).toEqual('baa')
    expect(nextTestUID('a9a')).toEqual('aab')
    expect(nextTestUID('aa9')).toEqual('aab')
  })
  it('throws if it cannot generate anymore', async () => {
    expect(() => nextTestUID('zzz')).toThrow('Unable to generate a UID')
  })
})
