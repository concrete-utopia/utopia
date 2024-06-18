import type { HighlightBoundsForUids } from './project-file-types'
import type { UIDMappings } from './uid-utils'
import { generateConsistentUID, incrementTestUID, updateHighlightBounds } from './uid-utils'

let someExistingIDs: Set<string> = new Set(['aaa', 'pqr', 'bbb', 'bbc'])

describe('generateConsistentUID', () => {
  it('if the starting value is unused return it', () => {
    const actualResult = generateConsistentUID('xyz', someExistingIDs)
    expect(actualResult).toBe('xyz')
  })
  it('if the starting value is used generate another', () => {
    const actualResult = generateConsistentUID('pqr', someExistingIDs)
    expect(actualResult).toBe('pqs')
  })
  it('if the starting value is used generate another, continuously', () => {
    const actualResult = generateConsistentUID('bbb', someExistingIDs)
    expect(actualResult).toBe('bbd')
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

describe('incrementTestUID', () => {
  it("increments a string's last character", async () => {
    expect(incrementTestUID('aaa')).toEqual('aab')
    expect(incrementTestUID('aab')).toEqual('aac')
    expect(incrementTestUID('aba')).toEqual('abb')
    expect(incrementTestUID('aaz')).toEqual('aba')
    expect(incrementTestUID('azz')).toEqual('baa')
    expect(incrementTestUID('a9a')).toEqual('aab')
    expect(incrementTestUID('aa9')).toEqual('aab')
  })
  it('throws if it cannot generate anymore', async () => {
    expect(() => incrementTestUID('zzz')).toThrow('Unable to generate a UID')
  })
})
