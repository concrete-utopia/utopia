import { HighlightBoundsForUids } from './project-file-types'
import { generateConsistentUID, UIDMappings, updateHighlightBounds } from './uid-utils'

let someExistingIDs: Set<string> = new Set(['aaa', 'pqr'])

describe('generateConsistentUID', () => {
  it('if the starting value is unused return it', () => {
    const actualResult = generateConsistentUID('xyz', someExistingIDs)
    expect(actualResult).toBe('xyz')
  })
  it('if the starting value is used generate another', () => {
    const actualResult = generateConsistentUID('pqr', someExistingIDs)
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
