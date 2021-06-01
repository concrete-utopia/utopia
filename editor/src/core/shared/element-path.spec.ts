import * as Chai from 'chai'
import * as EP from './element-path'
import { BakedInStoryboardUID } from '../model/scene-utils'
const chaiExpect = Chai.expect

describe('serialization', () => {
  it('path survives serialization', () => {
    const path = EP.elementPath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['VIEW1', 'VIEW2'],
    ])
    const pathString = EP.toComponentId(path)
    const restoredPath = EP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })

  it('empty path survives serialization', () => {
    const path = EP.elementPath([])
    const pathString = EP.toComponentId(path)
    const restoredPath = EP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })
})

describe('factory function', () => {
  it('caches results', () => {
    const first = EP.elementPath([['A', '1'], ['B', '2'], ['C']])
    const second = EP.elementPath([['A', '1'], ['B', '2'], ['C']])
    expect(first === second).toBeTruthy()
  })
})

describe('isStoryboardPath', () => {
  it('returns true for the storyboard', () => {
    expect(EP.isStoryboardPath(EP.elementPath([[BakedInStoryboardUID]]))).toBeTruthy()
  })
  it('returns false for any other path', () => {
    expect(EP.isStoryboardPath(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa']]))).toBeFalsy()

    expect(
      EP.isStoryboardPath(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa', 'app']])),
    ).toBeFalsy()

    expect(EP.isStoryboardPath(EP.elementPath([[BakedInStoryboardUID], ['app']]))).toBeFalsy()

    expect(
      EP.isStoryboardPath(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['app']])),
    ).toBeFalsy()
  })
})

describe('isStoryboardChild', () => {
  it('returns true for a child of the storyboard', () => {
    expect(EP.isStoryboardChild(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa']]))).toBeTruthy()
  })
  it('returns false for any other path', () => {
    expect(EP.isStoryboardChild(EP.elementPath([[BakedInStoryboardUID]]))).toBeFalsy()

    expect(
      EP.isStoryboardChild(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa', 'app']])),
    ).toBeFalsy()

    expect(EP.isStoryboardChild(EP.elementPath([[BakedInStoryboardUID], ['app']]))).toBeFalsy()

    expect(
      EP.isStoryboardChild(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['app']])),
    ).toBeFalsy()
  })
})

describe('isStoryboardDescendant', () => {
  it('returns true for any direct descendant of the storyboard', () => {
    expect(
      EP.isStoryboardDescendant(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa']])),
    ).toBeTruthy()

    expect(
      EP.isStoryboardDescendant(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa', 'app']])),
    ).toBeTruthy()
  })
  it('returns false for any other path', () => {
    expect(EP.isStoryboardDescendant(EP.elementPath([[BakedInStoryboardUID]]))).toBeFalsy()

    expect(EP.isStoryboardDescendant(EP.elementPath([[BakedInStoryboardUID], ['app']]))).toBeFalsy()

    expect(
      EP.isStoryboardDescendant(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['app']])),
    ).toBeFalsy()
  })
})

describe('isRootElementOfInstance', () => {
  it('returns true for any root element of an instance', () => {
    expect(
      EP.isRootElementOfInstance(EP.elementPath([[BakedInStoryboardUID], ['app']])),
    ).toBeTruthy()

    expect(
      EP.isRootElementOfInstance(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['app']])),
    ).toBeTruthy()
  })
  it('returns false for any other path', () => {
    expect(EP.isRootElementOfInstance(EP.elementPath([[BakedInStoryboardUID]]))).toBeFalsy()

    expect(
      EP.isRootElementOfInstance(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa']])),
    ).toBeFalsy()

    expect(
      EP.isRootElementOfInstance(EP.elementPath([[BakedInStoryboardUID, 'scene-aaa', 'app']])),
    ).toBeFalsy()
  })
})

describe('appending to a path', () => {
  it('appendToPath appends to the last part', () => {
    const start = EP.elementPath([
      ['A', 'B'],
      ['C', 'D'],
    ])
    const singleElementAdded = EP.appendToPath(start, 'E')
    const singleElementAddedViaArray = EP.appendToPath(start, ['E'])
    const multipleElementsAdded = EP.appendToPath(start, ['E', 'F'])

    expect(singleElementAdded).toEqual(
      EP.elementPath([
        ['A', 'B'],
        ['C', 'D', 'E'],
      ]),
    )
    expect(singleElementAddedViaArray).toEqual(
      EP.elementPath([
        ['A', 'B'],
        ['C', 'D', 'E'],
      ]),
    )
    expect(multipleElementsAdded).toEqual(
      EP.elementPath([
        ['A', 'B'],
        ['C', 'D', 'E', 'F'],
      ]),
    )
  })

  it('appendToPath works with an empty path', () => {
    const start = EP.emptyElementPath
    const singleElementAdded = EP.appendToPath(start, 'E')
    const singleElementAddedViaArray = EP.appendToPath(start, ['E'])
    const multipleElementsAdded = EP.appendToPath(start, ['E', 'F'])

    expect(singleElementAdded).toEqual(EP.elementPath([['E']]))
    expect(singleElementAddedViaArray).toEqual(EP.elementPath([['E']]))
    expect(multipleElementsAdded).toEqual(EP.elementPath([['E', 'F']]))
  })

  it('appendNewElementPath appends a new element path array', () => {
    const start = EP.elementPath([
      ['A', 'B'],
      ['C', 'D'],
    ])
    const singleElementAdded = EP.appendNewElementPath(start, 'E')
    const singleElementAddedViaArray = EP.appendNewElementPath(start, ['E'])
    const multipleElementsAdded = EP.appendNewElementPath(start, ['E', 'F'])

    expect(singleElementAdded).toEqual(EP.elementPath([['A', 'B'], ['C', 'D'], ['E']]))
    expect(singleElementAddedViaArray).toEqual(EP.elementPath([['A', 'B'], ['C', 'D'], ['E']]))
    expect(multipleElementsAdded).toEqual(
      EP.elementPath([
        ['A', 'B'],
        ['C', 'D'],
        ['E', 'F'],
      ]),
    )
  })

  it('appendNewElementPath works with an empty path', () => {
    const start = EP.emptyElementPath
    const singleElementAdded = EP.appendNewElementPath(start, 'E')
    const singleElementAddedViaArray = EP.appendNewElementPath(start, ['E'])
    const multipleElementsAdded = EP.appendNewElementPath(start, ['E', 'F'])

    expect(singleElementAdded).toEqual(EP.elementPath([['E']]))
    expect(singleElementAddedViaArray).toEqual(EP.elementPath([['E']]))
    expect(multipleElementsAdded).toEqual(EP.elementPath([['E', 'F']]))
  })
})

describe('pathsEqual', () => {
  it('returns true for empty paths', () => {
    const l = EP.elementPath([])
    const r = EP.elementPath([])
    expect(EP.pathsEqual(l, r)).toBeTruthy()
  })

  it('returns true for matching single length paths', () => {
    const l = EP.elementPath([['A', 'B']])
    const r = EP.elementPath([['A', 'B']])
    expect(EP.pathsEqual(l, r)).toBeTruthy()
  })

  it('returns true for matching longer paths', () => {
    const l = EP.elementPath([['A', 'B'], ['C'], ['D']])
    const r = EP.elementPath([['A', 'B'], ['C'], ['D']])
    expect(EP.pathsEqual(l, r)).toBeTruthy()
  })

  it('returns false for non-matching single length paths', () => {
    const l = EP.elementPath([['A', 'B']])
    const r = EP.elementPath([['C', 'D']])
    expect(EP.pathsEqual(l, r)).toBeFalsy()
  })

  it('returns false for non-matching longer paths', () => {
    const l = EP.elementPath([['A', 'B'], ['C'], ['D']])
    const r = EP.elementPath([['A', 'B'], ['C'], ['F']])
    expect(EP.pathsEqual(l, r)).toBeFalsy()
  })

  it('returns false for non-matching paths of different lengths', () => {
    const l = EP.elementPath([['A', 'B'], ['C'], ['D']])
    const r = EP.elementPath([['A', 'B'], ['C'], ['D', 'E']])
    const r2 = EP.elementPath([['A', 'B'], ['C'], ['D'], ['E']])

    expect(EP.pathsEqual(l, r)).toBeFalsy()
    expect(EP.pathsEqual(l, r2)).toBeFalsy()
  })
})

describe('isDescendantOf', () => {
  it('returns true if it is a descendant from the same instance', () => {
    const result = EP.isDescendantOf(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['X', 'Y'],
      ]),
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
    )
    chaiExpect(result).to.be.true
  })

  it('returns true if it is a descendant from a higher instance', () => {
    const result = EP.isDescendantOf(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['X', 'Y'],
      ]),
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa']]),
    )
    chaiExpect(result).to.be.true
  })

  it('returns true if it is a descendant of a descendant from a higher instance', () => {
    const result = EP.isDescendantOf(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['X', 'Y'],
      ]),
      EP.elementPath([[BakedInStoryboardUID]]),
    )
    chaiExpect(result).to.be.true
  })

  it('returns false if it is the same path', () => {
    const result = EP.isDescendantOf(
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
    )
    chaiExpect(result).to.be.false
  })

  it('returns false if not a descendant', () => {
    const result = EP.isDescendantOf(
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['Y']]),
    )
    chaiExpect(result).to.be.false
  })

  it('returns false if it is a parent of the target', () => {
    const result = EP.isDescendantOf(
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['X', 'Y'],
      ]),
    )
    chaiExpect(result).to.be.false
  })
})

describe('replaceIfAncestor', () => {
  it('where the path does not match', () => {
    const result = EP.replaceIfAncestor(
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['A']]),
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['B']]),
    )
    chaiExpect(result).to.be.null
  })
  it('where the path matches exactly', () => {
    const result = EP.replaceIfAncestor(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D'],
      ]),
    )
    expect(result).toEqual(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D'],
      ]),
    )
  })
  it('where the path is an ancestor', () => {
    const result = EP.replaceIfAncestor(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B', 'X'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D'],
      ]),
    )
    chaiExpect(result).to.deep.equal(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D', 'X'],
      ]),
    )
  })
  it('where the path is an ancestor of a longer path', () => {
    const result = EP.replaceIfAncestor(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
        ['E', 'F'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D'],
      ]),
    )
    expect(result).toEqual(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D'],
        ['E', 'F'],
      ]),
    )
  })
  it('where the path is an ancestor of part of a longer path', () => {
    const result = EP.replaceIfAncestor(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
        ['E', 'F'],
      ]),
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['A']]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D'],
      ]),
    )
    expect(result).toEqual(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D', 'B'],
        ['E', 'F'],
      ]),
    )
  })
  it('where the path is an ancestor of a longer path and the replacement is null', () => {
    const result = EP.replaceIfAncestor(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
        ['E', 'F'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
      ]),
      null,
    )
    expect(result).toEqual(EP.elementPath([['E', 'F']]))
  })
  it('where the path is an ancestor of part of a longer path and the replacement is null', () => {
    const result = EP.replaceIfAncestor(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
        ['E', 'F'],
      ]),
      EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['A']]),
      null,
    )
    expect(result).toEqual(EP.elementPath([['B'], ['E', 'F']]))
  })
})

describe('fromString', () => {
  it('parses a simple path correctly', () => {
    const expectedResult = EP.elementPath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['A', 'B', 'C'],
    ])
    const actualResult = EP.fromString(EP.toComponentId(expectedResult))
    chaiExpect(actualResult).to.deep.equal(expectedResult)
  })
})

describe('closestSharedAncestor', () => {
  it('returns the original path for equal paths when includePathsEqual is true', () => {
    const actualResult = EP.closestSharedAncestor(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'c'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'c'],
      ]),
      true,
    )
    const expectedResult = EP.elementPath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['a', 'b', 'c'],
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('returns the original path parent for equal paths when includePathsEqual is false', () => {
    const actualResult = EP.closestSharedAncestor(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'c'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'c'],
      ]),
      false,
    )
    const expectedResult = EP.elementPath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['a', 'b'],
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('returns the common parent for simple cases', () => {
    const actualResult = EP.closestSharedAncestor(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'c'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'd'],
      ]),
      false,
    )
    const expectedResult = EP.elementPath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['a', 'b'],
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('returns the common ancestor for more complex cases', () => {
    const actualResult = EP.closestSharedAncestor(
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b'],
        ['x', 'y', 'z'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'c', 'd'],
        ['e', 'f'],
        ['g', 'h'],
      ]),
      false,
    )
    const expectedResult = EP.elementPath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['a', 'b'],
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('returns null when there are no shared ancestors', () => {
    const actualResult = EP.closestSharedAncestor(
      EP.elementPath([
        ['a', 'b'],
        ['x', 'y', 'z'],
      ]),
      EP.elementPath([
        ['e', 'f'],
        ['g', 'h'],
      ]),
      false,
    )
    expect(actualResult).toBeNull()
  })
})

describe('getCommonParent', () => {
  it('empty array returns null', () => {
    const actualResult = EP.getCommonParent([])
    expect(actualResult).toBeNull()
  })
  it('single element returns the parent', () => {
    const actualResult = EP.getCommonParent([
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b'],
      ]),
    ])
    const expectedResult = EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['a']])
    expect(actualResult).toEqual(expectedResult)
  })
  it('for two elements returns the common parent', () => {
    const actualResult = EP.getCommonParent([
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'c'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'd'],
      ]),
    ])
    const expectedResult = EP.elementPath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['a', 'b'],
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('for three elements with a common parent returns that', () => {
    const actualResult = EP.getCommonParent([
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b'],
        ['x', 'y', 'z'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'c', 'd'],
        ['e', 'f'],
        ['g', 'h'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'c'],
        ['h', 'i', 'j'],
      ]),
    ])
    const expectedResult = EP.elementPath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['a', 'b'],
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('for three elements without a common parent returns null', () => {
    const actualResult = EP.getCommonParent([
      EP.elementPath([['scene-aaa'], ['a', 'b', 'c']]),
      EP.elementPath([['scene-bbb'], ['a', 'b', 'd']]),
      EP.elementPath([['scene-ccc'], ['x', 'b', 'd']]),
    ])
    expect(actualResult).toBeNull()
  })
  it('returns one of the passed paths if it is a common parent of the rest and includeSelf is set to true', () => {
    const actualResult = EP.getCommonParent(
      [
        EP.elementPath([
          [BakedInStoryboardUID, 'scene-aaa'],
          ['a', 'b'],
        ]),
        EP.elementPath([
          [BakedInStoryboardUID, 'scene-aaa'],
          ['a', 'b', 'd'],
        ]),
      ],
      true,
    )
    const expectedResult = EP.elementPath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['a', 'b'],
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('does not return one of the passed paths if it is a common parent of the rest and includeSelf is not set', () => {
    const actualResult = EP.getCommonParent([
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b'],
      ]),
      EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'd'],
      ]),
    ])
    const expectedResult = EP.elementPath([[BakedInStoryboardUID, 'scene-aaa'], ['a']])
    expect(actualResult).toEqual(expectedResult)
  })
})
