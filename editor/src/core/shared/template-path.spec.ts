import * as Chai from 'chai'
import * as TP from './template-path'
import { BakedInStoryboardUID } from '../model/scene-utils'
import { testStaticInstancePath, testStaticScenePath } from './template-path.test-utils'
const chaiExpect = Chai.expect

describe('serialization', () => {
  it('path survives serialization', () => {
    const path = TP.templatePath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['VIEW1', 'VIEW2'],
    ])
    const pathString = TP.toComponentId(path)
    const restoredPath = TP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })

  it('empty path survives serialization', () => {
    const path = TP.templatePath([])
    const pathString = TP.toComponentId(path)
    const restoredPath = TP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })
})

describe('factory function', () => {
  it('caches results', () => {
    const first = TP.templatePath([['A', '1'], ['B', '2'], ['C']])
    const second = TP.templatePath([['A', '1'], ['B', '2'], ['C']])
    expect(first === second).toBeTruthy()
  })
})

describe('pathsEqual', () => {
  it('returns true for empty paths', () => {
    const l = TP.templatePath([])
    const r = TP.templatePath([])
    expect(TP.pathsEqual(l, r)).toBeTruthy()
  })

  it('returns true for matching single length paths', () => {
    const l = TP.templatePath([['A', 'B']])
    const r = TP.templatePath([['A', 'B']])
    expect(TP.pathsEqual(l, r)).toBeTruthy()
  })

  it('returns true for matching longer paths', () => {
    const l = TP.templatePath([['A', 'B'], ['C'], ['D']])
    const r = TP.templatePath([['A', 'B'], ['C'], ['D']])
    expect(TP.pathsEqual(l, r)).toBeTruthy()
  })

  it('returns false for non-matching single length paths', () => {
    const l = TP.templatePath([['A', 'B']])
    const r = TP.templatePath([['C', 'D']])
    expect(TP.pathsEqual(l, r)).toBeFalsy()
  })

  it('returns false for non-matching longer paths', () => {
    const l = TP.templatePath([['A', 'B'], ['C'], ['D']])
    const r = TP.templatePath([['A', 'B'], ['C'], ['F']])
    expect(TP.pathsEqual(l, r)).toBeFalsy()
  })

  it('returns false for non-matching paths of different lengths', () => {
    const l = TP.templatePath([['A', 'B'], ['C'], ['D']])
    const r = TP.templatePath([['A', 'B'], ['C'], ['D', 'E']])
    const r2 = TP.templatePath([['A', 'B'], ['C'], ['D'], ['E']])

    expect(TP.pathsEqual(l, r)).toBeFalsy()
    expect(TP.pathsEqual(l, r2)).toBeFalsy()
  })
})

describe('isDescendantOf', () => {
  it('returns true if it is a descendant from the same instance', () => {
    const result = TP.isDescendantOf(
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['X', 'Y'],
      ]),
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
    )
    chaiExpect(result).to.be.true
  })

  it('returns true if it is a descendant from a higher instance', () => {
    const result = TP.isDescendantOf(
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['X', 'Y'],
      ]),
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa']]),
    )
    chaiExpect(result).to.be.true
  })

  it('returns true if it is a descendant of a descendant from a higher instance', () => {
    const result = TP.isDescendantOf(
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['X', 'Y'],
      ]),
      TP.templatePath([[BakedInStoryboardUID]]),
    )
    chaiExpect(result).to.be.true
  })

  it('returns false if it is the same path', () => {
    const result = TP.isDescendantOf(
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
    )
    chaiExpect(result).to.be.false
  })

  it('returns false if not a descendant', () => {
    const result = TP.isDescendantOf(
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['Y']]),
    )
    chaiExpect(result).to.be.false
  })

  it('returns false if it is a parent of the target', () => {
    const result = TP.isDescendantOf(
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['X', 'Y'],
      ]),
    )
    chaiExpect(result).to.be.false
  })
})

describe('isAncestorOf', () => {
  it('is not an ancestor', () => {
    const result = TP.isAncestorOf(
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['Y']]),
    )
    chaiExpect(result).to.be.false
  })

  it('is an ancestor', () => {
    const result = TP.isAncestorOf(
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['X', 'Y'],
      ]),
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
    )
    chaiExpect(result).to.be.true
  })

  it('the two paths are the same', () => {
    const result = TP.isAncestorOf(
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
    )
    chaiExpect(result).to.be.true
  })

  it('does not match same paths with flag set to false', () => {
    const result = TP.isAncestorOf(
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      false,
    )
    chaiExpect(result).to.be.false
  })

  it('target ancestor is children of path', () => {
    const result = TP.isAncestorOf(
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['X', 'Y'],
      ]),
    )
    chaiExpect(result).to.be.false
  })
})

describe('replaceIfAncestor', () => {
  it('where the path does not match', () => {
    const result = TP.replaceIfAncestor(
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['X']]),
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['A']]),
      TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['B']]),
    )
    chaiExpect(result).to.be.null
  })
  it('where the path matches exactly', () => {
    const result = TP.replaceIfAncestor(
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
      ]),
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
      ]),
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D'],
      ]),
    )
    expect(result).toEqual(
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D'],
      ]),
    )
  })
  it('where the path is an ancestor', () => {
    const result = TP.replaceIfAncestor(
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B', 'X'],
      ]),
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['A', 'B'],
      ]),
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D'],
      ]),
    )
    chaiExpect(result).to.deep.equal(
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['C', 'D', 'X'],
      ]),
    )
  })
})

describe('fromString', () => {
  it('parses a simple path correctly', () => {
    const expectedResult = TP.templatePath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['A', 'B', 'C'],
    ])
    const actualResult = TP.fromString(TP.toComponentId(expectedResult))
    chaiExpect(actualResult).to.deep.equal(expectedResult)
  })
})

describe('getCommonParent', () => {
  it('empty array returns null', () => {
    const actualResult = TP.getCommonParent([])
    expect(actualResult).toBeNull()
  })
  it('single element returns the parent', () => {
    const actualResult = TP.getCommonParent([
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b'],
      ]),
    ])
    const expectedResult = TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['a']])
    expect(actualResult).toEqual(expectedResult)
  })
  it('for two elements returns the common parent', () => {
    const actualResult = TP.getCommonParent([
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'c'],
      ]),
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'd'],
      ]),
    ])
    const expectedResult = TP.templatePath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['a', 'b'],
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('for three elements without a common parent returns null', () => {
    const actualResult = TP.getCommonParent([
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'c'],
      ]),
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-bbb'],
        ['a', 'b', 'd'],
      ]),
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-ccc'],
        ['x', 'b', 'd'],
      ]),
    ])
    expect(actualResult).toBeNull()
  })
  it('returns one of the passed paths if it is a common parent of the rest and includeSelf is set to true', () => {
    const actualResult = TP.getCommonParent(
      [
        TP.templatePath([
          [BakedInStoryboardUID, 'scene-aaa'],
          ['a', 'b'],
        ]),
        TP.templatePath([
          [BakedInStoryboardUID, 'scene-aaa'],
          ['a', 'b', 'd'],
        ]),
      ],
      true,
    )
    const expectedResult = TP.templatePath([
      [BakedInStoryboardUID, 'scene-aaa'],
      ['a', 'b'],
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('does not return one of the passed paths if it is a common parent of the rest and includeSelf is not set', () => {
    const actualResult = TP.getCommonParent([
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b'],
      ]),
      TP.templatePath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['a', 'b', 'd'],
      ]),
    ])
    const expectedResult = TP.templatePath([[BakedInStoryboardUID, 'scene-aaa'], ['a']])
    expect(actualResult).toEqual(expectedResult)
  })
})

describe('Scenes, Instances and Element Paths', () => {
  const appInstanceElementPath = ['storyboard', 'app-instance']
  const cardInstanceElementPath = ['app-root', 'card-instance']
  const cardRootElementPath = ['card-root']

  const appScenePath = testStaticScenePath([appInstanceElementPath])
  const cardScenePath = testStaticScenePath([appInstanceElementPath, cardInstanceElementPath])
  const cardRootScenePath = testStaticScenePath([
    appInstanceElementPath,
    cardInstanceElementPath,
    cardRootElementPath,
  ])

  const appInstancePath = testStaticInstancePath(TP.emptyScenePath, appInstanceElementPath)
  const cardInstancePath = testStaticInstancePath(appScenePath, cardInstanceElementPath)
  const cardRootInstancePath = testStaticInstancePath(cardScenePath, cardRootElementPath)

  it('scenePathPartOfTemplatePath returns the scene path part of a given instance path', () => {
    expect(TP.scenePathPartOfTemplatePath(TP.emptyInstancePath)).toEqual(TP.emptyScenePath)
    expect(TP.scenePathPartOfTemplatePath(appInstancePath)).toEqual(TP.emptyScenePath)
    expect(TP.scenePathPartOfTemplatePath(cardInstancePath)).toEqual(appScenePath)
    expect(TP.scenePathPartOfTemplatePath(cardRootInstancePath)).toEqual(cardScenePath)
  })

  it('calling scenePathPartOfTemplatePath with a scene path returns that same scene path', () => {
    expect(TP.scenePathPartOfTemplatePath(TP.emptyScenePath)).toEqual(TP.emptyScenePath)
    expect(TP.scenePathPartOfTemplatePath(appScenePath)).toEqual(appScenePath)
    expect(TP.scenePathPartOfTemplatePath(cardScenePath)).toEqual(cardScenePath)
    expect(TP.scenePathPartOfTemplatePath(cardRootScenePath)).toEqual(cardRootScenePath)
  })

  it('instancePathForElementAtScenePath creates a new instance path pointing to last element path of a scene', () => {
    expect(TP.instancePathForElementAtScenePath(TP.emptyScenePath)).toEqual(TP.emptyInstancePath)
    expect(TP.instancePathForElementAtScenePath(appScenePath)).toEqual(appInstancePath)
    expect(TP.instancePathForElementAtScenePath(cardScenePath)).toEqual(cardInstancePath)
    expect(TP.instancePathForElementAtScenePath(cardRootScenePath)).toEqual(cardRootInstancePath)
  })

  it('scenePathForElementAtInstancePath creates a new scene path pointing to full element path of an instance path', () => {
    expect(TP.scenePathForElementAtInstancePath(TP.emptyInstancePath)).toEqual(TP.emptyScenePath)
    expect(TP.scenePathForElementAtInstancePath(appInstancePath)).toEqual(appScenePath)
    expect(TP.scenePathForElementAtInstancePath(cardInstancePath)).toEqual(cardScenePath)
    expect(TP.scenePathForElementAtInstancePath(cardRootInstancePath)).toEqual(cardRootScenePath)
  })
})
