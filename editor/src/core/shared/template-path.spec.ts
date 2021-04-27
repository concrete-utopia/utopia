import * as Chai from 'chai'
import * as TP from './template-path'
import { BakedInStoryboardUID } from '../model/scene-utils'
import { testStaticInstancePath, testStaticScenePath } from './template-path.test-utils'
const chaiExpect = Chai.expect

describe('serialization', function () {
  it('path survives serialization', function () {
    const path = TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), [
      'VIEW1',
      'VIEW2',
    ])
    const pathString = TP.toComponentId(path)
    const restoredPath = TP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })

  it('empty path survives serialization', function () {
    const path = TP.instancePath(TP.emptyScenePath, [])
    const pathString = TP.toComponentId(path)
    const restoredPath = TP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })

  it('ScenePath survives serialization', function () {
    const path = TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']])
    const pathString = TP.toString(path)
    const restoredPath = TP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })

  it('Empty ScenePath survives serialization', function () {
    const path = TP.emptyScenePath
    const pathString = TP.toString(path)
    const restoredPath = TP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })
})

describe('isDescendantOf', () => {
  it('returns true if it is a descendant from the same instance', () => {
    const result = TP.isDescendantOf(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X', 'Y']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
    )
    chaiExpect(result).to.be.true
  })

  it('returns true if it is a descendant from a higher instance', () => {
    const result = TP.isDescendantOf(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X', 'Y']),
      TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]),
    )
    chaiExpect(result).to.be.true
  })

  it('returns true if it is a descendant of a descendant from a higher instance', () => {
    const result = TP.isDescendantOf(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X', 'Y']),
      TP.scenePath([[BakedInStoryboardUID]]),
    )
    chaiExpect(result).to.be.true
  })

  it('returns false if it is the same path', () => {
    const result = TP.isDescendantOf(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
    )
    chaiExpect(result).to.be.false
  })

  it('returns false if not a descendant', () => {
    const result = TP.isDescendantOf(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['Y']),
    )
    chaiExpect(result).to.be.false
  })

  it('returns false if it is a parent of the target', () => {
    const result = TP.isDescendantOf(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X', 'Y']),
    )
    chaiExpect(result).to.be.false
  })
})

describe('isAncestorOf', () => {
  it('is not an ancestor', () => {
    const result = TP.isAncestorOf(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['Y']),
    )
    chaiExpect(result).to.be.false
  })

  it('is an ancestor', () => {
    const result = TP.isAncestorOf(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X', 'Y']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
    )
    chaiExpect(result).to.be.true
  })

  it('the two paths are the same', () => {
    const result = TP.isAncestorOf(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
    )
    chaiExpect(result).to.be.true
  })

  it('does not match same paths with flag set to false', () => {
    const result = TP.isAncestorOf(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
      false,
    )
    chaiExpect(result).to.be.false
  })

  it('target ancestor is children of path', () => {
    const result = TP.isAncestorOf(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X', 'Y']),
    )
    chaiExpect(result).to.be.false
  })
})

describe('replaceIfAncestor', () => {
  it('where the path does not match', () => {
    const result = TP.replaceIfAncestor(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['X']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['A']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['B']),
    )
    chaiExpect(result).to.be.null
  })
  it('where the path matches exactly', () => {
    const result = TP.replaceIfAncestor(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['A', 'B']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['A', 'B']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['C', 'D']),
    )
    expect(result).toEqual(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['C', 'D']),
    )
  })
  it('where the path is an ancestor', () => {
    const result = TP.replaceIfAncestor(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['A', 'B', 'X']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['A', 'B']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['C', 'D']),
    )
    chaiExpect(result).to.deep.equal(
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['C', 'D', 'X']),
    )
  })
})

describe('fromString', () => {
  it('parses a simple path correctly', () => {
    const expectedResult = TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), [
      'A',
      'B',
      'C',
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
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['a', 'b']),
    ])
    const expectedResult = TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), [
      'a',
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('for two elements returns the common parent', () => {
    const actualResult = TP.getCommonParent([
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['a', 'b', 'c']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['a', 'b', 'd']),
    ])
    const expectedResult = TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), [
      'a',
      'b',
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('for three elements without a common parent returns null', () => {
    const actualResult = TP.getCommonParent([
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['a', 'b', 'c']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-bbb']]), ['a', 'b', 'd']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-ccc']]), ['x', 'b', 'd']),
    ])
    expect(actualResult).toBeNull()
  })
  it('returns one of the passed paths if it is a common parent of the rest and includeSelf is set to true', () => {
    const actualResult = TP.getCommonParent(
      [
        TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['a', 'b']),
        TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['a', 'b', 'd']),
      ],
      true,
    )
    const expectedResult = TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), [
      'a',
      'b',
    ])
    expect(actualResult).toEqual(expectedResult)
  })
  it('does not return one of the passed paths if it is a common parent of the rest and includeSelf is not set', () => {
    const actualResult = TP.getCommonParent([
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['a', 'b']),
      TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['a', 'b', 'd']),
    ])
    const expectedResult = TP.instancePath(TP.scenePath([[BakedInStoryboardUID, 'scene-aaa']]), [
      'a',
    ])
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
