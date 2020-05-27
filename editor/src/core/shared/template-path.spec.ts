import * as Chai from 'chai'
import * as TP from './template-path'
import { BakedInStoryboardUID } from '../model/scene-utils'
const chaiExpect = Chai.expect

describe('serialization', function() {
  it('path survives serialization', function() {
    const path = TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['VIEW1', 'VIEW2'])
    const pathString = TP.toComponentId(path)
    const restoredPath = TP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })

  it('empty path survives serialization', function() {
    const path = TP.instancePath([], [])
    const pathString = TP.toComponentId(path)
    const restoredPath = TP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })

  it('ScenePath survives serialization', function() {
    const path = TP.scenePath([BakedInStoryboardUID, 'scene-aaa'])
    const pathString = TP.toString(path)
    const restoredPath = TP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })

  it('Empty ScenePath survives serialization', function() {
    const path = TP.scenePath([])
    const pathString = TP.toString(path)
    const restoredPath = TP.fromString(pathString)
    chaiExpect(restoredPath).to.deep.equal(path)
  })
})

describe('isAncestorOf', () => {
  it('is not an ancestor', () => {
    const result = TP.isAncestorOf(
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['X']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['Y']),
    )
    chaiExpect(result).to.be.false
  })

  it('is an ancestor', () => {
    const result = TP.isAncestorOf(
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['X', 'Y']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['X']),
    )
    chaiExpect(result).to.be.true
  })

  it('the two paths are the same', () => {
    const result = TP.isAncestorOf(
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['X']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['X']),
    )
    chaiExpect(result).to.be.true
  })

  it('does not match same paths with flag set to false', () => {
    const result = TP.isAncestorOf(
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['X']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['X']),
      false,
    )
    chaiExpect(result).to.be.false
  })

  it('target ancestor is children of path', () => {
    const result = TP.isAncestorOf(
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['X']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['X', 'Y']),
    )
    chaiExpect(result).to.be.false
  })
})

describe('replaceIfAncestor', () => {
  it('where the path does not match', () => {
    const result = TP.replaceIfAncestor(
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['X']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['A']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['B']),
    )
    chaiExpect(result).to.be.null
  })
  it('where the path matches exactly', () => {
    const result = TP.replaceIfAncestor(
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['A', 'B']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['A', 'B']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['C', 'D']),
    )
    expect(result).toEqual(TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['C', 'D']))
  })
  it('where the path is an ancestor', () => {
    const result = TP.replaceIfAncestor(
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['A', 'B', 'X']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['A', 'B']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['C', 'D']),
    )
    chaiExpect(result).to.deep.equal(
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['C', 'D', 'X']),
    )
  })
})

describe('fromString', () => {
  it('parses a simple path correctly', () => {
    const expectedResult = TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['A', 'B', 'C'])
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
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a', 'b']),
    ])
    const expectedResult = TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a'])
    expect(actualResult).toEqual(expectedResult)
  })
  it('for two elements returns the common parent', () => {
    const actualResult = TP.getCommonParent([
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a', 'b', 'c']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a', 'b', 'd']),
    ])
    const expectedResult = TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a', 'b'])
    expect(actualResult).toEqual(expectedResult)
  })
  it('for three elements without a common parent returns null', () => {
    const actualResult = TP.getCommonParent([
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a', 'b', 'c']),
      TP.instancePath([BakedInStoryboardUID, 'scene-bbb'], ['a', 'b', 'd']),
      TP.instancePath([BakedInStoryboardUID, 'scene-ccc'], ['x', 'b', 'd']),
    ])
    expect(actualResult).toBeNull()
  })
  it('returns one of the passed paths if it is a common parent of the rest and includeSelf is set to true', () => {
    const actualResult = TP.getCommonParent(
      [
        TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a', 'b']),
        TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a', 'b', 'd']),
      ],
      true,
    )
    const expectedResult = TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a', 'b'])
    expect(actualResult).toEqual(expectedResult)
  })
  it('does not return one of the passed paths if it is a common parent of the rest and includeSelf is not set', () => {
    const actualResult = TP.getCommonParent([
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a', 'b']),
      TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a', 'b', 'd']),
    ])
    const expectedResult = TP.instancePath([BakedInStoryboardUID, 'scene-aaa'], ['a'])
    expect(actualResult).toEqual(expectedResult)
  })
})
