import { buildTree, forEachChildOfTarget, printTree } from './element-path-tree'
import * as EP from './element-path'
import type { ElementPath } from './project-file-types'
import { left } from './either'
import type { ElementInstanceMetadataMap, ElementInstanceMetadata } from './element-template'

function dummyMetadataFromPaths(elementPaths: ElementPath[]): ElementInstanceMetadataMap {
  const metadata: ElementInstanceMetadataMap = {}
  for (const path of elementPaths) {
    metadata[EP.toString(path)] = {
      elementPath: path,
      element: left('dummy'),
    } as ElementInstanceMetadata
  }
  return metadata
}

describe('buildTree', () => {
  it('should build a simple tree', () => {
    const actualResult = buildTree(
      dummyMetadataFromPaths([
        EP.elementPath([['aaa']]),
        EP.elementPath([['aaa', 'bbb']]),
        EP.elementPath([['aaa', 'bbb'], ['ccc']]),
        EP.elementPath([['aaa', 'bbb'], ['ddd']]),
        EP.elementPath([
          ['aaa', 'bbb'],
          ['ccc', 'ccc1'],
        ]),
        EP.elementPath([
          ['aaa', 'bbb'],
          ['ccc', 'ccc2'],
        ]),
      ]),
    )
    expect(printTree(actualResult)).toMatchInlineSnapshot(`
      "aaa
        aaa/bbb
          aaa/bbb:ccc
            aaa/bbb:ccc/ccc1
            aaa/bbb:ccc/ccc2
          aaa/bbb:ddd
      "
    `)
  })
  it('omits subtrees with missing parents', () => {
    const actualResult = buildTree(
      dummyMetadataFromPaths([
        EP.elementPath([['aaa']]),
        EP.elementPath([['aaa', 'bbb']]),
        EP.elementPath([['aaa', 'bbb'], ['ccc']]),
        EP.elementPath([['aaa', 'bbb'], ['ddd']]),
        EP.elementPath([
          ['aaa', 'bbb'],
          ['ccc', 'ccc1'],
        ]),
        EP.elementPath([
          ['aaa', 'bbb'],
          ['ccc', 'ccc2'],
        ]),
        EP.elementPath([
          // this has no parent
          ['aaa', 'foo'],
          ['ccc', 'ccc2'],
        ]),
        EP.elementPath([
          // this has no parent
          ['aaa', 'bbb', 'bar'],
          ['ccc', 'ccc2'],
        ]),
      ]),
    )
    expect(printTree(actualResult)).toMatchInlineSnapshot(`
      "aaa
        aaa/bbb
          aaa/bbb:ccc
            aaa/bbb:ccc/ccc1
            aaa/bbb:ccc/ccc2
          aaa/bbb:ddd
      "
    `)
  })
})

describe('forEachChildOfTarget', () => {
  it('should run for just the children of the targeted element', () => {
    const tree = buildTree(
      dummyMetadataFromPaths([
        EP.elementPath([['aaa']]),
        EP.elementPath([['aaa', 'bbb']]),
        EP.elementPath([['aaa', 'bbb'], ['ccc']]),
        EP.elementPath([['aaa', 'bbb'], ['ddd']]),
        EP.elementPath([
          ['aaa', 'bbb'],
          ['ccc', 'ccc1'],
        ]),
        EP.elementPath([
          ['aaa', 'bbb'],
          ['ccc', 'ccc2'],
        ]),
      ]),
    )
    let actualResult: Array<ElementPath> = []
    forEachChildOfTarget(tree, EP.elementPath([['aaa', 'bbb'], ['ccc']]), (elem) => {
      actualResult.push(elem)
    })
    expect(actualResult).toEqual([
      EP.elementPath([
        ['aaa', 'bbb'],
        ['ccc', 'ccc1'],
      ]),
      EP.elementPath([
        ['aaa', 'bbb'],
        ['ccc', 'ccc2'],
      ]),
    ])
  })
})
