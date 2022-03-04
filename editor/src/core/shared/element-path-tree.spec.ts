import { buildTree, forEachChildOfTarget, printTree } from './element-path-tree'
import * as EP from './element-path'
import { ElementPath } from './project-file-types'

describe('buildTree', () => {
  it('should build a simple tree', () => {
    const actualResult = buildTree([
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
    ])
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
    const tree = buildTree([
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
    ])
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
