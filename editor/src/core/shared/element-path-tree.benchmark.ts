import * as Benny from 'benny'
import { buildTree, getSubTree } from './element-path-tree'
import type { ElementPath } from './project-file-types'
import * as EP from './element-path'
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

export async function benchmarkBuildTree(): Promise<void> {
  await Benny.suite(
    'isDescendantOf - for immediate descendant',
    Benny.add('for immediate descendant', () => {
      const path1 = EP.fromString(`aaa/bbb/ccc`)
      const path2 = EP.fromString(`aaa/bbb/ccc:ddd`)

      return () => {
        EP.isDescendantOf(path2, path1)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'buildTree immediate descendant', details: true }),
  )
  await Benny.suite(
    'isDescendantOf - non-descendant',
    Benny.add('non-descendant', () => {
      const path1 = EP.fromString(`aaa/bbb/ccc`)
      const path2 = EP.fromString(`aaa/bbb/ddd`)

      return () => {
        EP.isDescendantOf(path2, path1)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'buildTree non-descendant', details: true }),
  )
  await Benny.suite(
    'isChildOf - child',
    Benny.add('child', () => {
      const path1 = EP.fromString(`aaa/bbb/ccc`)
      const path2 = EP.fromString(`aaa/bbb/ccc:ddd`)

      return () => {
        EP.isChildOf(path2, path1)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'isChildOf child', details: true }),
  )
  await Benny.suite(
    'isChildOf - not child',
    Benny.add('not child', () => {
      const path1 = EP.fromString(`aaa/bbb/ccc`)
      const path2 = EP.fromString(`aaa/bbb/ddd`)

      return () => {
        EP.isChildOf(path2, path1)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'isChildOf not child', details: true }),
  )
  await Benny.suite(
    'buildTree - deeply nested elements',
    Benny.add('deeply nested elements', () => {
      let workingPath: ElementPath = EP.elementPath([['root']])
      let elementPaths: Array<ElementPath> = [workingPath]
      for (let stepCount = 1; stepCount < 100; stepCount++) {
        workingPath = EP.appendToPath(workingPath, `step${stepCount}`)
        elementPaths.push(workingPath)
      }

      return () => {
        buildTree(dummyMetadataFromPaths(elementPaths))
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'buildTree deeply nested', details: true }),
  )
  await Benny.suite(
    'buildTree - very wide elements',
    Benny.add('very wide elements', () => {
      const rootPath: ElementPath = EP.elementPath([['root']])
      let elementPaths: Array<ElementPath> = []
      for (let stepCount = 1; stepCount < 100; stepCount++) {
        const newPath = EP.appendToPath(rootPath, `step${stepCount}`)
        elementPaths.push(newPath)
      }

      return () => {
        buildTree(dummyMetadataFromPaths(elementPaths))
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'buildTree very wide', details: true }),
  )
  await Benny.suite(
    'getSubTree - deeply nested elements',
    Benny.add('deeply nested elements', () => {
      let workingPath: ElementPath = EP.elementPath([['root']])
      let elementPaths: Array<ElementPath> = [workingPath]
      let halfwayThroughPath: ElementPath
      for (let stepCount = 1; stepCount < 100; stepCount++) {
        workingPath = EP.appendToPath(workingPath, `step${stepCount}`)
        elementPaths.push(workingPath)
        if (stepCount === 50) {
          halfwayThroughPath = workingPath
        }
      }
      const tree = buildTree(dummyMetadataFromPaths(elementPaths))

      return () => {
        getSubTree(tree, halfwayThroughPath)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'getSubTree deeply nested', details: true }),
  )
  await Benny.suite(
    'getSubTree - very wide elements',
    Benny.add('very wide elements', () => {
      const rootPath: ElementPath = EP.elementPath([['root']])
      let elementPaths: Array<ElementPath> = []
      let halfwayThroughPath: ElementPath
      for (let stepCount = 1; stepCount < 100; stepCount++) {
        const newPath = EP.appendToPath(rootPath, `step${stepCount}`)
        elementPaths.push(newPath)
        if (stepCount === 50) {
          halfwayThroughPath = newPath
        }
      }
      const tree = buildTree(dummyMetadataFromPaths(elementPaths))

      return () => {
        getSubTree(tree, halfwayThroughPath)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'getSubTree very wide', details: true }),
  )
}
