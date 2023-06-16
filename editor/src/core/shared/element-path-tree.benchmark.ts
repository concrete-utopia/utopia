import * as Benny from 'benny'
import { buildTree, getSubTree } from './element-path-tree'
import { ElementPath } from './project-file-types'
import * as EP from './element-path'
import { dummyMetadataFromPaths } from '../../utils/utils.test-utils'

export async function benchmarkBuildTree(): Promise<void> {
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
