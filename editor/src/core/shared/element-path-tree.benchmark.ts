import * as Benny from 'benny'
import { buildTree } from './element-path-tree'
import { ElementPath } from './project-file-types'
import * as EP from './element-path'

export function benchmarkBuildTree(): void {
  Benny.suite(
    'buildTree - deeply nested elements',
    Benny.add('deeply nested elements', () => {
      let workingPath: ElementPath = EP.elementPath([['root']])
      let elementPaths: Array<ElementPath> = [workingPath]
      for (let stepCount = 1; stepCount < 100; stepCount++) {
        workingPath = EP.appendToPath(workingPath, `step${stepCount}`)
        elementPaths.push(workingPath)
      }

      return () => {
        buildTree(elementPaths)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'buildTree deeply nested', details: true }),
  )
  Benny.suite(
    'buildTree - very wide elements',
    Benny.add('very wide elements', () => {
      const rootPath: ElementPath = EP.elementPath([['root']])
      let elementPaths: Array<ElementPath> = []
      for (let stepCount = 1; stepCount < 100; stepCount++) {
        const newPath = EP.appendToPath(rootPath, `step${stepCount}`)
        elementPaths.push(newPath)
      }

      return () => {
        buildTree(elementPaths)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'buildTree very wide', details: true }),
  )
}
