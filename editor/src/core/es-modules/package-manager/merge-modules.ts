import { PackagerServerFileDescriptor } from '../../shared/npm-dependency-types'
import type { NodeModules } from '../../shared/project-file-types'

function movePathInsidePackage(mainPackage: string, filePath: string): string {
  return `/node_modules/${mainPackage}${filePath}`
}

export function pathForMainPackage(mainPackage: string): string {
  return `/node_modules/${mainPackage}/`
}

export function mangleNodeModulePaths(
  mainPackage: string,
  nodeModulesContents: NodeModules,
): NodeModules {
  const mainPackagePath = pathForMainPackage(mainPackage)
  return Object.keys(nodeModulesContents).reduce((newContents, packagePath: string) => {
    if (packagePath.startsWith(`/node_modules/`)) {
      if (packagePath.startsWith(mainPackagePath)) {
        // we leave these in place
        newContents[packagePath] = nodeModulesContents[packagePath]
      } else {
        // this package is not the main package, we move this under
        // /node_modules/<mainpackage>/node_modules
        // to avoid clashing transitive dependencies
        // TODO implement the npm module merging logic
        const mangledPath = movePathInsidePackage(mainPackage, packagePath)
        newContents[mangledPath] = nodeModulesContents[packagePath]
      }
    } else {
      // this is a file outside /node_modules/, leave it as is
      newContents[packagePath] = nodeModulesContents[packagePath]
    }
    return newContents
  }, {} as NodeModules)
}

export function mergeNodeModules(nodeModulesArr: Array<NodeModules>): NodeModules {
  // TODO: we need a more sophisticated merge
  return nodeModulesArr.reduce((acc, curr) => {
    return Object.assign(acc, curr)
  }, {} as NodeModules)
}
