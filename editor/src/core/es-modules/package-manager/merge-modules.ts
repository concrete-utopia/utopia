import { PackagerServerFileDescriptor } from '../../shared/npm-dependency-types'
import { NodeModules } from '../../shared/project-file-types'

function movePathInsidePackage(mainPackage: string, filePath: string): string {
  return `/node_modules/${mainPackage}${filePath}`
}

export function mangleNodeModulePaths(
  mainPackage: string,
  nodeModulesContents: NodeModules,
): NodeModules {
  const mainPackagePath = `/node_modules/${mainPackage}/`
  return Object.keys(nodeModulesContents).reduce((newContents, packagePath: string) => {
    if (packagePath.startsWith(`/node_modules/`)) {
      if (packagePath.startsWith(mainPackagePath)) {
        // we leave these in place
        return {
          ...newContents,
          [packagePath]: nodeModulesContents[packagePath],
        }
      } else {
        // this package is not the main package, we move this under
        // /node_modules/<mainpackage>/node_modules
        // to avoid clashing transitive dependencies
        // TODO implement the npm module merging logic
        const mangledPath = movePathInsidePackage(mainPackage, packagePath)
        return {
          ...newContents,
          [mangledPath]: nodeModulesContents[packagePath],
        }
      }
    } else {
      // this is a file outside /node_modules/, leave it as is
      return {
        ...newContents,
        [packagePath]: nodeModulesContents[packagePath],
      }
    }
  }, {} as NodeModules)
}

export function mergeNodeModules(nodeModulesArr: Array<NodeModules>): NodeModules {
  // TODO: we need a more sophisticated merge
  return nodeModulesArr.reduce((acc, curr) => {
    return {
      ...acc,
      ...curr,
    }
  }, {} as NodeModules)
}
