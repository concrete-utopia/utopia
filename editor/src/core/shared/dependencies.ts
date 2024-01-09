import { createSelector } from 'reselect'
import { packageJsonFileFromProjectContents } from '../../components/assets'
import type {
  SetPackageStatus,
  SetRefreshingDependencies,
  UpdateNodeModulesContents,
  UpdateNodeModulesContentsAndSetPackageStatus,
} from '../../components/editor/action-types'
import {
  setPackageStatus,
  setRefreshingDependencies,
  updateNodeModulesContents,
  updateNodeModulesContentsAndSetPackageStatus,
} from '../../components/editor/actions/action-creators'
import {
  createLoadedPackageStatusMapFromDependencies,
  dependenciesFromPackageJsonContents,
} from '../../components/editor/npm-dependency/npm-dependency'
import type { EditorStorePatched } from '../../components/editor/store/editor-state'
import type { BuiltInDependencies } from '../es-modules/package-manager/built-in-dependencies-list'
import { fetchNodeModules } from '../es-modules/package-manager/fetch-packages'
import type { RequestedNpmDependency } from './npm-dependency-types'
import { objectFilter, objectMap } from './object-utils'
import type { NodeModules } from './project-file-types'
import { isTextFile } from './project-file-types'
import { fastForEach } from './utils'

export function removeModulesFromNodeModules(
  modulesToRemove: Array<string>,
  nodeModules: NodeModules,
): NodeModules {
  const filePathsToRemove = modulesToRemove.map((m) => `/node_modules/${m}/`)

  return objectFilter(
    (_module, modulePath) =>
      !filePathsToRemove.some((pathToRemove) => (modulePath as string).startsWith(pathToRemove)),
    nodeModules,
  )
}

export async function refreshDependencies(
  onUpdate: (
    updates: SetRefreshingDependencies | UpdateNodeModulesContentsAndSetPackageStatus,
  ) => void,
  packageJsonContents: string,
  currentDeps: RequestedNpmDependency[] | null,
  builtInDependencies: BuiltInDependencies,
  nodeModules: NodeModules,
): Promise<NodeModules> {
  async function doRefresh() {
    const deps = dependenciesFromPackageJsonContents(packageJsonContents)
    let newDeps: RequestedNpmDependency[] = []
    let updatedDeps: RequestedNpmDependency[] = []
    let removedDeps: RequestedNpmDependency[] = []
    if (currentDeps != null) {
      let foundMatchingDeps: RequestedNpmDependency[] = []

      fastForEach(deps, (dep) => {
        const matchingCurrentDep = currentDeps.find((currentDep) => dep.name === currentDep.name)

        // Find the new or updated dependencies
        if (matchingCurrentDep == null) {
          // A new dependency has been added
          newDeps.push(dep)
        } else {
          foundMatchingDeps.push(matchingCurrentDep)

          if (matchingCurrentDep.version !== dep.version) {
            // An updated dependency
            updatedDeps.push(dep)
          }
        }

        // Find the deleted dependencies
        removedDeps = currentDeps.filter((currentDep) => !foundMatchingDeps.includes(currentDep))
      })
    } else {
      newDeps = deps
    }

    const modulesToRemove = updatedDeps.concat(removedDeps).map((d) => d.name)

    const updatedNodeModulesFiles = removeModulesFromNodeModules(modulesToRemove, nodeModules)

    const depsToFetch = newDeps.concat(updatedDeps)

    const fetchNodeModulesResult = await fetchNodeModules(depsToFetch, builtInDependencies)

    const loadedPackagesStatus = createLoadedPackageStatusMapFromDependencies(
      deps,
      fetchNodeModulesResult.dependenciesWithError,
      fetchNodeModulesResult.dependenciesNotFound,
    )
    onUpdate(
      updateNodeModulesContentsAndSetPackageStatus(
        fetchNodeModulesResult.nodeModules,
        loadedPackagesStatus,
      ),
    )

    return updatedNodeModulesFiles
  }

  onUpdate(setRefreshingDependencies(true))
  return doRefresh().finally(() => {
    onUpdate(setRefreshingDependencies(false))
  })
}

export const projectDependenciesSelector = createSelector(
  (store: EditorStorePatched) => store.editor.projectContents,
  (projectContents): Array<RequestedNpmDependency> => {
    const currentDepsFile = packageJsonFileFromProjectContents(projectContents)
    if (currentDepsFile != null && isTextFile(currentDepsFile)) {
      return dependenciesFromPackageJsonContents(currentDepsFile.fileContents.code)
    }
    return []
  },
)
