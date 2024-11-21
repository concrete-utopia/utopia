import { createSelector } from 'reselect'
import { packageJsonFileFromProjectContents } from '../../components/assets'
import type { EditorDispatch } from '../../components/editor/action-types'
import {
  setPackageStatus,
  setRefreshingDependencies,
  updateNodeModulesContents,
} from '../../components/editor/actions/action-creators'
import {
  createLoadedPackageStatusMapFromDependencies,
  dependenciesFromPackageJsonContents,
} from '../../components/editor/npm-dependency/npm-dependency'
import type { EditorStorePatched } from '../../components/editor/store/editor-state'
import type { BuiltInDependencies } from '../es-modules/package-manager/built-in-dependencies-list'
import { fetchNodeModules } from '../es-modules/package-manager/fetch-packages'
import type {
  PackageDetails,
  PackageStatusMap,
  RequestedNpmDependency,
} from './npm-dependency-types'
import { objectFilter } from './object-utils'
import type { NodeModules } from './project-file-types'
import { isTextFile } from './project-file-types'
import { fastForEach } from './utils'
import { notifyOperationFinished } from './import/import-operation-service'
import { ImportOperationResult } from './import/import-operation-types'

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
  dispatch: EditorDispatch,
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

    const fetchNodeModulesResult = await fetchNodeModules(
      dispatch,
      depsToFetch,
      builtInDependencies,
    )

    const loadedPackagesStatus = createLoadedPackageStatusMapFromDependencies(
      deps,
      fetchNodeModulesResult.dependenciesWithError,
      fetchNodeModulesResult.dependenciesNotFound,
    )
    const packageErrorActions = Object.keys(loadedPackagesStatus).map((dependencyName) =>
      setPackageStatus(dependencyName, loadedPackagesStatus[dependencyName].status),
    )
    dispatch([
      ...packageErrorActions,
      updateNodeModulesContents(fetchNodeModulesResult.nodeModules),
    ])

    notifyOperationFinished(
      dispatch,
      { type: 'refreshDependencies' },
      getDependenciesStatus(loadedPackagesStatus),
    )

    return updatedNodeModulesFiles
  }

  dispatch([setRefreshingDependencies(true)], 'everyone')
  return doRefresh().finally(() => {
    dispatch([setRefreshingDependencies(false)], 'everyone')
  })
}

function isPackageMissing(status: PackageDetails): boolean {
  return status.status === 'error' || status.status === 'not-found'
}

export function getDependenciesStatus(
  loadedPackagesStatus: PackageStatusMap,
): ImportOperationResult {
  if (Object.values(loadedPackagesStatus).every(isPackageMissing)) {
    return ImportOperationResult.Error
  }
  if (Object.values(loadedPackagesStatus).some(isPackageMissing)) {
    return ImportOperationResult.Warn
  }
  return ImportOperationResult.Success
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
