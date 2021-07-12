import { PropertyControls } from 'utopia-api'
import { ProjectContentTreeRoot } from '../../components/assets'
import {
  getExportValuesFromAllModules,
  incorporateBuildResult,
  processExportsInfo,
  PropertyControlsInfo,
} from '../../components/custom-code/code-file'
import { EvaluationCache, getRequireFn } from '../es-modules/package-manager/package-manager'
import {
  applyNodeModulesUpdate,
  getControlsForExternalDependencies,
  NodeModulesUpdate,
} from '../property-controls/property-controls-utils'
import { RequestedNpmDependency } from '../shared/npm-dependency-types'
import { NodeModules } from '../shared/project-file-types'
import { fastForEach } from '../shared/utils'
import { resolvedDependencyVersions } from '../third-party/third-party-components'
import { ExportsInfo, MultiFileBuildResult } from '../workers/ts/ts-worker'

export const initPropertyControlsProcessor = (
  onControlsProcessed: (propertyControlsInfo: PropertyControlsInfo) => void,
) => {
  let currentNodeModules: NodeModules = {}

  const processPropertyControls = async (
    npmDependencies: RequestedNpmDependency[],
    nodeModulesUpdate: NodeModulesUpdate,
    projectContents: ProjectContentTreeRoot,
    evaluationCache: EvaluationCache,
    bundledProjectFiles: MultiFileBuildResult,
  ) => {
    currentNodeModules = applyNodeModulesUpdate(currentNodeModules, nodeModulesUpdate)
    const resolvedNpmDependencies = resolvedDependencyVersions(npmDependencies, currentNodeModules)

    incorporateBuildResult(currentNodeModules, projectContents, bundledProjectFiles)

    let propertyControlsInfo: PropertyControlsInfo = getControlsForExternalDependencies(
      resolvedNpmDependencies,
    )

    processPropertyControlsWithBuildResult(
      propertyControlsInfo,
      projectContents,
      evaluationCache,
      bundledProjectFiles,
    )
  }

  const processPropertyControlsWithBuildResult = async (
    propertyControlsInfo: PropertyControlsInfo,
    projectContents: ProjectContentTreeRoot,
    evaluationCache: EvaluationCache,
    bundledProjectFiles: MultiFileBuildResult,
  ) => {
    const onRemoteModuleDownload = (moduleDownload: Promise<NodeModules>) => {
      moduleDownload.then((downloadedModules: NodeModules) => {
        // MUTATION
        Object.assign(currentNodeModules, downloadedModules)
        processPropertyControlsWithBuildResult(
          propertyControlsInfo,
          projectContents,
          evaluationCache,
          bundledProjectFiles,
        )
      })
    }

    const requireFn = getRequireFn(
      onRemoteModuleDownload,
      projectContents,
      currentNodeModules,
      evaluationCache,
      'canvas',
    )

    const exportValues = getExportValuesFromAllModules(bundledProjectFiles, requireFn)
    for (const fileName in exportValues) {
      const filenameNoExtension = fileName.replace(/\.(js|jsx|ts|tsx)$/, '')
      if (propertyControlsInfo[filenameNoExtension] == null) {
        propertyControlsInfo[filenameNoExtension] = {}
      }
      const exportsForFile = exportValues[fileName]
      for (const exportedVariableName in exportsForFile) {
        const exportedObject = exportsForFile[exportedVariableName]
        if (exportedObject.propertyControls != null) {
          // FIXME validate shape
          propertyControlsInfo[filenameNoExtension][exportedVariableName] =
            exportedObject.propertyControls
        }
      }
    }

    onControlsProcessed(propertyControlsInfo)
  }

  return processPropertyControls
}
