import { PropertyControls } from 'utopia-api'
import { ProjectContentTreeRoot } from '../../components/assets'
import {
  getExportValuesFromAllModules,
  incorporateBuildResult,
  processExportsInfo,
  PropertyControlsInfo,
} from '../../components/custom-code/code-file'
import { getRequireFn } from '../es-modules/package-manager/package-manager'
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
    bundledProjectFiles: MultiFileBuildResult,
    exportsInfo: ReadonlyArray<ExportsInfo>,
  ) => {
    currentNodeModules = applyNodeModulesUpdate(currentNodeModules, nodeModulesUpdate)
    const resolvedNpmDependencies = resolvedDependencyVersions(npmDependencies, currentNodeModules)

    incorporateBuildResult(currentNodeModules, bundledProjectFiles)

    let propertyControlsInfo: PropertyControlsInfo = getControlsForExternalDependencies(
      resolvedNpmDependencies,
    )

    processPropertyControlsWithBuildResult(
      propertyControlsInfo,
      projectContents,
      bundledProjectFiles,
      exportsInfo,
    )
  }

  const processPropertyControlsWithBuildResult = async (
    propertyControlsInfo: PropertyControlsInfo,
    projectContents: ProjectContentTreeRoot,
    bundledProjectFiles: MultiFileBuildResult,
    exportsInfo: ReadonlyArray<ExportsInfo>,
  ) => {
    const onRemoteModuleDownload = (moduleDownload: Promise<NodeModules>) => {
      moduleDownload.then((downloadedModules: NodeModules) => {
        // MUTATION
        Object.assign(currentNodeModules, downloadedModules)
        processPropertyControlsWithBuildResult(
          propertyControlsInfo,
          projectContents,
          bundledProjectFiles,
          exportsInfo,
        )
      })
    }

    const requireFn = getRequireFn(onRemoteModuleDownload, projectContents, currentNodeModules)

    const exportValues = getExportValuesFromAllModules(bundledProjectFiles, requireFn)
    fastForEach(exportsInfo, (result) => {
      const codeResult = processExportsInfo(exportValues[result.filename], result.exportTypes)
      let propertyControls: { [name: string]: PropertyControls } = {}
      if (codeResult.exports != null) {
        fastForEach(Object.keys(codeResult.exports), (name) => {
          const exportedObject = codeResult.exports[name].value
          if (exportedObject != null && exportedObject.propertyControls != null) {
            // FIXME validate shape
            propertyControls[name] = exportedObject.propertyControls
          }
        })
        const filenameNoExtension = result.filename.replace(/\.(js|jsx|ts|tsx)$/, '')
        propertyControlsInfo[filenameNoExtension] = propertyControls
      }
    })

    onControlsProcessed(propertyControlsInfo)
  }

  return processPropertyControls
}
