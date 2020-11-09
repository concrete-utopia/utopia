import * as React from 'react'
import * as ReactDOM from 'react-dom'
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
  CanvasRelatedProps,
  collectCanvasProps,
  getControlsForExternalDependencies,
  NodeModulesUpdate,
} from './property-controls-utils'
import { RequestedNpmDependency } from '../shared/npm-dependency-types'
import { NodeModules } from '../shared/project-file-types'
import { fastForEach, NO_OP } from '../shared/utils'
import { resolvedDependencyVersions } from '../third-party/third-party-components'
import { ExportsInfo, MultiFileBuildResult } from '../workers/ts/ts-worker'
import { UiJsxCanvas } from '../../components/canvas/ui-jsx-canvas'

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
    canvasRelatedProps: CanvasRelatedProps | null,
  ) => {
    // console.log('processPropertyControls triggerd')
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
      canvasRelatedProps,
    )
  }

  const processPropertyControlsWithBuildResult = async (
    propertyControlsInfo: PropertyControlsInfo,
    projectContents: ProjectContentTreeRoot,
    bundledProjectFiles: MultiFileBuildResult,
    exportsInfo: ReadonlyArray<ExportsInfo>,
    canvasRelatedProps: CanvasRelatedProps | null,
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
          canvasRelatedProps,
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

    const canvasProps = collectCanvasProps(false, canvasRelatedProps, projectContents, requireFn)

    // console.log('rendering fresh canvas')
    if (canvasProps != null) {
      ReactDOM.render(
        <UiJsxCanvas {...canvasProps} clearErrors={NO_OP} />,
        document.getElementById('canvas-root')!,
      )
    }
  }

  return processPropertyControls
}
