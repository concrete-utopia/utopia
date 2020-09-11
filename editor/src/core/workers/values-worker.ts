import { PropertyControls } from 'utopia-api'
import {
  getExportValuesFromAllModules,
  processExportsInfo,
  PropertyControlsInfo,
} from '../../components/custom-code/code-file'
import { getRequireFn } from '../es-modules/package-manager/package-manager'
import { getControlsForExternalDependencies } from '../property-controls/property-controls-utils'
import { NpmDependency } from '../shared/npm-dependency-types'
import { NodeModules } from '../shared/project-file-types'
import { fastForEach } from '../shared/utils'
import { ExportsInfo, MultiFileBuildResult } from './ts/ts-worker'

export interface GetPropertyControlsInfoMessage {
  type: 'getpropertycontrolsinfo'
  nodeModules: NodeModules
  exportsInfo: ReadonlyArray<ExportsInfo>
  projectModules: MultiFileBuildResult
  npmDependencies: ReadonlyArray<NpmDependency>
}

export function createGetPropertyControlsInfoMessage(
  nodeModules: NodeModules,
  exportsInfo: ReadonlyArray<ExportsInfo>,
  projectModules: MultiFileBuildResult,
  npmDependencies: ReadonlyArray<NpmDependency>,
): GetPropertyControlsInfoMessage {
  return {
    type: 'getpropertycontrolsinfo',
    nodeModules: nodeModules,
    exportsInfo: exportsInfo,
    projectModules: projectModules,
    npmDependencies: npmDependencies,
  }
}

export type IncomingValuesWorkerMessage = GetPropertyControlsInfoMessage

export interface GetPropertyControlsInfoResult {
  type: 'getpropertycontrolsinforesult'
  propertyControlsInfo: PropertyControlsInfo
}

export function createGetPropertyControlsInfoResult(
  propertyControlsInfo: PropertyControlsInfo,
): GetPropertyControlsInfoResult {
  return {
    type: 'getpropertycontrolsinforesult',
    propertyControlsInfo: propertyControlsInfo,
  }
}

export interface GetPropertyControlsInfoFailure {
  type: 'getpropertycontrolsinfofailure'
}

export function createGetPropertyControlsInfoFailure(): GetPropertyControlsInfoFailure {
  return {
    type: 'getpropertycontrolsinfofailure',
  }
}

export type OutgoingValuesWorkerMessage =
  | GetPropertyControlsInfoResult
  | GetPropertyControlsInfoFailure

export function handleMessage(
  workerMessage: IncomingValuesWorkerMessage,
  sendMessage: (content: OutgoingValuesWorkerMessage) => void,
): void {
  switch (workerMessage.type) {
    case 'getpropertycontrolsinfo': {
      try {
        getPropertyControlsInfo(
          workerMessage.nodeModules,
          workerMessage.exportsInfo,
          workerMessage.projectModules,
          workerMessage.npmDependencies,
          sendMessage,
        )
      } catch (e) {
        sendMessage(createGetPropertyControlsInfoFailure())
        throw e
      }
      break
    }
  }
}

function getPropertyControlsInfo(
  nodeModules: NodeModules,
  exportsInfo: ReadonlyArray<ExportsInfo>,
  projectModules: MultiFileBuildResult,
  npmDependencies: ReadonlyArray<NpmDependency>,
  sendMessage: (content: OutgoingValuesWorkerMessage) => void,
): void {
  let workingNodeModules: NodeModules = {
    ...nodeModules,
  }
  const requireFn = getRequireFn((modulesToAdd: NodeModules) => {
    fastForEach(Object.keys(modulesToAdd), (moduleKey) => {
      workingNodeModules[moduleKey] = modulesToAdd[moduleKey]
    })
  }, workingNodeModules)

  const exportValues = getExportValuesFromAllModules(projectModules, requireFn)
  let propertyControlsInfo: PropertyControlsInfo = getControlsForExternalDependencies(
    npmDependencies,
  )
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

  sendMessage(createGetPropertyControlsInfoResult(propertyControlsInfo))
}
