import type {
  ComponentDescriptorsForFile,
  ComponentDescriptorWithName,
  PropertyControlsInfo,
} from '../custom-code/code-file'
import deepEqual from 'fast-deep-equal'
import type { EditorDispatch } from '../editor/action-types'
import { updatePropertyControlsInfo } from '../editor/actions/action-creators'
import { emptySet } from '../../core/shared/set-utils'
import type { Either } from '../../core/shared/either'
import { forEachRight } from '../../core/shared/either'
import { mapArrayToDictionary } from '../../core/shared/array-utils'
import { fastForEach } from '../../core/shared/utils'

export type ControlsToCheck = Promise<Either<string, Array<ComponentDescriptorWithName>>>

export interface PropertyControlsInfoToCheck {
  moduleNameOrPath: string
  newDescriptorsForFile: ControlsToCheck
}

let previousRegisteredModules: Set<string> = emptySet()
let allControlsRegisteredByFileEver: Map<string, Array<PropertyControlsInfoToCheck>> = new Map()
let controlsRegisteredByFileInLastRender: Map<
  string,
  Array<PropertyControlsInfoToCheck>
> = new Map()

export function addRegisteredControls(
  sourceFile: string,
  moduleNameOrPath: string,
  newDescriptorsForFile: ControlsToCheck,
): void {
  const newControlsToCheck = {
    moduleNameOrPath: moduleNameOrPath,
    newDescriptorsForFile: newDescriptorsForFile,
  }
  let existing = controlsRegisteredByFileInLastRender.get(sourceFile) ?? []
  existing.push(newControlsToCheck)
  controlsRegisteredByFileInLastRender.set(sourceFile, existing)
}

export function clearAllRegisteredControls() {
  allControlsRegisteredByFileEver = new Map()
  previousRegisteredModules = emptySet()
}

export async function validateControlsToCheck(
  dispatch: EditorDispatch,
  propertyControlsInfo: PropertyControlsInfo,
  resolvedFileNames: Array<string>,
  evaluatedFileNames: Array<string>,
): Promise<void> {
  // Replace the registered controls for the files that were evaluated on this canvas render
  fastForEach(evaluatedFileNames, (fileName) => {
    allControlsRegisteredByFileEver.set(
      fileName,
      controlsRegisteredByFileInLastRender.get(fileName) ?? [],
    )
  })

  // Now clear current render's map
  controlsRegisteredByFileInLastRender = new Map()

  let shouldDispatch: boolean = false // We only dispatch on changes to the controls
  let allRegisteredModules: Set<string> = new Set() // We need to track all of the registered modules

  let updatedPropertyControlsInfo: PropertyControlsInfo = {} // These are the ones that have actually changed

  // Gather all of the registered controls info in each file
  const registeredControlsInfoInEachFile = resolvedFileNames.map(
    (fileName) => allControlsRegisteredByFileEver.get(fileName) ?? [],
  )

  // Now create the Component Descriptors for all of the registered modules
  for (const controlsInfoToCheckArray of registeredControlsInfoInEachFile) {
    for (const controlsInfoToCheck of controlsInfoToCheckArray) {
      const newDescriptorsForFileEither = await controlsInfoToCheck.newDescriptorsForFile

      forEachRight(newDescriptorsForFileEither, (newDescriptorsForFileArray) => {
        allRegisteredModules.add(controlsInfoToCheck.moduleNameOrPath)

        const newDescriptorsForFile: ComponentDescriptorsForFile = mapArrayToDictionary(
          newDescriptorsForFileArray,
          (descriptorWithName) => descriptorWithName.componentName,
          (descriptorWithName) => {
            return {
              properties: descriptorWithName.properties,
              supportsChildren: descriptorWithName.supportsChildren,
              variants: descriptorWithName.variants,
            }
          },
        )

        const currentDescriptorsForFile =
          propertyControlsInfo[controlsInfoToCheck.moduleNameOrPath] ?? {}
        const descriptorsChanged = !deepEqual(currentDescriptorsForFile, newDescriptorsForFile)

        if (descriptorsChanged) {
          shouldDispatch = true
          // Merge with any existing entries if there are any.
          updatedPropertyControlsInfo[controlsInfoToCheck.moduleNameOrPath] = {
            ...(updatedPropertyControlsInfo[controlsInfoToCheck.moduleNameOrPath] ?? {}),
            ...newDescriptorsForFile,
          }
        }
      })
    }
  }

  // Capture those that have been deleted.
  let moduleNamesOrPathsToDelete: Array<string> = []
  for (const previousModuleNameOrPath of previousRegisteredModules) {
    if (!allRegisteredModules.has(previousModuleNameOrPath)) {
      shouldDispatch = true
      moduleNamesOrPathsToDelete.push(previousModuleNameOrPath)
    }
  }

  previousRegisteredModules = allRegisteredModules

  if (shouldDispatch) {
    dispatch([updatePropertyControlsInfo(updatedPropertyControlsInfo, moduleNamesOrPathsToDelete)])
  }
}
