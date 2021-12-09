import {
  ComponentDescriptorsForFile,
  ComponentDescriptorWithName,
  PropertyControlsInfo,
} from '../custom-code/code-file'
import { EditorState } from '../editor/store/editor-state'
import deepEqual from 'fast-deep-equal'
import { EditorDispatch } from '../editor/action-types'
import { updatePropertyControlsInfo } from '../editor/actions/action-creators'
import { emptySet } from '../../core/shared/set-utils'
import { Either, forEachLeft, forEachRight } from '../../core/shared/either'
import { mapArrayToDictionary } from '../../core/shared/array-utils'

export type ControlsToCheck = Promise<Either<string, Array<ComponentDescriptorWithName>>>

export interface PropertyControlsInfoToCheck {
  moduleNameOrPath: string
  newDescriptorsForFile: ControlsToCheck
}

let previousModuleNamesOrPaths: Set<string> = emptySet()
let controlsToCheck: Array<PropertyControlsInfoToCheck> = []

export function addControlsToCheck(
  moduleNameOrPath: string,
  newDescriptorsForFile: ControlsToCheck,
): void {
  controlsToCheck.push({
    moduleNameOrPath: moduleNameOrPath,
    newDescriptorsForFile: newDescriptorsForFile,
  })
}

export function resetControlsToCheck(): void {
  previousModuleNamesOrPaths.clear()
  controlsToCheck.forEach((control) => {
    previousModuleNamesOrPaths.add(control.moduleNameOrPath)
  })
  controlsToCheck = []
}

export async function validateControlsToCheck(
  dispatch: EditorDispatch,
  propertyControlsInfo: PropertyControlsInfo,
): Promise<void> {
  let shouldDispatch: boolean = false
  let updatedPropertyControlsInfo: PropertyControlsInfo = {}
  // This should capture new or updated property controls.
  for (const toCheck of controlsToCheck) {
    const currentDescriptorsForFile = propertyControlsInfo[toCheck.moduleNameOrPath] ?? {}
    const newDescriptorsForFileEither = await toCheck.newDescriptorsForFile
    // The descriptors check out.
    forEachRight(newDescriptorsForFileEither, (newDescriptorsForFileArray) => {
      // FIXME At what point should we be caching / memoising this?
      const newDescriptorsForFile: ComponentDescriptorsForFile = mapArrayToDictionary(
        newDescriptorsForFileArray,
        (descriptorWithName) => descriptorWithName.componentName,
        (descriptorWithName) => {
          return {
            propertyControls: descriptorWithName.propertyControls,
            insertOptions: descriptorWithName.insertOptions,
          }
        },
      )
      const descriptorsChanged = !deepEqual(currentDescriptorsForFile, newDescriptorsForFile)
      if (descriptorsChanged) {
        shouldDispatch = true
        // Merge with any existing entries if there are any.
        updatedPropertyControlsInfo[toCheck.moduleNameOrPath] = {
          ...(updatedPropertyControlsInfo[toCheck.moduleNameOrPath] ?? {}),
          ...newDescriptorsForFile,
        }
      }
    })
    // There was some kind of an error with the descriptors.
    forEachLeft(newDescriptorsForFileEither, (error) => {
      console.error(
        `There was a problem with 'registerModule' ${toCheck.moduleNameOrPath}: ${error}`,
      )
    })
  }
  // Capture those that have been deleted.
  let moduleNamesOrPathsToDelete: Array<string> = []
  for (const previousModuleNameOrPath of previousModuleNamesOrPaths) {
    const foundIt = controlsToCheck.some(
      (control) => control.moduleNameOrPath === previousModuleNameOrPath,
    )
    if (!foundIt) {
      shouldDispatch = true
      moduleNamesOrPathsToDelete.push(previousModuleNameOrPath)
    }
  }

  // Only send if there's something to send.
  if (shouldDispatch) {
    dispatch([updatePropertyControlsInfo(updatedPropertyControlsInfo, moduleNamesOrPathsToDelete)])
  }
}
