import type { ProjectContentTreeRoot, ProjectContentsTree } from '../../../components/assets'
import {
  isProjectContentFile,
  getProjectFileFromTree,
  zipContentsTree,
} from '../../../components/assets'
import {
  getHighlightBoundsForProject,
  getSavedCodeFromTextFile,
  getUnsavedCodeFromTextFile,
} from '../../../core/model/project-file-utils'
import type { ProjectFile } from '../../../core/shared/project-file-types'
import { isTextFile } from '../../../core/shared/project-file-types'
import { fastForEach, isBrowserEnvironment } from '../../../core/shared/utils'
import {
  applyProjectChanges,
  getCodeEditorDecorations,
  getSelectedElementChangedMessage,
  sendMessage,
} from '../../../core/vscode/vscode-bridge'
import type {
  UpdateDecorationsMessage,
  SelectedElementChanged,
  AccumulatedToVSCodeMessage,
  ToVSCodeMessageNoAccumulated,
} from 'utopia-vscode-common'
import { accumulatedToVSCodeMessage, toVSCodeExtensionMessage } from 'utopia-vscode-common'
import type { EditorState } from './editor-state'
import { getHighlightBoundsForElementPaths, getUnderlyingVSCodeBridgeID } from './editor-state'
import { shallowEqual } from '../../../core/shared/equality-utils'
import * as EP from '../../../core/shared/element-path'

export interface WriteProjectFileChange {
  type: 'WRITE_PROJECT_FILE'
  fullPath: string
  projectFile: ProjectFile
}

export function writeProjectFileChange(
  fullPath: string,
  projectFile: ProjectFile,
): WriteProjectFileChange {
  return {
    type: 'WRITE_PROJECT_FILE',
    fullPath: fullPath,
    projectFile: projectFile,
  }
}

export interface DeletePathChange {
  type: 'DELETE_PATH'
  fullPath: string
  recursive: boolean
}

export function deletePathChange(fullPath: string, recursive: boolean): DeletePathChange {
  return {
    type: 'DELETE_PATH',
    fullPath: fullPath,
    recursive: recursive,
  }
}

export interface EnsureDirectoryExistsChange {
  type: 'ENSURE_DIRECTORY_EXISTS'
  fullPath: string
}

export function ensureDirectoryExistsChange(fullPath: string): EnsureDirectoryExistsChange {
  return {
    type: 'ENSURE_DIRECTORY_EXISTS',
    fullPath: fullPath,
  }
}

export type ProjectFileChange =
  | WriteProjectFileChange
  | DeletePathChange
  | EnsureDirectoryExistsChange

export function collateProjectChanges(
  projectID: string,
  oldContents: ProjectContentTreeRoot,
  newContents: ProjectContentTreeRoot,
): Array<ProjectFileChange> {
  let changesToProcess: Array<ProjectFileChange> = []

  function applyChanges(
    fullPath: string,
    firstContents: ProjectContentsTree,
    secondContents: ProjectContentsTree,
  ): boolean {
    if (isProjectContentFile(firstContents)) {
      if (isProjectContentFile(secondContents)) {
        if (firstContents.content === secondContents.content) {
          // Do nothing, no change.
        } else if (isTextFile(firstContents.content) && isTextFile(secondContents.content)) {
          // We need to be careful around only sending this across if the text has been updated.
          const firstSavedContent = getSavedCodeFromTextFile(firstContents.content)
          const firstUnsavedContent = getUnsavedCodeFromTextFile(firstContents.content)
          const secondSavedContent = getSavedCodeFromTextFile(secondContents.content)
          const secondUnsavedContent = getUnsavedCodeFromTextFile(secondContents.content)

          const savedContentChanged = firstSavedContent !== secondSavedContent
          const unsavedContentChanged = firstUnsavedContent !== secondUnsavedContent
          const fileMarkedDirtyButNoCodeChangeYet =
            firstUnsavedContent == null && secondUnsavedContent === firstSavedContent

          const fileShouldBeWritten =
            // This means that we'll only send the code across when it is in sync with the parsed model, rather
            // than sending a stale version of the code across whilst waiting on the new version.
            secondContents.content.fileContents.revisionsState === 'BOTH_MATCH' &&
            // When a parsed model is updated but that change hasn't been reflected in the code yet, we end up with a file
            // that has no code change, so we don't want to write that to the FS for VS Code to act on it until the new code
            // has been generated
            (savedContentChanged || (unsavedContentChanged && !fileMarkedDirtyButNoCodeChangeYet))

          if (fileShouldBeWritten) {
            changesToProcess.push(writeProjectFileChange(fullPath, secondContents.content))
          }
        } else {
          changesToProcess.push(writeProjectFileChange(fullPath, secondContents.content))
        }
      } else {
        changesToProcess.push(deletePathChange(fullPath, true))
        changesToProcess.push(ensureDirectoryExistsChange(fullPath))
      }
    } else {
      if (isProjectContentFile(secondContents)) {
        changesToProcess.push(deletePathChange(fullPath, true))
        changesToProcess.push(writeProjectFileChange(fullPath, secondContents.content))
      } else {
        // Do nothing, both sides are a directory.
      }
    }

    return true
  }

  function onElement(
    fullPath: string,
    firstContents: ProjectContentsTree | null,
    secondContents: ProjectContentsTree | null,
  ): boolean {
    if (firstContents == null) {
      if (secondContents == null) {
        // Do nothing, nothing exists.
        return false
      } else {
        changesToProcess.push(
          writeProjectFileChange(fullPath, getProjectFileFromTree(secondContents)),
        )
        return true
      }
    } else {
      if (secondContents == null) {
        changesToProcess.push(deletePathChange(fullPath, true))
        return false
      } else {
        if (firstContents === secondContents) {
          // Same value, stop here.
          return false
        } else {
          return applyChanges(fullPath, firstContents, secondContents)
        }
      }
    }
  }
  if (isBrowserEnvironment) {
    if (oldContents != newContents) {
      zipContentsTree(oldContents, newContents, onElement)
    }
  }

  return changesToProcess
}

export function shouldIncludeVSCodeDecorations(
  oldEditorState: EditorState,
  newEditorState: EditorState,
): boolean {
  const oldHighlightBounds = getHighlightBoundsForElementPaths(
    [...oldEditorState.highlightedViews, ...oldEditorState.selectedViews],
    oldEditorState,
  )
  const newHighlightBounds = getHighlightBoundsForElementPaths(
    [...newEditorState.highlightedViews, ...newEditorState.selectedViews],
    newEditorState,
  )
  return !(
    EP.arrayOfPathsEqual(oldEditorState.selectedViews, newEditorState.selectedViews) &&
    EP.arrayOfPathsEqual(oldEditorState.highlightedViews, newEditorState.highlightedViews) &&
    shallowEqual(oldHighlightBounds, newHighlightBounds)
  )
}

export function shouldIncludeSelectedElementChanges(
  oldEditorState: EditorState,
  newEditorState: EditorState,
): boolean {
  const oldHighlightBounds = getHighlightBoundsForElementPaths(
    oldEditorState.selectedViews,
    oldEditorState,
  )
  const newHighlightBounds = getHighlightBoundsForElementPaths(
    newEditorState.selectedViews,
    newEditorState,
  )
  return (
    !(
      EP.arrayOfPathsEqual(oldEditorState.selectedViews, newEditorState.selectedViews) &&
      shallowEqual(oldHighlightBounds, newHighlightBounds)
    ) && newEditorState.selectedViews.length > 0
  )
}

export function getProjectContentsChanges(
  oldEditorState: EditorState,
  newEditorState: EditorState,
): Array<ProjectFileChange> {
  if (oldEditorState.vscodeBridgeId != null) {
    return collateProjectChanges(
      getUnderlyingVSCodeBridgeID(oldEditorState.vscodeBridgeId),
      oldEditorState.projectContents,
      newEditorState.projectContents,
    )
  } else {
    return []
  }
}

export interface ProjectChanges {
  fileChanges: Array<ProjectFileChange>
  updateDecorations: UpdateDecorationsMessage | null
  selectedChanged: SelectedElementChanged | null
}

function combineFileChanges(
  first: Array<ProjectFileChange>,
  second: Array<ProjectFileChange>,
): Array<ProjectFileChange> {
  let writeFilePathsSeen: Set<string> = new Set()
  let reversedResult: Array<ProjectFileChange> = []
  fastForEach([...first, ...second].reverse(), (change) => {
    if (change.type === 'WRITE_PROJECT_FILE') {
      if (writeFilePathsSeen.has(change.fullPath)) {
        // We only want the last version of each file
        return
      } else {
        reversedResult.push(change)
        writeFilePathsSeen.add(change.fullPath)
      }
    } else {
      reversedResult.push(change)
    }
  })

  const result = reversedResult.reverse()
  return result
}

export function combineProjectChanges(
  first: ProjectChanges,
  second: ProjectChanges,
): ProjectChanges {
  return {
    fileChanges: combineFileChanges(first.fileChanges, second.fileChanges),
    updateDecorations: second.updateDecorations ?? first.updateDecorations,
    selectedChanged: second.selectedChanged ?? first.selectedChanged,
  }
}

export const emptyProjectChanges: ProjectChanges = {
  fileChanges: [],
  updateDecorations: null,
  selectedChanged: null,
}

export function projectChangesToVSCodeMessages(local: ProjectChanges): AccumulatedToVSCodeMessage {
  let messages: Array<ToVSCodeMessageNoAccumulated> = []
  if (local.updateDecorations != null) {
    messages.push(local.updateDecorations)
  }
  if (local.selectedChanged != null) {
    messages.push(local.selectedChanged)
  }
  return accumulatedToVSCodeMessage(messages)
}

export function getProjectChanges(
  oldEditorState: EditorState,
  newEditorState: EditorState,
  updatedFromVSCode: boolean,
): ProjectChanges {
  return {
    fileChanges: updatedFromVSCode ? [] : getProjectContentsChanges(oldEditorState, newEditorState),
    updateDecorations: shouldIncludeVSCodeDecorations(oldEditorState, newEditorState)
      ? getCodeEditorDecorations(newEditorState)
      : null,
    selectedChanged:
      !updatedFromVSCode && shouldIncludeSelectedElementChanges(oldEditorState, newEditorState)
        ? getSelectedElementChangedMessage(newEditorState)
        : null,
  }
}

export function sendVSCodeChanges(changes: ProjectChanges) {
  applyProjectChanges(changes.fileChanges)
  const toVSCodeAccumulated = projectChangesToVSCodeMessages(changes)
  if (toVSCodeAccumulated.messages.length > 0) {
    sendMessage(toVSCodeExtensionMessage(toVSCodeAccumulated))
  }
}
