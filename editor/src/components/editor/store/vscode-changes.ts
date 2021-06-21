import {
  ProjectContentTreeRoot,
  ProjectContentsTree,
  isProjectContentFile,
  getProjectFileFromTree,
  zipContentsTree,
} from '../../../components/assets'
import {
  getSavedCodeFromTextFile,
  getUnsavedCodeFromTextFile,
} from '../../../core/model/project-file-utils'
import { ProjectFile, isTextFile } from '../../../core/shared/project-file-types'
import { fastForEach, isBrowserEnvironment } from '../../../core/shared/utils'
import {
  applyProjectChanges,
  getCodeEditorDecorations,
  getSelectedElementChangedMessage,
} from '../../../core/vscode/vscode-bridge'
import {
  UpdateDecorationsMessage,
  SelectedElementChanged,
  AccumulatedToVSCodeMessage,
  ToVSCodeMessageNoAccumulated,
  accumulatedToVSCodeMessage,
  sendMessage,
} from 'utopia-vscode-common'
import { EditorState, getHighlightBoundsForUids } from './editor-state'

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

export type ProjectChange = WriteProjectFileChange | DeletePathChange | EnsureDirectoryExistsChange

export function collateProjectChanges(
  projectID: string,
  oldContents: ProjectContentTreeRoot,
  newContents: ProjectContentTreeRoot,
): Array<ProjectChange> {
  let changesToProcess: Array<ProjectChange> = []

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

          // When a parsed model is updated but that change hasn't been reflected in the code yet, we end up with a file
          // that has no code change, so we don't want to write that to the FS for VS Code to act on it until the new code
          // has been generated
          const fileShouldBeWritten =
            savedContentChanged || (unsavedContentChanged && !fileMarkedDirtyButNoCodeChangeYet)

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
  const oldHighlightBounds = getHighlightBoundsForUids(oldEditorState)
  const newHighlightBounds = getHighlightBoundsForUids(newEditorState)
  return (
    oldEditorState.selectedViews !== newEditorState.selectedViews ||
    oldEditorState.highlightedViews !== newEditorState.highlightedViews ||
    oldHighlightBounds !== newHighlightBounds
  )
}

export function shouldIncludeSelectedElementChanges(
  oldEditorState: EditorState,
  newEditorState: EditorState,
): boolean {
  return (
    oldEditorState.selectedViews !== newEditorState.selectedViews &&
    newEditorState.selectedViews.length > 0
  )
}

export function getProjectContentsChanges(
  oldEditorState: EditorState,
  newEditorState: EditorState,
  updateCameFromVSCode: boolean,
): Array<ProjectChange> {
  if (oldEditorState.vscodeBridgeId != null && !updateCameFromVSCode) {
    return collateProjectChanges(
      oldEditorState.vscodeBridgeId,
      oldEditorState.projectContents,
      newEditorState.projectContents,
    )
  } else {
    return []
  }
}

export interface AccumulatedVSCodeChanges {
  fileChanges: Array<ProjectChange>
  updateDecorations: UpdateDecorationsMessage | null
  selectedChanged: SelectedElementChanged | null
}

function combineFileChanges(
  first: Array<ProjectChange>,
  second: Array<ProjectChange>,
): Array<ProjectChange> {
  let writeFilePathsSeen: Set<string> = new Set()
  let reversedResult: Array<ProjectChange> = []
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

export function combineAccumulatedVSCodeChanges(
  first: AccumulatedVSCodeChanges,
  second: AccumulatedVSCodeChanges,
): AccumulatedVSCodeChanges {
  return {
    fileChanges: combineFileChanges(first.fileChanges, second.fileChanges),
    updateDecorations: second.updateDecorations ?? first.updateDecorations,
    selectedChanged: second.selectedChanged ?? first.selectedChanged,
  }
}

export const emptyAccumulatedVSCodeChanges: AccumulatedVSCodeChanges = {
  fileChanges: [],
  updateDecorations: null,
  selectedChanged: null,
}

export function localAccumulatedToVSCodeAccumulated(
  local: AccumulatedVSCodeChanges,
): AccumulatedToVSCodeMessage {
  let messages: Array<ToVSCodeMessageNoAccumulated> = []
  if (local.updateDecorations != null) {
    messages.push(local.updateDecorations)
  }
  if (local.selectedChanged != null) {
    messages.push(local.selectedChanged)
  }
  return accumulatedToVSCodeMessage(messages)
}

export function getVSCodeChanges(
  oldEditorState: EditorState,
  newEditorState: EditorState,
  updateCameFromVSCode: boolean,
): AccumulatedVSCodeChanges {
  return {
    fileChanges: getProjectContentsChanges(oldEditorState, newEditorState, updateCameFromVSCode),
    updateDecorations: shouldIncludeVSCodeDecorations(oldEditorState, newEditorState)
      ? getCodeEditorDecorations(newEditorState)
      : null,
    selectedChanged: shouldIncludeSelectedElementChanges(oldEditorState, newEditorState)
      ? getSelectedElementChangedMessage(newEditorState)
      : null,
  }
}

export async function sendVSCodeChanges(changes: AccumulatedVSCodeChanges): Promise<void> {
  await applyProjectChanges(changes.fileChanges)
  const toVSCodeAccumulated = localAccumulatedToVSCodeAccumulated(changes)
  if (toVSCodeAccumulated.messages.length > 0) {
    await sendMessage(toVSCodeAccumulated)
  }
  return Promise.resolve()
}
