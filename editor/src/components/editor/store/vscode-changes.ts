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
  FromUtopiaToVSCodeMessage,
} from 'utopia-vscode-common'
import type { EditorState } from './editor-state'
import { getHighlightBoundsForElementPaths } from './editor-state'
import { shallowEqual } from '../../../core/shared/equality-utils'
import * as EP from '../../../core/shared/element-path'
import { collateCollaborativeProjectChanges } from './collaborative-editing'

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

          const revisionStateWarrantWrite =
            secondContents.content.fileContents.revisionsState === 'BOTH_MATCH' ||
            secondContents.content.fileContents.revisionsState ===
              'CODE_AHEAD_BUT_PLEASE_TELL_VSCODE_ABOUT_IT'

          const fileShouldBeWritten =
            // This means that we'll only send the code across when it is in sync with the parsed model, rather
            // than sending a stale version of the code across whilst waiting on the new version.
            revisionStateWarrantWrite &&
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
  return (
    !EP.arrayOfPathsEqual(oldEditorState.selectedViews, newEditorState.selectedViews) &&
    newEditorState.selectedViews.length > 0
  )
}

export interface ProjectContentProjectChanges {
  collabProjectChanges: Array<ProjectFileChange>
  changesForVSCode: Array<ProjectFileChange>
}

export function getProjectContentsChanges(
  oldEditorState: EditorState,
  newEditorState: EditorState,
): ProjectContentProjectChanges {
  const projectChanges = collateProjectChanges(
    oldEditorState.projectContents,
    newEditorState.projectContents,
  )
  const collabProjectChanges = collateCollaborativeProjectChanges(
    oldEditorState.projectContents,
    newEditorState.projectContents,
  )

  return {
    collabProjectChanges: collabProjectChanges,
    changesForVSCode: projectChanges,
  }
}

export interface ProjectChanges {
  fileChanges: ProjectContentProjectChanges
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
    fileChanges: {
      changesForVSCode: combineFileChanges(
        first.fileChanges.changesForVSCode,
        second.fileChanges.changesForVSCode,
      ),
      collabProjectChanges: combineFileChanges(
        first.fileChanges.collabProjectChanges,
        second.fileChanges.collabProjectChanges,
      ),
    },
    updateDecorations: second.updateDecorations ?? first.updateDecorations,
    selectedChanged: second.selectedChanged ?? first.selectedChanged,
  }
}

export const emptyProjectChanges: ProjectChanges = {
  fileChanges: {
    changesForVSCode: [],
    collabProjectChanges: [],
  },
  updateDecorations: null,
  selectedChanged: null,
}

function projectChangesToVSCodeMessages(local: ProjectChanges): Array<FromUtopiaToVSCodeMessage> {
  let messages: Array<FromUtopiaToVSCodeMessage> = []
  if (local.updateDecorations != null) {
    messages.push(local.updateDecorations)
  }
  if (local.selectedChanged != null) {
    messages.push(local.selectedChanged)
  }
  return messages
}

export function getProjectChanges(
  oldEditorState: EditorState,
  newEditorState: EditorState,
  updatedFromVSCode: boolean,
): ProjectChanges {
  const projectChanges = getProjectContentsChanges(oldEditorState, newEditorState)
  return {
    fileChanges: updatedFromVSCode ? { ...projectChanges, changesForVSCode: [] } : projectChanges,
    updateDecorations: shouldIncludeVSCodeDecorations(oldEditorState, newEditorState)
      ? getCodeEditorDecorations(newEditorState)
      : null,
    selectedChanged:
      !updatedFromVSCode && shouldIncludeSelectedElementChanges(oldEditorState, newEditorState)
        ? getSelectedElementChangedMessage(newEditorState, 'do-not-force-navigation')
        : null,
  }
}

export function sendVSCodeChanges(changes: ProjectChanges) {
  applyProjectChanges(changes.fileChanges.changesForVSCode)
  const toVSCodeAccumulated = projectChangesToVSCodeMessages(changes)
  toVSCodeAccumulated.forEach((message) => sendMessage(message))
}
