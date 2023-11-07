import { collabTextFile, type CollaborativeEditingSupport } from './editor-state'
import {
  writeProjectFileChange,
  type ProjectChanges,
  type ProjectFileChange,
  deletePathChange,
  ensureDirectoryExistsChange,
} from './vscode-changes'
import type { ProjectContentsTree } from '../../../components/assets'
import {
  walkContentsTree,
  type ProjectContentTreeRoot,
  getProjectFileFromTree,
  isProjectContentFile,
  zipContentsTree,
} from '../../../components/assets'
import type { EditorDispatch } from '../action-types'
import { applyCollabFileUpdate, updateFile } from '../actions/action-creators'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { assertNever, isBrowserEnvironment } from '../../../core/shared/utils'
import { isTextFile } from '../../../core/shared/project-file-types'
import { ParsedTextFileKeepDeepEquality } from './store-deep-equality-instances'

export function collateCollaborativeProjectChanges(
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
          if (
            ParsedTextFileKeepDeepEquality(
              firstContents.content.fileContents.parsed,
              secondContents.content.fileContents.parsed,
            ).areEqual
          ) {
            // Do nothing, no change.
          } else {
            const firstRevisionState = firstContents.content.fileContents.revisionsState
            const secondRevisionState = secondContents.content.fileContents.revisionsState
            const revisionStateIsAppropriate = secondRevisionState === 'BOTH_MATCH'
            const fileShouldBeWritten = revisionStateIsAppropriate
            if (fileShouldBeWritten) {
              changesToProcess.push(writeProjectFileChange(fullPath, secondContents.content))
            }
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
function applyFileChangeToMap(
  change: ProjectFileChange,
  projectContentsMap: CollaborativeEditingSupport['projectContents'],
): void {
  switch (change.type) {
    case 'DELETE_PATH':
      const keyIterator = projectContentsMap.keys()
      let keyIteratorValue = keyIterator.next()
      while (!keyIteratorValue.done) {
        const key = keyIteratorValue.value
        if (
          key === change.fullPath ||
          (change.recursive && key.startsWith(`${change.fullPath}/`))
        ) {
          projectContentsMap.delete(key)
        }
      }
      break
    case 'WRITE_PROJECT_FILE':
      if (
        change.projectFile.type === 'TEXT_FILE' &&
        change.projectFile.fileContents.parsed.type === 'PARSE_SUCCESS'
      ) {
        projectContentsMap.set(
          change.fullPath,
          collabTextFile(change.projectFile.fileContents.parsed),
        )
      }
      break
    case 'ENSURE_DIRECTORY_EXISTS':
      // Not necessary to handle right now.
      break
    default:
      assertNever(change)
  }
}

export function updateCollaborativeProjectContents(
  collaborativeEditingSupport: CollaborativeEditingSupport,
  projectChanges: ProjectChanges,
): void {
  const projectContentsMap = collaborativeEditingSupport.projectContents
  for (const change of projectChanges.fileChanges.collabProjectChanges) {
    applyFileChangeToMap(change, projectContentsMap)
  }
}

export function populateCollaborativeProjectContents(
  collaborativeEditingSupport: CollaborativeEditingSupport,
  projectContents: ProjectContentTreeRoot,
): void {
  walkContentsTree(projectContents, (fullPath, file) => {
    applyFileChangeToMap(
      writeProjectFileChange(fullPath, file),
      collaborativeEditingSupport.projectContents,
    )
  })
}

export function addHookForProjectChanges(
  collaborativeEditingSupport: CollaborativeEditingSupport,
  dispatch: EditorDispatch,
): void {
  collaborativeEditingSupport.projectContents.observe((changeEvent) => {
    changeEvent.changes.keys.forEach((change, key) => {
      switch (change.action) {
        case 'add':
        case 'update':
          const file = forceNotNull(
            'Should not be null.',
            collaborativeEditingSupport.projectContents.get(key),
          )
          dispatch([applyCollabFileUpdate(key, file)])
          break
        case 'delete':
          console.error('Deletes to be handled later.')
          break
        default:
          console.error(`Not sure what this is: ${change}`)
      }
    })
  })
}
