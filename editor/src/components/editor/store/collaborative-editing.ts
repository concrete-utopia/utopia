import type { CollabTextFile, CollabTextFileTopLevelElements } from './editor-state'
import { type CollaborativeEditingSupport } from './editor-state'
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
import {
  applyCollabFileUpdate,
  updateFile,
  updateTopLevelElements,
} from '../actions/action-creators'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { assertNever, isBrowserEnvironment } from '../../../core/shared/utils'
import type { ParseSuccess } from '../../../core/shared/project-file-types'
import { isTextFile } from '../../../core/shared/project-file-types'
import {
  ParsedTextFileKeepDeepEquality,
  TextFileKeepDeepEquality,
  TopLevelElementKeepDeepEquality,
} from './store-deep-equality-instances'
import * as Y from 'yjs'
import type { TopLevelElement } from '../../../core/shared/element-template'
import { v4 as UUID } from 'uuid'

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
  mergeDoc: Y.Doc,
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
        const parsedPart = change.projectFile.fileContents.parsed
        let collabFile: CollabTextFile
        if (projectContentsMap.has(change.fullPath)) {
          collabFile = projectContentsMap.get(change.fullPath)!
          // TODO Handle not text type
        } else {
          collabFile = new Y.Map()
          collabFile.set('type', 'TEXT_FILE')
          const topLevelElementsArray = new Y.Array<TopLevelElement>()
          collabFile.set('topLevelElements', topLevelElementsArray)
        }

        mergeDoc.transact(() => {
          if (!projectContentsMap.has(change.fullPath)) {
            projectContentsMap.set(change.fullPath, collabFile)
          }
          synchroniseParseSuccessToCollabFile(parsedPart, collabFile)
        })
      }
      break
    case 'ENSURE_DIRECTORY_EXISTS':
      // Not necessary to handle right now.
      break
    default:
      assertNever(change)
  }
}

// FIXME This is still being called on load
export function updateCollaborativeProjectContents(
  collaborativeEditingSupport: CollaborativeEditingSupport,
  projectChanges: ProjectChanges,
): void {
  const projectContentsMap = collaborativeEditingSupport.projectContents
  for (const change of projectChanges.fileChanges.collabProjectChanges) {
    applyFileChangeToMap(change, projectContentsMap, collaborativeEditingSupport.mergeDoc)
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
      collaborativeEditingSupport.mergeDoc,
    )
  })
}

export function addHookForProjectChanges(
  collaborativeEditingSupport: CollaborativeEditingSupport,
  dispatch: EditorDispatch,
): void {
  // collaborativeEditingSupport.projectContents.observe((changeEvent) => {
  //   console.log(`observe`, changeEvent.changes)
  //   changeEvent.changes.keys.forEach((change, key) => {
  //     switch (change.action) {
  //       case 'add':
  //       case 'update':
  //         const file = forceNotNull(
  //           'Should not be null.',
  //           collaborativeEditingSupport.projectContents.get(key),
  //         )
  //         dispatch([applyCollabFileUpdate(key, file)])
  //         break
  //       case 'delete':
  //         console.error('Deletes to be handled later.')
  //         break
  //       default:
  //         console.error(`Not sure what this is: ${change}`)
  //     }
  //   })
  // })

  collaborativeEditingSupport.projectContents.observeDeep((changeEvents) => {
    // TODO Check that this is the array change before doing anything
    changeEvents.forEach((changeEvent) => {
      if (changeEvent.path.length === 0) {
        return
      }
      const filePath = changeEvent.path[0] as string
      // FIXME Can be null actually
      const file = forceNotNull(
        `Could not find file ${filePath}`,
        collaborativeEditingSupport.projectContents.get(filePath),
      )
      const oldTopLevelElements = file.get('topLevelElements') as CollabTextFileTopLevelElements
      let newTopLevelElements: Array<TopLevelElement> = []
      let writeIndex = 0
      let readIndex = 0
      changeEvent.delta.forEach((delta) => {
        if (delta.retain != undefined) {
          const elementsToPush = oldTopLevelElements.slice(readIndex, readIndex + delta.retain)
          newTopLevelElements.push(...elementsToPush)
          readIndex += delta.retain
          writeIndex += delta.retain
        }
        if (delta.insert != null && Array.isArray(delta.insert)) {
          newTopLevelElements.push(...delta.insert)
          writeIndex += delta.insert.length
        }
        if (delta.delete != undefined) {
          readIndex += delta.delete
        }
      })

      if (readIndex < oldTopLevelElements.length) {
        // There is an implicit retain for the remainder of the items
        const elementsToPush = oldTopLevelElements.slice(readIndex)
        newTopLevelElements.push(...elementsToPush)
      }

      dispatch([updateTopLevelElements(filePath, newTopLevelElements)])
    })
  })
}

export interface ArrayChanges {
  updatesAt: Array<number>
  deleteFrom: number | null
}

function arrayChanges(updatesAt: Array<number>, deleteFrom: number | null): ArrayChanges {
  return {
    updatesAt: updatesAt,
    deleteFrom: deleteFrom,
  }
}

export function calculateArrayChanges<T>(
  from: Array<T>,
  into: Y.Array<T>,
  equals: (from: T, into: T) => boolean = (fromToCheck, intoToCheck) => fromToCheck === intoToCheck,
): ArrayChanges {
  let updatesAt: Array<number> = []
  for (let index: number = 0; index < Math.max(from.length, into.length); index++) {
    if (!equals(from[index], into.get(index))) {
      updatesAt.push(index)
    }
  }

  let deleteFrom: number | null = null
  if (into.length > from.length) {
    deleteFrom = into.length
  }

  return arrayChanges(updatesAt, deleteFrom)
}

function synchroniseParseSuccessToCollabFile(
  success: ParseSuccess,
  collabFile: CollabTextFile,
): void {
  const collabFileTopLevelElements = collabFile.get('topLevelElements')
  if (collabFileTopLevelElements === 'TEXT_FILE' || collabFileTopLevelElements === undefined) {
    throw new Error('Invalid value for topLevelElements.')
  } else {
    const changes = calculateArrayChanges(
      success.topLevelElements,
      collabFileTopLevelElements,
      // Fix this shitshow up
      (l, r) => l != null && r != null && TopLevelElementKeepDeepEquality(l, r).areEqual,
    )
    for (const updateAtIndex of changes.updatesAt) {
      if (collabFileTopLevelElements.length > updateAtIndex) {
        collabFileTopLevelElements.delete(updateAtIndex, 1)
      }
      collabFileTopLevelElements.insert(updateAtIndex, [success.topLevelElements[updateAtIndex]])
    }
    if (changes.deleteFrom != null) {
      collabFileTopLevelElements.delete(changes.deleteFrom)
    }
  }
}
