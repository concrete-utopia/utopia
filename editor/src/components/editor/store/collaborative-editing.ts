import type {
  CollabTextFile,
  CollabTextFileTopLevelElements,
  CollaborativeEditingSupportSession,
} from './editor-state'
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
  addFileToProjectContents,
} from '../../../components/assets'
import type { EditorDispatch } from '../action-types'
import { updateProjectContents, updateTopLevelElements } from '../actions/action-creators'
import { assertNever, isBrowserEnvironment } from '../../../core/shared/utils'
import type { ParseSuccess } from '../../../core/shared/project-file-types'
import {
  RevisionsState,
  isTextFile,
  parseSuccess,
  textFile,
  textFileContents,
} from '../../../core/shared/project-file-types'
import {
  ParsedTextFileKeepDeepEquality,
  TopLevelElementKeepDeepEquality,
} from './store-deep-equality-instances'
import * as Y from 'yjs'
import type { TopLevelElement } from '../../../core/shared/element-template'
import { isFeatureEnabled } from '../../../utils/feature-switches'

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
  if (isBrowserEnvironment && isFeatureEnabled('Collaboration')) {
    if (oldContents != newContents) {
      zipContentsTree(oldContents, newContents, onElement)
    }
  }

  return changesToProcess
}

function applyFileChangeToMap(
  change: ProjectFileChange,
  projectContentsMap: CollaborativeEditingSupportSession['projectContents'],
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
  session: CollaborativeEditingSupportSession,
  projectChanges: ProjectChanges,
  filesModifiedByElsewhere: Array<string>,
): void {
  if (isFeatureEnabled('Collaboration')) {
    const projectContentsMap = session.projectContents
    for (const change of projectChanges.fileChanges.collabProjectChanges) {
      if (!filesModifiedByElsewhere.includes(change.fullPath)) {
        applyFileChangeToMap(change, projectContentsMap, session.mergeDoc)
      }
    }
  }
}

export function populateCollaborativeProjectContents(
  session: CollaborativeEditingSupportSession,
  projectContents: ProjectContentTreeRoot,
): void {
  walkContentsTree(projectContents, (fullPath, file) => {
    applyFileChangeToMap(
      writeProjectFileChange(fullPath, file),
      session.projectContents,
      session.mergeDoc,
    )
  })
}

export function addHookForProjectChanges(
  session: CollaborativeEditingSupportSession,
  dispatch: EditorDispatch,
): void {
  if (isFeatureEnabled('Collaboration')) {
    session.projectContents.observeDeep((changeEvents) => {
      // TODO Check that this is the array change before doing anything
      for (const changeEvent of changeEvents) {
        switch (changeEvent.path.length) {
          case 0: {
            updateEntireProjectContents(session, changeEvent, dispatch)
            break
          }
          case 1: {
            const filePath = changeEvent.path[0] as string
            // TODO: Entire file update.
            break
          }
          case 2: {
            const filePath = changeEvent.path[0] as string
            if (changeEvent.path[1] !== 'topLevelElements') {
              throw new Error(`Unexpected second part of change path: ${changeEvent.path[1]}`)
            }
            updateTopLevelElementsOfFile(session, filePath, changeEvent, dispatch)
            break
          }
          default:
            throw new Error(`Unexpected change path: ${JSON.stringify(changeEvent.path)}`)
        }
      }
    })
  }
}

export interface ArrayChanges {
  updatesAt: Array<number>
  deleteFrom: number | null
}

function updateEntireProjectContents(
  session: CollaborativeEditingSupportSession,
  changeEvent: Y.YEvent<any>,
  dispatch: EditorDispatch,
): void {
  for (const delta of changeEvent.delta) {
    if (delta.retain != null) {
      // Do nothing.
    }
    if (delta.insert != null && Array.isArray(delta.insert)) {
      if (delta.insert instanceof Y.Map) {
        const insertMap: CollabTextFile = delta.insert
        let projectContents: ProjectContentTreeRoot = {}
        for (const entry of insertMap.entries()) {
          switch (entry[0]) {
            case 'type':
              // Ignore.
              break
            case 'topLevelElements':
              const success = parseSuccess({}, entry[1], {}, null, null, [], {})
              const newFile = textFile(
                textFileContents('', success, RevisionsState.ParsedAhead),
                null,
                null,
                1,
              )
              projectContents = addFileToProjectContents(projectContents, entry[0], newFile)
              break
            default:
              throw new Error(`Unknown key: ${entry[0]}`)
          }
        }
        dispatch([updateProjectContents(projectContents)])
      }
    }
    if (delta.delete != null) {
      dispatch([updateProjectContents({})])
    }
  }
}

function updateTopLevelElementsOfFile(
  session: CollaborativeEditingSupportSession,
  filePath: string,
  changeEvent: Y.YEvent<any>,
  dispatch: EditorDispatch,
): void {
  const file = session.projectContents.get(filePath)
  if (file != null) {
    const oldTopLevelElements = file.get('topLevelElements') as CollabTextFileTopLevelElements
    let newTopLevelElements: Array<TopLevelElement> = []
    let readIndex = 0
    for (const delta of changeEvent.delta) {
      if (delta.retain != undefined) {
        const elementsToPush = oldTopLevelElements.slice(readIndex, readIndex + delta.retain)
        newTopLevelElements.push(...elementsToPush)
        readIndex += delta.retain
      }
      if (delta.insert != null && Array.isArray(delta.insert)) {
        newTopLevelElements.push(...delta.insert)
      }
      if (delta.delete != undefined) {
        readIndex += delta.delete
      }
    }

    if (readIndex < oldTopLevelElements.length) {
      // There is an implicit retain for the remainder of the items
      const elementsToPush = oldTopLevelElements.slice(readIndex)
      newTopLevelElements.push(...elementsToPush)
    }

    dispatch([updateTopLevelElements(filePath, newTopLevelElements)])
  }
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
