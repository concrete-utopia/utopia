import type { CollaborativeEditingSupport } from './editor-state'
import {
  writeProjectFileChange,
  type ProjectChanges,
  type ProjectFileChange,
} from './vscode-changes'
import { drop } from '../../../core/shared/array-utils'
import { walkContentsTree, type ProjectContentTreeRoot } from '../../../components/assets'
import type { EditorDispatch } from '../action-types'
import { updateFile } from '../actions/action-creators'
import { forceNotNull } from '../../../core/shared/optional-utils'

function applyFileChangeToMap(
  change: ProjectFileChange,
  projectContentsMap: CollaborativeEditingSupport['projectContents'],
): void {
  console.log('applyFileChangeToMap', change.fullPath, change.type, projectContentsMap.toJSON())
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
      projectContentsMap.set(
        change.fullPath,
        change.projectFile.type === 'DIRECTORY' ? 'DIRECTORY' : change.projectFile,
      )
      break
    case 'ENSURE_DIRECTORY_EXISTS':
      const splitPath = change.fullPath.split('/')
      if (splitPath.length > 0) {
        function checkDir(pathToCheck: string): void {
          let shouldCreateDir: boolean = false
          if (projectContentsMap.has(pathToCheck)) {
            const value = projectContentsMap.get(pathToCheck)
            if (value !== 'DIRECTORY') {
              shouldCreateDir = true
            }
          } else {
            shouldCreateDir = true
          }

          if (shouldCreateDir) {
            projectContentsMap.set(pathToCheck, 'DIRECTORY')
          }
        }
        let path = splitPath[0]
        checkDir(path)
        for (const pathPart of drop(1, splitPath)) {
          path += '/'
          path += pathPart
          checkDir(path)
        }
      }
      break
  }
}

export function updateCollaborativeProjectContents(
  collaborativeEditingSupport: CollaborativeEditingSupport,
  projectChanges: ProjectChanges,
): void {
  const projectContentsMap = collaborativeEditingSupport.projectContents
  console.log('projectContentsMap', projectContentsMap.toJSON())
  for (const change of projectChanges.fileChanges.allChanges) {
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
          if (file === 'DIRECTORY') {
            console.log('Handling directories later.')
          } else {
            console.log(`updateFile for ${key}`)
            dispatch([updateFile(key, file, true)])
          }
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
