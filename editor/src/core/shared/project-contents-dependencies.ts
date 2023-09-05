import type { MapLike } from 'typescript'
import type { ProjectContentsTree, ProjectContentTreeRoot } from '../../components/assets'
import {
  getProjectFileByFilePath,
  isProjectContentFile,
  walkContentsTree,
  zipContentsTree,
} from '../../components/assets'
import { resolveModule } from '../es-modules/package-manager/module-resolution'
import { getSavedCodeFromTextFile, getUnsavedCodeFromTextFile } from '../model/project-file-utils'
import { addToMapOfArraysUnique } from './array-utils'
import type { NodeModules } from './project-file-types'
import { isParseSuccess, isTextFile } from './project-file-types'
import { emptySet } from './set-utils'

export function getDirectReverseDependencies(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
): MapLike<Array<string>> {
  let result: MapLike<Array<string>> = {}
  walkContentsTree(projectContents, (fullPath, file) => {
    if (isTextFile(file)) {
      const parseResult = file.fileContents.parsed
      if (isParseSuccess(parseResult)) {
        for (const importSource of Object.keys(parseResult.imports)) {
          const resolveResult = resolveModule(projectContents, nodeModules, fullPath, importSource)
          switch (resolveResult.type) {
            case 'RESOLVE_SUCCESS':
              const successResult = resolveResult.success
              switch (successResult.file.type) {
                case 'ES_CODE_FILE':
                  // Check if the file is part of the project.
                  if (getProjectFileByFilePath(projectContents, successResult.path) != null) {
                    // Dependency, so record the connection.
                    result = addToMapOfArraysUnique(result, successResult.path, fullPath)
                  }
                  break
                case 'ES_REMOTE_DEPENDENCY_PLACEHOLDER':
                  // Do nothing for these.
                  break
                default:
                  const _exhaustiveCheck: never = successResult.file
                  throw new Error(`Unhandled case ${JSON.stringify(successResult.file)}`)
              }
              break
            case 'RESOLVE_NOT_PRESENT':
              // Do nothing for now...
              break
            case 'RESOLVE_SUCCESS_IGNORE_MODULE':
              // Do nothing for now...
              break
            default:
              const _exhaustiveCheck: never = resolveResult
              throw new Error(`Unhandled case ${JSON.stringify(resolveResult)}`)
          }
        }
      }
    }
  })
  return result
}

export function getTransitiveReverseDependencies(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  filenamesThatChanged: Array<string>,
): Array<string> {
  if (filenamesThatChanged.length > 0) {
    let result: Set<string> = emptySet()
    const directReverseDeps = getDirectReverseDependencies(projectContents, nodeModules)
    function exploreDeps(filename: string): void {
      if (!result.has(filename)) {
        result.add(filename)
        const reverseDeps = directReverseDeps[filename]
        if (reverseDeps != null) {
          reverseDeps.forEach(exploreDeps)
        }
      }
    }
    filenamesThatChanged.forEach(exploreDeps)
    return Array.from(result).sort()
  } else {
    return []
  }
}

export function identifyFilesThatHaveChanged(
  oldContents: ProjectContentTreeRoot,
  newContents: ProjectContentTreeRoot,
): Set<string> {
  let filesWithChanges: Set<string> = emptySet()

  function addFileToChanges(filename: string): void {
    filesWithChanges.add(filename)
  }

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
          // Be slightly careful around what has changed across the saved and unsaved values.
          const firstSavedContent = getSavedCodeFromTextFile(firstContents.content)
          const firstUnsavedContent = getUnsavedCodeFromTextFile(firstContents.content)
          const secondSavedContent = getSavedCodeFromTextFile(secondContents.content)
          const secondUnsavedContent = getUnsavedCodeFromTextFile(secondContents.content)

          const savedContentChanged = firstSavedContent !== secondSavedContent
          const unsavedContentChanged = firstUnsavedContent !== secondUnsavedContent
          const fileMarkedDirtyButNoCodeChangeYet =
            firstUnsavedContent == null && secondUnsavedContent === firstSavedContent

          // When a parsed model is updated but that change hasn't been reflected in the code yet, we end up with a file
          // that has no code change.
          const fileShouldBeWritten =
            savedContentChanged || (unsavedContentChanged && !fileMarkedDirtyButNoCodeChangeYet)

          if (fileShouldBeWritten) {
            addFileToChanges(fullPath)
          }
        } else {
          addFileToChanges(fullPath)
        }
      } else {
        addFileToChanges(fullPath)
      }
    } else {
      if (isProjectContentFile(secondContents)) {
        addFileToChanges(fullPath)
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
        addFileToChanges(fullPath)
        return true
      }
    } else {
      if (secondContents == null) {
        // Value does not exist, delete it.
        addFileToChanges(fullPath)
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

  zipContentsTree(oldContents, newContents, onElement)

  return filesWithChanges
}
