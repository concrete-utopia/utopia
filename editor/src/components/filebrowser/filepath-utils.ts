import Utils from '../../utils/utils'
import type { EditorDispatch } from '../editor/action-types'
import { addFolder, addTextFile } from '../editor/actions/action-creators'

// Important: getFilePathToImport uses file paths with a leading `/`!
export function getFilePathToImport(importFromPath: string, targetFilePath: string): string {
  if (!isAbsoluteFilePath(importFromPath)) {
    return importFromPath
  }
  const fromFilePathArray = importFromPath.split('/').slice(1)
  const targetFilePathArray = targetFilePath.split('/').slice(1)

  let matchingPathPart = 0
  let lastMatchedIndex = -1
  Utils.fastForEach(targetFilePathArray, (pathPart, index) => {
    if (pathPart === fromFilePathArray[index] && lastMatchedIndex + 1 === index) {
      matchingPathPart += 1
      lastMatchedIndex += 1
    }
  })

  const fromFilePathArrayWithoutMatching = fromFilePathArray.slice(matchingPathPart)
  const targetFilePathArrayWithoutMatching = targetFilePathArray.slice(matchingPathPart)
  if (
    targetFilePathArrayWithoutMatching.length > fromFilePathArrayWithoutMatching.length ||
    targetFilePathArray.length > 1
  ) {
    // the imported line probably needs '../../etc.'
    if (targetFilePathArrayWithoutMatching.length > 1) {
      let importPath = ''
      const lastIndex = targetFilePathArrayWithoutMatching.length - 1
      Utils.fastForEach(targetFilePathArrayWithoutMatching, (pathPart, index) => {
        if (index != lastIndex) {
          importPath += '../'
        } else {
          importPath += fromFilePathArrayWithoutMatching.join('/')
        }
      })
      return importPath
    } else {
      return `./${fromFilePathArrayWithoutMatching.join('/')}`
    }
  }

  // nothing special, use default full path
  return `.${importFromPath}`
}

function isAbsoluteFilePath(path: string) {
  return path.startsWith('/')
}

export function dropLeadingSlash(path: string): string {
  if (path.startsWith('/')) {
    return path.slice(1)
  } else {
    return path
  }
}

export interface AddingFile {
  fileOrFolder: 'file' | 'folder'
  filename: string
}

export function applyAddingFile(
  dispatch: EditorDispatch,
  parentPath: string,
  addingFile: AddingFile | null,
): void {
  if (addingFile != null) {
    switch (addingFile.fileOrFolder) {
      case 'file':
        dispatch([addTextFile(parentPath, addingFile.filename)], 'everyone')
        break
      case 'folder':
        dispatch([addFolder(parentPath, addingFile.filename)], 'everyone')
        break
      default:
        const _exhaustiveCheck: never = addingFile.fileOrFolder
        throw new Error(`Unhandled type ${JSON.stringify(addingFile)}`)
    }
  }
}
